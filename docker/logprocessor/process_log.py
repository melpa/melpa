#!/usr/bin/env python

# Standard libraries
import argparse
from datetime import datetime, timedelta
from timeit import default_timer as timer
import gzip
import json
import re
import sys
import socket
import struct
import tempfile
import csv

# Installed packages
import duckdb

LOGREGEX = r'^(?P<ip>[\d.]+) [ -]+ \[(?P<date>[\w/: +-]+)\] ' \
           r'"GET /+packages/+(?P<package>[^ ]+)-(?P<version>[0-9.]+).(?:el|tar) ' \
           r'HTTP/\d.\d" 200 \d+ "[^"]*?" "(?P<agent>[^"]*)'


def json_handler(obj):
    if isinstance(obj, datetime):
        return obj.isoformat()
    elif isinstance(obj, set):
        return list(obj)
    raise TypeError(
        'Object of type {0} with value {1} is not JSON serializable'.format(
            type(obj), repr(obj)))


def json_dump(data, jsonfile, indent=None):
    """
    jsonify `data`
    """
    return json.dump(data, jsonfile, default=json_handler, indent=indent)

def ip2int(addr):
    return struct.unpack("!I", socket.inet_aton(addr))[0]

# In Python 2.7, strptime doesn't understand "%z", so we have to work around it
OFFSET_RE = re.compile(" ([-+])([01]\d)([0-5]\d)$")
def parse_log_datetime(s):
    "Parse string t and return a datetime object with offset timezone."
    m = OFFSET_RE.search(s)
    if not m:
        raise Exception("Bad date: {}".format(s))
    # Eg. "16/Apr/2013:07:45:09 -0500"
    dt = datetime.strptime(s[:-6],'%d/%b/%Y:%H:%M:%S')
    delta = timedelta(hours=int(m.group(2)),minutes=int(m.group(3)))
    if m.group(1) == "+":
        return dt - delta
    else:
        return dt + delta

EPOCH = datetime(1970,1,1)

def parse_logfile(logfilename, conn):
    """
    """
    if logfilename.endswith("gz"):
        logfile = gzip.open(logfilename, 'rt', encoding='ascii')
    else:
        logfile = open(logfilename, 'r')

    logre = re.compile(LOGREGEX)
    count = 0

    with tempfile.NamedTemporaryFile('wt') as temp_csv:
        writer = csv.writer(temp_csv, quoting=csv.QUOTE_MINIMAL)
        writer.writerow(["package", "version", "date", "ip", "agent"])
        for line in logfile:
            match = logre.match(line)

            if match is None:
                continue

            # Convert ips to four character strings.
            ip = ip2int(match.group('ip'))
            pkg = match.group('package')
            date = parse_log_datetime(match.group('date'))
            version = match.group('version')
            agent = match.group('agent')

            writer.writerow([pkg, version, date.isoformat(), ip, agent])

            count += 1
        temp_csv.flush()
        conn.execute("INSERT OR IGNORE INTO downloads SELECT DISTINCT * FROM read_csv_auto('{}', header=true, force_not_null=true)".format(temp_csv.name))

    return count


def main():
    """main function"""

    parser = argparse.ArgumentParser(description='MELPA Log File Parser')
    parser.add_argument('--jsondir', help='JSON output directory (default: working directory)', default=".")
    parser.add_argument('--db', help='Database file (default: download_log.db)', default="download_log.db")
    parser.add_argument('logs', metavar="logs", type=str, nargs="+",
                        help="HTTP access log files to parse.")
    args = parser.parse_args()

    conn = duckdb.connect(args.db)

    print("Ensuring database setup")
    # We normalise common strings into separate tables along semantic lines
    # so that the "downloads" table is a compact set of ints.
    #
    # The approximate cardinality of data is as follows:
    #
    #                           Jan 2020     Aug 2023
    # Packages:                 4950         6302
    # Distinct agents:          12.5k        21k
    # Distinct version strings: 74k          117k
    # Distinct IPs              1.75M        3.5M
    # Distinct (IP, Agent):      2.2M        4.7M
    # Downloads:                174M         337M
    conn.execute('''CREATE TABLE IF NOT EXISTS downloads
                      ( package TEXT NOT NULL
                      , version TEXT NOT NULL
                      , date TIMESTAMPTZ NOT NULL
                      , ip UINTEGER NOT NULL
                      , agent TEXT NOT NULL
                      , PRIMARY KEY (package, version, date, ip, agent))''')
    conn.commit()

    # parse each parameter
    for logfile in args.logs:
        print(("Processing logfile {0}".format(logfile)))
        start = timer()
        count = parse_logfile(logfile, conn)
        print(("-> {0} records processed in {1}s".format(count, timer() - start)))
        conn.commit()

    # calculate current package totals

    print("Querying totals")
    start = timer()
    pkgcount = {p: c for p, c in conn.execute(
        "SELECT package, count(1) AS count FROM downloads GROUP by package").fetchall()}
    print(("-> Done in {}s".format(timer() - start)))

    json_dump(pkgcount, open(args.jsondir + "/download_counts.json", 'w'), indent=1)

    return 0


if __name__ == '__main__':
    sys.exit(main())
