#!/usr/bin/env python


import argparse
from datetime import datetime, timedelta
from timeit import default_timer as timer
import gzip
import json
import re
import sys
import time
import sqlite3
from operator import or_

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
    return json.dump(data, jsonfile, default=json_handler, indent=indent, encoding='utf-8')


def datetime_parser(dct):
    for key, val in dct.items():
        if isinstance(val, list):
            dct[key] = set(val)
    return dct


def json_load(jsonfile):
    return json.load(jsonfile, object_hook=datetime_parser)


def parse_val(val):
    try:
        return datetime.strptime(val, "%Y-%m-%dT%H:%M:%S")
    except ValueError:
        return val


def ip_to_number(ip):
    return reduce(or_, ((int(n) << (i*8)) for i, n in enumerate(
        reversed(ip.split('.')))), 0)

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

def parse_logfile(logfilename, curs):
    """
    """
    if logfilename.endswith("gz"):
        logfile = gzip.open(logfilename, 'r')
    else:
        logfile = open(logfilename, 'r')

    logre = re.compile(LOGREGEX)
    count = 0

    for line in logfile:
        match = logre.match(line)

        if match is None:
            continue

        # Convert ips to four character strings.
        ip = match.group('ip')
        pkg = match.group('package')
        date = int((parse_log_datetime(match.group('date')) - EPOCH).total_seconds())
        version = match.group('version')
        agent = match.group('agent')

        curs.execute("INSERT OR IGNORE INTO packages VALUES (?)", (pkg,))
        package_id, = curs.execute("SELECT rowid FROM packages WHERE name = ?", (pkg,)).fetchone()
        curs.execute("INSERT OR IGNORE INTO versions VALUES (?)", (version,))
        version_id, = curs.execute("SELECT rowid FROM versions WHERE version = ?", (version,)).fetchone()
        curs.execute("INSERT OR IGNORE INTO agents VALUES (?)", (agent,))
        agent_id, = curs.execute("SELECT rowid FROM agents WHERE agent = ?;", (agent,)).fetchone()
        curs.execute("INSERT OR IGNORE INTO clients VALUES (?, ?)", (ip, agent_id))
        client_id, = curs.execute("SELECT rowid FROM clients WHERE ip = ? AND agent_id = ?", (ip, agent_id)).fetchone()
        curs.execute("INSERT OR IGNORE INTO downloads VALUES (?, ?, ?, ?)", (package_id, version_id, date, client_id))
        count += 1

    return count


def main():
    """main function"""

    parser = argparse.ArgumentParser(description='MELPA Log File Parser')
    parser.add_argument('--jsondir', help='JSON output directory (default: working directory)', default=".")
    parser.add_argument('--db', help='Database file (default: download_log.db)', default="download_log.db")
    parser.add_argument('logs', metavar="logs", type=unicode, nargs="+",
                        help="HTTP access log files to parse.")
    args = parser.parse_args()

    conn = sqlite3.connect(args.db)
    curs = conn.cursor()

    print("Ensuring database setup")
    # We normalise common strings into separate tables along semantic lines
    # so that the "downloads" table is a compact set of ints.
    #
    # As of end Jan 2020, the approximate cardinality of data is as follows:
    #
    # Packages: 4950
    # Distinct agents: 12.5k
    # Distinct version strings: 74k
    # Distinct IPs 1.75M
    # Distinct clients: 2.2M
    # Downloads: 174M
    curs.execute("CREATE TABLE IF NOT EXISTS versions (version TEXT, PRIMARY KEY (version))")
    curs.execute("CREATE TABLE IF NOT EXISTS packages (name TEXT, PRIMARY KEY (name))")
    curs.execute("CREATE TABLE IF NOT EXISTS agents (agent TEXT, PRIMARY KEY (agent))")
    curs.execute("CREATE TABLE IF NOT EXISTS clients (ip TEXT, agent_id INT, PRIMARY KEY (ip, agent_id))")
    curs.execute("CREATE TABLE IF NOT EXISTS downloads (pkg_id INT, version_id INT, date INT, client_id INT, PRIMARY KEY (pkg_id, version_id, date, client_id)) WITHOUT ROWID")
    curs.execute('''
      CREATE VIEW IF NOT EXISTS download_totals AS
        SELECT p.name AS package, counts.count
         FROM (SELECT pkg_id, COUNT(1) AS count FROM downloads GROUP BY pkg_id) counts
         JOIN packages p ON p.rowid = pkg_id
    ''')
    conn.commit()

    # parse each parameter
    for logfile in args.logs:
        print("Processing logfile {0}".format(logfile))
        start = timer()
        count = parse_logfile(logfile, curs)
        print("-> {0} records processed in {1}s".format(count, timer() - start))
        conn.commit()

    # calculate current package totals

    print("Querying totals")
    start = timer()
    pkgcount = {p: c for p, c in curs.execute(
        "SELECT package, count FROM download_totals")}
    print("-> Done in {}s".format(timer() - start))

    json_dump(pkgcount, open(args.jsondir + "/download_counts.json", 'w'), indent=1)

    return 0


if __name__ == '__main__':
    sys.exit(main())
