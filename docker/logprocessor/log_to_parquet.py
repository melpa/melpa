#!/usr/bin/env python3

# Standard libraries
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

def ip2int(addr):
    return struct.unpack("!I", socket.inet_aton(addr))[0]

def parse_logfile():
    duckdb.execute('''CREATE TEMP TABLE IF NOT EXISTS downloads
                      ( package TEXT NOT NULL
                      , version TEXT NOT NULL
                      , date TIMESTAMPTZ NOT NULL
                      , ip UINTEGER NOT NULL
                      , agent TEXT NOT NULL)''')
    logre = re.compile(LOGREGEX)
    count = 0
    with tempfile.NamedTemporaryFile('w', suffix='.csv') as csvfile:
        rows = csv.writer(csvfile, quoting=csv.QUOTE_MINIMAL)
        rows.writerow(["package", "version", "date", "ip", "agent"])
        for line in sys.stdin:
            match = logre.match(line)
            if match is None:
                continue
            ip = ip2int(match.group('ip'))
            package = match.group('package')
            date = match.group('date')
            version = match.group('version')
            agent = match.group('agent')
            rows.writerow([package, version, date, ip, agent])
            count += 1

        csvfile.flush()
        duckdb.execute(f"INSERT INTO downloads SELECT package, version, strptime(date, '%d/%b/%Y:%H:%M:%S %z'), ip, agent from read_csv_auto('{csvfile.name}', nullstr='&&&&&')")

    duckdb.execute("COPY downloads TO '/dev/stdout' (FORMAT PARQUET, COMPRESSION 'zstd')")
    print(f"Processed {count} downloads", file=sys.stderr)
    return count


if __name__ == '__main__':
    parse_logfile()
