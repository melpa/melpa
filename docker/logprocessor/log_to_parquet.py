#!/usr/bin/env python3

# Standard libraries
import re
import sys
import socket
import struct
import tempfile

# Installed packages
import duckdb

LOGREGEX = r'^(?P<ip>[\d.]+) [ -]+ \[(?P<date>[\w/: +-]+)\] ' \
           r'"GET /+packages/+(?P<package>[^ ]+)-(?P<version>[0-9.]+).(?:el|tar) ' \
           r'HTTP/\d.\d" 200 \d+ "[^"]*?" "(?P<agent>[^"]*)'

def ip2int(addr):
    return struct.unpack("!I", socket.inet_aton(addr))[0]

def parse_logfile():
    duckdb.execute('''CREATE TABLE IF NOT EXISTS downloads
                      ( package TEXT NOT NULL
                      , version TEXT NOT NULL
                      , date TIMESTAMPTZ NOT NULL
                      , ip UINTEGER NOT NULL
                      , agent TEXT NOT NULL)''')
    logre = re.compile(LOGREGEX)
    count = 0
    for line in sys.stdin:
        match = logre.match(line)
        if match is None:
            continue
        ip = ip2int(match.group('ip'))
        pkg = match.group('package')
        date = match.group('date')
        version = match.group('version')
        agent = match.group('agent')
        duckdb.execute("INSERT INTO downloads VALUES (?, ?, strptime(?, '%d/%b/%Y:%H:%M:%S %z'), ?, ?)", (pkg, version, date, ip, agent))
        count += 1

    duckdb.execute("COPY downloads TO '/dev/stdout' (FORMAT PARQUET, COMPRESSION 'zstd')")
    print(f"Processed {count} downloads", file=sys.stderr)
    return count


if __name__ == '__main__':
    parse_logfile()
