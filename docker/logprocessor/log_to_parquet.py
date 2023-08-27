#!/usr/bin/env python

# Standard libraries
from datetime import datetime, timedelta
from timeit import default_timer as timer
import gzip
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

# In Python 2.7, strptime doesn't understand "%z", so we have to work around it
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
        # Convert ips to four character strings.
        ip = ip2int(match.group('ip'))
        pkg = match.group('package')
        date = match.group('date')
        version = match.group('version')
        agent = match.group('agent')
        duckdb.execute("INSERT INTO downloads VALUES (?, ?, strptime(?, '%d/%b/%Y:%H:%M:%S %z'), ?, ?)", (pkg, version, date, ip, agent))
        count += 1

    # force_not_null doesn't seem to be available in this duckdb version, contrary to the docs, so we work around with nullstr
    # to make sure empty strings aren't interpreted as nulls
    duckdb.execute("COPY downloads TO '/dev/stdout' (FORMAT PARQUET, COMPRESSION 'zstd')")
    print(f"Processed {count} downloads", file=sys.stderr)
    return count


def main():
    """main function"""
    parse_logfile()
    return 0


if __name__ == '__main__':
    sys.exit(main())
