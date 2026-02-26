#!/usr/bin/env python3

# Standard libraries
import re
import sys
import socket
import struct
from time import strptime

LOGREGEX = r'^(?P<ip>\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}) [ -]+ \[(?P<date>[\w/: +-]+)\] ' \
           r'"GET /+packages/+(?P<package>[^ ]+)-(?P<version>[0-9.]+).(?:el|tar) ' \
           r'HTTP/\d.\d" 200 \d+ "[^"]*?" "(?P<agent>[^"]*)'

TIME_FORMAT = '%d/%b/%Y:%H:%M:%S %z'

def ip2int(addr):
    return struct.unpack("!I", socket.inet_aton(addr))[0]

def parse_logfile(stream):
    logre = re.compile(LOGREGEX)
    for line in stream:
        match = logre.match(line)
        if match is None:
            continue
        ip = ip2int(match.group('ip'))
        package = match.group('package')
        date = match.group('date')
        version = match.group('version')
        agent = match.group('agent')
        yield({'package': package, 'version': version, 'date': date, 'ip': ip, 'agent': agent})
