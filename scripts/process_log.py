#!/usr/bin/env python


import argparse
from datetime import datetime
import gzip
import json
import os
import re
import sys
import time
import tempfile
from operator import or_

LOGFILE = "/home/melpa/log/melpa.access.log"
LOGREGEX = r'(?P<ip>[\d.]+) [ -]+ \[(?P<date>[\w/: +-]+)\] ' \
           r'"GET /packages/(?P<package>[^ ]+)-[0-9.]+.(?:el|tar) ' \
           r'HTTP/\d.\d" 200'


def json_handler(obj):
    if isinstance(obj, datetime):
        return obj.isoformat()
    elif isinstance(obj, set):
        return list(obj)
    raise TypeError(
        'Object of type {0} with value {0} is not JSON serializable'.format(
        type(obj), repr(obj)))


def json_dump(data, jsonfile, indent=None):
    """
    jsonfiy `data`
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

def parse_logfile(logfilename, pkg_ip_time):
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
        dtstamp = int(time.mktime(
            datetime.strptime(match.group('date').split()[0],
                              "%d/%b/%Y:%H:%M:%S").timetuple()))
        pkg = match.group('package')

        pkg_ip_time.setdefault(pkg, {}).setdefault(ip, set()).add(dtstamp)

        count += 1

    return count


def main():
    """main function"""

    parser = argparse.ArgumentParser(description='MELPA Log File Parser')
    parser.add_argument('logs', metavar="logs", type=unicode, nargs="*",
                        help="Log files to parse.", default=[LOGFILE])
    args = parser.parse_args()

    pid = str(os.getpid())
    pidfile = os.path.join(os.path.join(tempfile.gettempdir(), "process_log.pid"))

    if os.access(pidfile, os.F_OK):
        running_pid = open(pidfile, "r").readline()

        try:
            os.kill(int(running_pid), 0)
            print "Process {0} currently running.".format(running_pid)
            return 1
        except OSError:
            print "Stale lockfile."
            os.unlink(pidfile)

    file(pidfile, 'w').write(pid)

    # load old data file
    if os.path.exists("download_log.json.gz"):
        pkg_ip_time = json_load(gzip.open("download_log.json.gz"))
    else:
        pkg_ip_time = {}

    # parse each parameter
    for logfile in args.logs:
        sys.stdout.write("processing logfile {0}... ".format(logfile))
        sys.stdout.flush()

        count = parse_logfile(logfile, pkg_ip_time)
        sys.stdout.write("{0}\n".format(count))

    # dump new data file
    json_dump(pkg_ip_time, gzip.open("download_log.json.gz", 'w'))

    # calculate current package totals
    pkgcount = {p: len(i) for p, i in pkg_ip_time.iteritems()}

    json_dump(pkgcount, open("html/download_counts.json", 'w'), indent=1)

    os.unlink(pidfile)
    return 0


if __name__ == '__main__':
    sys.exit(main())
