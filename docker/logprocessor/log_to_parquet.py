#!/usr/bin/env python3

# Standard libraries
import re
import sys
import tempfile

from parse import parse_logfile, TIME_FORMAT

# Installed packages
import duckdb

def main():
    count = 0
    stats = {}
    for row in parse_logfile(sys.stdin):
        stats[row['package']] = stats.get(row['package'], 0) + 1
        count += 1
    duckdb.execute('CREATE TEMP TABLE stats (package TEXT NOT NULL, count UINTEGER NOT NULL)')
    duckdb.executemany('INSERT INTO stats VALUES (?, ?)', stats.items())
    duckdb.execute("COPY stats TO '/dev/stdout' (FORMAT PARQUET, COMPRESSION 'zstd')")
    print(f"Processed {count} downloads", file=sys.stderr)
    return count


if __name__ == '__main__':
    main()
