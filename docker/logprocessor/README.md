# Web log processing

Here are scripts that are used to generate download statistics from
our current and historical web server logs. As of Aug 2023, the main
MELPA repo has 11GB of historical logfiles, mostly gzipped, so perhaps
40+GB or more of text.

There's always a current logfile, which grows during the day, and this
is rotated to a timestamped copy of itself (currently daily), so that
the next day starts with a fresh empty log.

This note explains how we ended up with the specific code in this
directory.

## History

### Sqlite

For a while we incrementally parsed and deduplicated these logs via a
single Python script into a 7.6GB sqlite database, which was carefully
normalised and optimised.

The database stored package names, versions, download timestamp,
client IP and user agent.

Deduplication was necessary because the database was a mutable store,
and we would repeatedly re-scan the current logfile throughout the
day.

### Parquet

In Aug 2023 Steve decided to tinker, and so there appeared a new scheme in which
each server log file is instead summarised into a corresponding
[parquet](https://parquet.apache.org/) file, each containing an
unnormalised list of individual downloads with the above fields. This
resulted in 3500+ parquet files totalling only 1.9GB, since the data
is efficiently stored in a compressed columnar format.

Using [duckdb](https://duckdb.org), a tree of parquet files can be trivially
queried en-masse to produce the download totals, which took around 12 seconds
on an M2 Macbook Air, but longer on the server.

In this scheme, the single parquet file for the current log is
regenerated from scratch regularly, so no deduplication is
necessary. All the contributing files are regenerated on demand.

### Slimming down

Ultimately, the only thing the data is used for is deriving
per-package download totals, which need to be exported into the
`html/download_counts.json` file for use by the web frontend.

So after doing the fancy stuff above, we switched to just recording
the totals in each parquet file, rather than the full list of
downloads. This uses vastly less storage (56MB) and fits the
bill perfectly well.

Parquet + duckdb is still convenient for totalling up all these
numbers, at the expense of needing to install `duckdb` using `pip`, but
obviously at this stage we could simply write JSON files and total
them up ourselves with Python. That's really what we should have
done in the first place.
