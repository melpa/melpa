#!/bin/sh
jq 'to_entries | map({ name: .key, downloads: .value|tonumber }) | sort_by(.downloads)' |
	mlr --json --infer-none --jlistwrap put -q '
@records[NR] = $*;
end {
  for (k, v in @records) {
    @records[k]["percentile"] = 100 * int(k)/NR;
  }
  emit @records;
}'
