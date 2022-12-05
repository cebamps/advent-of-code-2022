#!/bin/bash

set -e

day=$(sed -rn '/^# d([0-9]{2}):.*/{ s//\1/p; q }' package.yaml)

if [[ -z "$day" ]]; then
  echo "uh oh, found this value for the day: $day"
  exit 1
fi

mkdir src/D"$day"
touch inputs/d"$day"{,-test}.txt
sed "s/00/$day/g" template/d00.hs > exe/d"$day".hs
sed "s/00/$day/g" template/Solution.hs > src/D"$day"/Solution.hs

sed -ri '0,/^# d([0-9]{2}):/{s//  d\1:/}' package.yaml
