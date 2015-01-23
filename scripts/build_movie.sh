#!/usr/bin/env bash

set -e

function usage {
  echo "Usage: $0 <scene name>"
}

if [ -z "$1" ]
then
  usage
  exit 1
fi

SNAME=$1

for f in $SNAME-*.bmp
do
  convert $f $f.jpg
done

ffmpeg -r 30 -i $SNAME-%d.bmp.jpg $SNAME.mp4

rm $SNAME-*.bmp.jpg
