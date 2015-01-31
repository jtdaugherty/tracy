#!/usr/bin/env bash

set -e

function usage {
	echo "Usage: $0 <name>"
}

NAME=$1

if [ -z "$NAME" ]
then
	usage
	exit 1
fi

git stash
cabal install -f bench
.cabal-sandbox/bin/tracy-bench --output=$NAME-before.html $NAME
git stash pop
cabal install -f bench
.cabal-sandbox/bin/tracy-bench --output=$NAME-after.html $NAME
open $NAME-*.html
