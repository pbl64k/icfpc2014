#!/bin/sh

if [ -e solution/lambdaman.gcc ]; then
    git archive --format=tar HEAD | gzip >icfpc-2014-submision.tar.gz
    exit 0
fi;

echo 'You have not picked a solution, you idiot.'
exit 1

