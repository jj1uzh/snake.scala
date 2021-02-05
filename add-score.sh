#!/bin/sh -e

test $# -eq 1
score=`printf '%5d' "$1"`
score_line="$score `date`"

score_file='score.txt'
touch $score_file
all_scores="$score_line
`cat $score_file`"

printf "$all_scores" | sort -k 1nr > $score_file
