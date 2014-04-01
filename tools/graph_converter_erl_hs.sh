#!/bin/bash

TMP=`cat $1 | sed 's/\r//g' | tr '\n' ' ' | sed 's/ //g' | sed 's/{/[/g' | sed 's/}/]/g' | sed 's/,inf//g' | sed 's/[.]/,/g' | sed 's/,$//'`

echo "["$TMP"]"
