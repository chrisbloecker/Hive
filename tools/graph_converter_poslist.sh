#!/bin/bash

cat $1 | sed 's/^/[/' | sed 's/$/]/' | sed 's/[ ]/,/' | tr '\n' ',' | sed 's/^/[/' | sed 's/,$/]/'
