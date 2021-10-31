#!/bin/bash

path=$1
find $path -type f -name \*.c -o -name \*.h | xargs grep ".\+" | wc -l

