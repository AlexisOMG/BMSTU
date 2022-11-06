#!/bin/sh
flex -o main.yy.cpp main.lex
g++ -g -w -std=c++11 -o main main.yy.cpp
./main < input1.txt

