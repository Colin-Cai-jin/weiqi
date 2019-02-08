#!/bin/sh

#You can use Makefile here. Or, you can modify the program as you want.
[ -e ./input ] || cc ./term.c -o ./input
[ -e ./weiqi ] || raco exe -o ./weiqi weiqi.rkt

#Run
./input | ./weiqi
