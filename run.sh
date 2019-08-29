#!/bin/sh

#You can use Makefile here. Or, you can modify the program as you want.
[ -e input -a input -nt term.c ] || cc term.c -o input

if [ x"$1" = x -o x"$1" = x'Racket' ]; then
	#Racket
	[ -e weiqi -a weiqi -nt weiqi.rkt ] || raco exe -o weiqi weiqi.rkt
	./input | ./weiqi
elif [ x"$1" = x'Chez' ]; then
	#Chez Scheme
	[ -e weiqi.ss -a weiqi.ss -ot weiqi.rkt ] || sed '1d' weiqi.rkt >weiqi.ss
	./input | scheme -q --script weiqi.ss
else
	echo $0 '[Racket|Chez]'
fi
