#!/bin/sh

provide_func()
{
	sed -nr 's/;.*//;s/\(|\)|[ \t]+/\n&\n/g;p' $1 | awk 'BEGIN{L=0}$0=="("{++L;if(L==1)df=0;next}$0==")"{L--;next}$0=="define"{df++;next}NF==1&&df==1{r=r" "$0;df++;}END{print r}'
}

#You can use Makefile here. Or, you can modify the program as you want.
[ -e input -a input -nt term.c ] || cc term.c -o input

#Or, you can compile the program.
if [ x"$1" = x -o x"$1" = x'Racket' ]; then
	#Racket
	if [ \! \( -e weiqi.rkt -a weiqi.rkt -nt weiqi.scm -a -e func.rkt -a func.rkt -nt func.scm \) ]; then
		{
			echo '#lang scheme'
			echo '(require "func.rkt")'
			cat weiqi.scm
		} >weiqi.rkt
		{
			echo '#lang scheme'
			echo '(provide ' $(provide_func func.scm) ')'
			cat func.scm
		} >func.rkt
	fi
	./input | racket weiqi.rkt
elif [ x"$1" = x'Chez' ]; then
	#Chez Scheme
	if [ \! \( -e weiqi.ss -a weiqi.ss -nt weiqi.scm -a -e func.ss -a func.ss -nt func.scm \) ]; then
		{
			echo '(import (scheme) (func))'
			cat weiqi.scm
		} >weiqi.ss
		{
			echo '(library (func)'
			echo '(export ' $(provide_func func.scm) ')'
			echo '(import (scheme))'
			cat func.scm
			echo ')'
		} >func.ss
	fi
	./input | scheme --optimize-level 3 -q --script weiqi.ss
else
	echo $0 '[Racket|Chez]'
fi
