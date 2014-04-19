#!/bin/bash

export PATH=/c/dev/MinGW/bin:$PATH
ECL=/c/usr/local/ecl
CC=gcc
ECL_CONFIG=$ECL/ecl-config
CFLAGS="-I${ECL} -L${ECL} `$ECL_CONFIG --cflags`"
LDFLAGS="`$ECL_CONFIG --ldflags`"

$CC \
	-DEXE \
	$CFLAGS \
	-o test main.c \
	$LDFLAGS

chmod 755 test.exe

