#!/bin/bash

export PATH=/d/MinGW/bin:$PATH
CC=gcc
ECL_CONFIG=/d/usr/local/ecl/ecl-config
CFLAGS="`$ECL_CONFIG --cflags`"
LDFLAGS="`$ECL_CONFIG --ldflags`"

$CC \
	-DEXE \
	$CFLAGS \
	-o test main.c \
	$LDFLAGS

chmod 755 test.exe

