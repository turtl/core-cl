CC=gcc
ECL=/usr/local/ecl
CFLAGS=-fPIC -I$(ECL) $$($(ECL)/bin/ecl-config --cflags)
LDFLAGS=-L$(ECL) $$($(ECL)/bin/ecl-config --ldflags)

EXEFILE=ecl-test.exe
ifeq ($(OS),Windows_NT)
	WINFLAGS=-Wl,--output-def,turtl.def
	LIB=turtl.dll
else
	WINFLAGS=
	LIB=turtl.so
endif

exe: CFLAGS := -DEXE $(CFLAGS)

all: shared

shared: main.o

	$(CC) -shared -o $(LIB) main.o $(WINFLAGS) $(LDFLAGS)
	#strip $(LIB)

lib: dll

	lib /machine:i386 /def:turtl.def

exe: main.o

	$(CC) main.o -o ../test/$(EXEFILE) $(LDFLAGS)

main.o:

	$(CC) ../src/main.c -c $(CFLAGS) -o main.o

clean:

	rm -f *.a *.o *.def *.so *.dll *.lib *.exp

remove:

	rm -f ../test/$(EXEFILE)

