CC=gcc
ECL=/c/usr/local/ecl
CFLAGS=-I$(ECL)
LDFLAGS=-L$(ECL) -lecl

EXEFILE=test.exe

exe: CFLAGS := -DEXE $(CFLAGS)

all: dll

dll: main.o
	$(CC) -shared -o ../turtl.dll main.o -Wl,--output-def,turtl.def $(LDFLAGS)

lib: dll
	lib /machine:i386 /def:turtl.def

exe: main.o
	$(CC) main.o -o ../$(EXEFILE) $(LDFLAGS)

main.o:
	$(CC) ../src/main.c -c $(CFLAGS) -o main.o

clean:
	rm -f *.a *.o *.def *.dll *.lib *.exp

remove:
	rm -f ../turtl.dll ../$(EXEFILE) ../turtl.lib

