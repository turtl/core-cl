CC=gcc
ECL=/c/usr/local/ecl
CFLAGS=-I$(ECL)
LDFLAGS=-L$(ECL) -lecl

EXEFILE=ecl-test.exe

exe: CFLAGS := -DEXE $(CFLAGS)

all: dll

dll: main.o

	$(CC) -shared -o ../turtl.dll main.o -Wl,--output-def,turtl.def $(LDFLAGS)
	strip ../turtl.dll

lib: dll

	lib /machine:i386 /def:turtl.def
	mv turtl.lib ..

exe: main.o

	$(CC) main.o -o ../test/$(EXEFILE) $(LDFLAGS)

main.o:

	$(CC) ../src/main.c -c $(CFLAGS) -o main.o

clean:

	rm -f *.a *.o *.def *.dll *.lib *.exp

remove:

	rm -f ../turtl.dll ../test/$(EXEFILE) ../turtl.lib

