# Time-stamp: <2007-10-17 17:52:29 jcgs>

all:	joylisp xjoylisp

joylisp: joylisp.c
	cc -g -Wall -o joylisp -lm joylisp.c
	cp joylisp ~/bin

joylisp_graphics.o: joylisp_graphics.c joylisp.h gamepad_diagram.h
	cc -g -DDIAGRAM -Wall -c -o joylisp_graphics.o joylisp_graphics.c

xjoylisp: joylisp.c joylisp_graphics.o joylisp.h
	cc -g -DDIAGRAM -Wall -o xjoylisp -lm -lX11 -lXt joylisp.c joylisp_graphics.o
	cp xjoylisp ~/bin
