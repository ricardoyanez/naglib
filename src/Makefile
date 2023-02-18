# Written by Ricardo Yanez <ricardo.yanez@calel.org>

GCC=gcc
CFLAGS=-g -Wall

SRC=c_c05adf.c

OBJ=$(SRC:%.c=%.o)

cnag: cnag.h $(SRC)
	$(GCC) $(CFLAGS) -fPIC -c $(SRC)
	install -m 644 $(OBJ) ../

clean:
	-rm -f *.o *~
