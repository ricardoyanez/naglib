# Written by Ricardo Yanez <ricardo.yanez@calel.org>

GCC=gcc
CFLAGS=-g -Wall -std=gnu99

SRC=c_c05adf.c c_d01amf.c c_d01asf.c c_g05ddf.c c_g05ecf.c c_g05eyf.c \
    c_s14aaf.c c_s14abf.c c_s15adf.c c_s18aef.c c_s18aff.c c_x01aaf.c \
    c_x05baf.c c_g05ccf.c c_d1mach.c

OBJ=$(SRC:%.c=%.o)

cnag: cnag_glib.h cnag_gsl.h cnag_gsl_rng.h $(SRC)
	$(GCC) $(CFLAGS) -fPIC -c $(SRC)
	install -m 644 $(OBJ) ../

clean:
	-rm -f *.o *~
