# Written by Ricardo Yanez <ricardo.yanez@calel.org>

# RKSUITE

F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

SRC=rksuite.f

OBJ=$(SRC:%.f=%.o)

rksuite:
ifeq (,$(wildcard rksuite.f))
	wget --no-check-certificate https://netlib.sandia.gov/ode/rksuite/rksuite.f
endif
	cp -af rksuite.f rksuite.f.orig
	patch -Nur /dev/null rksuite.f rksuite.f.patch || true
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	install -m 644 $(OBJ) ../

clean:
	-mv -f rksuite.f.orig rksuite.f
	-rm -f *.o *.so *~
