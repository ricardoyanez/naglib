F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic


SRC=zbesi.f dgamln.f \

OBJ=$(SRC:%.f=%.o)

quadpack:
ifeq (,$(wildcard zbesi.f))
	wget --no-check-certificate https://netlib.org/amos/zbesi.f
	cp -af zbesi.f zbesi.f.orig
endif
ifeq (,$(wildcard dgamln.f))
	wget --no-check-certificate https://netlib.org/amos/dgamln.f
endif
	cp -af zbesi.f zbesi.f.orig
	patch -Nur /dev/null zbesi.f zbesi.f.patch || true
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	install -m 644 $(OBJ) ../

clean:
	-mv -f zbesi.f.orig zbesi.f
	-rm -f *.o *.so *~
