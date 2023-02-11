# Written by Ricardo Yanez <ricardo.yanez@calel.org>

F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

SRC=zbrent.f bessi0.f bessi1.f

OBJ=$(SRC:%.f=%.o)

nrf77:
ifeq (,$(wildcard $(SRC)))
	@echo 'Before proceeding, copy the source into this directory:' $(SRC) && exit 1
endif
ifeq (,$(wildcard zbrent.f.orig))
	cp -a zbrent.f zbrent.f.orig
endif
ifeq (,$(wildcard bessi0.f.orig))
	cp -a bessi0.f bessi0.f.orig
endif
ifeq (,$(wildcard bessi1.f.orig))
	cp -a bessi1.f bessi1.f.orig
endif
	patch -Nur /dev/null zbrent.f zbrent.f.patch || true
	patch -Nur /dev/null bessi0.f bessi0.f.patch || true
	patch -Nur /dev/null bessi1.f bessi1.f.patch || true
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	install -m 644 $(OBJ) ../

clean:
	-mv -f zbrent.f.orig zbrent.f
	-mv -f bessi0.f.orig bessi0.f
	-mv -f bessi1.f.orig bessi1.f
	-rm -f *.o *.so *~
