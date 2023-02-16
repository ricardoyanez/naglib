#
# Written by Ricardo Yanez <ricardo.yanez@calel.org>
# Last change: Feb 16, 2023
#

F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

SRC=zbrent.f

OBJ=$(SRC:%.f=%.o)

nrf77:
ifeq (,$(wildcard $(SRC)))
	@echo 'Before proceeding, copy the source into this directory:' $(SRC) && exit 1
endif
ifeq (,$(wildcard zbrent.f.orig))
	cp -a zbrent.f zbrent.f.orig
endif
	patch -Nur /dev/null zbrent.f zbrent.f.patch || true
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	install -m 644 $(OBJ) ../

clean:
	-mv -f zbrent.f.orig zbrent.f
	-rm -f *.o *.so *~
