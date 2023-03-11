F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

SRC=dpchim.f dpchst.f dpchfe.f dchfev.f dpchfd.f dchfdv.f \

OBJ=$(SRC:%.f=%.o)

quadpack:
ifeq (,$(wildcard dpchim.f))
	wget --no-check-certificate https://netlib.org/slatec/pchip/dpchim.f
endif
ifeq (,$(wildcard dpchst.f))
	wget --no-check-certificate https://netlib.org/slatec/pchip/dpchst.f
endif
ifeq (,$(wildcard dpchfe.f))
	wget --no-check-certificate https://netlib.org/slatec/pchip/dpchfe.f
endif
ifeq (,$(wildcard dchfev.f))
	wget --no-check-certificate https://netlib.org/slatec/pchip/dchfev.f
endif
ifeq (,$(wildcard dpchfd.f))
	wget --no-check-certificate https://netlib.org/slatec/pchip/dpchfd.f
endif
ifeq (,$(wildcard dchfdv.f))
	wget --no-check-certificate https://netlib.org/slatec/pchip/dchfdv.f
endif
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	install -m 644 $(OBJ) ../

clean:
	-rm -f *.o *.so *~
