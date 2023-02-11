# Written by Ricardo Yanez <ricardo.yanez@calel.org>

# AMOS routines and dependencies

F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

SRC=zbesi.f zabs.f zbinu.f zseri.f zbuni.f zasyi.f zwrsk.f zmlri.f zuoik.f zexp.f \
    zuni2.f zmlt.f zuni1.f zuchk.f zlog.f dgamln.f zbknu.f zdiv.f zrati.f zunik.f \
    zsqrt.f zunhj.f zkscl.f zairy.f zshch.f zacai.f zs1s2.f
OBJ=$(SRC:%.f=%.o)

amos:
ifeq (,$(wildcard zbesi.f))
	wget https://netlib.org/amos/zbesi.f
endif
ifeq (,$(wildcard dgamln.f))
	wget https://netlib.org/amos/dgamln.f
endif
ifeq (,$(wildcard zabs.f))
	wget https://netlib.org/slatec/src/zabs.f
endif
ifeq (,$(wildcard zbinu.f))
	wget https://netlib.org/slatec/src/zbinu.f
endif
ifeq (,$(wildcard zseri.f))
	wget https://netlib.org/slatec/src/zseri.f
endif
ifeq (,$(wildcard zbuni.f))
	wget https://netlib.org/slatec/src/zbuni.f
endif
ifeq (,$(wildcard zasyi.f))
	wget https://netlib.org/slatec/src/zasyi.f
endif
ifeq (,$(wildcard zwrsk.f))
	wget https://netlib.org/slatec/src/zwrsk.f
endif
ifeq (,$(wildcard zmlri.f))
	wget https://netlib.org/slatec/src/zmlri.f
endif
ifeq (,$(wildcard zuoik.f))
	wget https://netlib.org/slatec/src/zuoik.f
endif
ifeq (,$(wildcard zexp.f))
	wget https://netlib.org/slatec/src/zexp.f
endif
ifeq (,$(wildcard zuni2.f))
	wget https://netlib.org/slatec/src/zuni2.f
endif
ifeq (,$(wildcard zmlt.f))
	wget https://netlib.org/slatec/src/zmlt.f
endif
ifeq (,$(wildcard zuni1.f))
	wget https://netlib.org/slatec/src/zuni1.f
endif
ifeq (,$(wildcard zuchk.f))
	wget https://netlib.org/slatec/src/zuchk.f
endif
ifeq (,$(wildcard zlog.f))
	wget https://netlib.org/slatec/src/zlog.f
endif
ifeq (,$(wildcard zbknu.f))
	wget https://netlib.org/slatec/src/zbknu.f
endif
ifeq (,$(wildcard zdiv.f))
	wget https://netlib.org/slatec/src/zdiv.f
endif
ifeq (,$(wildcard zrati.f))
	wget https://netlib.org/slatec/src/zrati.f
endif
ifeq (,$(wildcard zunik.f))
	wget https://netlib.org/slatec/src/zunik.f
endif
ifeq (,$(wildcard zsqrt.f))
	wget https://netlib.org/slatec/src/zsqrt.f
endif
ifeq (,$(wildcard zunhj.f))
	wget https://netlib.org/slatec/src/zunhj.f
endif
ifeq (,$(wildcard zkscl.f))
	wget https://netlib.org/slatec/src/zkscl.f
endif
ifeq (,$(wildcard zairy.f))
	wget https://netlib.org/slatec/src/zairy.f
endif
ifeq (,$(wildcard zshch.f))
	wget https://netlib.org/slatec/src/zshch.f
endif
ifeq (,$(wildcard zacai.f))
	wget https://netlib.org/slatec/src/zacai.f
endif
ifeq (,$(wildcard zs1s2.f))
	wget https://netlib.org/slatec/src/zs1s2.f
endif
	cp -af zbesi.f zbesi.f.orig
	patch -Nur /dev/null zbesi.f zbesi.f.patch || true
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	install -m 644 $(OBJ) ../

clean:
	-mv -f zbesi.f.orig zbesi.f
	-rm -f *.o *.so *~
