# Written by Ricardo Yanez <ricardo.yanez@calel.org>

# SLATEC routines

F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

SRC=zairy.f zbinu.f zdiv.f zlog.f zrati.f zshch.f zunhj.f zunik.f zabs.f \
    zasyi.f zbknu.f zexp.f zmlri.f zs1s2.f zsqrt.f zuni1.f zuoik.f zacai.f \
    zbuni.f zkscl.f zmlt.f zseri.f zuchk.f zuni2.f zwrsk.f \
    xermsg.f xerprn.f xerhlt.f xersve.f xercnt.f \
    xgetua.f j4save.f fdump.f i1mach.f
OBJ=$(SRC:%.f=%.o)

slatec:
ifeq (,$(wildcard zabs.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zabs.f
endif
ifeq (,$(wildcard zbinu.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zbinu.f
endif
ifeq (,$(wildcard zseri.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zseri.f
endif
ifeq (,$(wildcard zbuni.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zbuni.f
endif
ifeq (,$(wildcard zasyi.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zasyi.f
endif
ifeq (,$(wildcard zwrsk.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zwrsk.f
endif
ifeq (,$(wildcard zmlri.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zmlri.f
endif
ifeq (,$(wildcard zuoik.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zuoik.f
endif
ifeq (,$(wildcard zexp.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zexp.f
endif
ifeq (,$(wildcard zuni2.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zuni2.f
endif
ifeq (,$(wildcard zmlt.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zmlt.f
endif
ifeq (,$(wildcard zuni1.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zuni1.f
endif
ifeq (,$(wildcard zuchk.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zuchk.f
endif
ifeq (,$(wildcard zlog.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zlog.f
endif
ifeq (,$(wildcard zbknu.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zbknu.f
endif
ifeq (,$(wildcard zdiv.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zdiv.f
endif
ifeq (,$(wildcard zrati.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zrati.f
endif
ifeq (,$(wildcard zunik.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zunik.f
endif
ifeq (,$(wildcard zsqrt.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zsqrt.f
endif
ifeq (,$(wildcard zunhj.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zunhj.f
endif
ifeq (,$(wildcard zkscl.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zkscl.f
endif
ifeq (,$(wildcard zairy.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zairy.f
endif
ifeq (,$(wildcard zshch.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zshch.f
endif
ifeq (,$(wildcard zacai.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zacai.f
endif
ifeq (,$(wildcard zs1s2.f))
	wget --no-check-certificate https://netlib.org/slatec/src/zs1s2.f
endif
ifeq (,$(wildcard xermsg.f))
	wget --no-check-certificate https://netlib.org/slatec/src/xermsg.f
endif
ifeq (,$(wildcard xerprn.f))
	wget --no-check-certificate https://netlib.org/slatec/src/xerprn.f
endif
ifeq (,$(wildcard xerhlt.f))
	wget --no-check-certificate https://netlib.org/slatec/src/xerhlt.f
endif
ifeq (,$(wildcard xersve.f))
	wget --no-check-certificate https://netlib.org/slatec/src/xersve.f
endif
ifeq (,$(wildcard xercnt.f))
	wget --no-check-certificate https://netlib.org/slatec/src/xercnt.f
endif
ifeq (,$(wildcard xgetua.f))
	wget --no-check-certificate https://netlib.org/slatec/src/xgetua.f
endif
ifeq (,$(wildcard j4save.f))
	wget --no-check-certificate https://netlib.org/slatec/src/j4save.f
endif
ifeq (,$(wildcard fdump.f))
	wget --no-check-certificate https://netlib.org/slatec/src/fdump.f
endif
ifeq (,$(wildcard i1mach.f))
	wget --no-check-certificate https://netlib.org/port/i1mach.f
endif
	$(F77) $(FFLAGS) -fPIC -c $(SRC)
	install -m 644 $(OBJ) ../

clean:
	-rm -f *.o *.so *~
