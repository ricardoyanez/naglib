# Written by Ricardo Yanez <ricardo.yanez@calel.org>

F77=gfortran
FFLAGS=-g -std=legacy -fno-automatic

CC=gcc
CFLAGS=-g

OBJ=dpchim.o zasyi.o zlog.o zunhj.o \
    dpchst.o rksuite.o zbesi.o zmlri.o zuni1.o \
    zbinu.o zmlt.o zuni2.o \
    dchfdv.o zbknu.o zrati.o zunik.o \
    dchfev.o xercnt.o xersve.o zs1s2.o zuoik.o \
    dgamln.o fnag.o zbuni.o zseri.o zwrsk.o \
    fourpt.o xerhlt.o zabs.o zdiv.o zshch.o \
    dpchfd.o xermsg.o zacai.o zexp.o zsqrt.o \
    dpchfe.o xerprn.o zairy.o zkscl.o zuchk.o \
    xgetua.o j4save.o fdump.o i1mach.o \
    c_c05adf.o c_d01amf.o c_d01asf.o c_g05ddf.o c_g05ecf.o c_g05eyf.o \
    c_s14aaf.o c_s14abf.o c_s15adf.o c_s18aef.o c_s18aff.o c_x01aaf.o \
    c_x05baf.o c_d1mach.o

install: libnag.so
	install libnag.so /usr/local/lib
	ldconfig
	$(MAKE) clean
	@echo
	@echo libnag.o installed in /usr/local/lib
	@echo

libnag.so: fnag.o cnag.o fourpt.o amos.o pchip.o rksuite.o slatec.o
	$(F77) -shared -o libnag.so $(OBJ)

fnag.o:
	$(F77) $(FFLAGS) -fPIC -c fnag.f

cnag.o:
	$(MAKE) -C src

fourpt.o:
	$(F77) $(FFLAGS) -fPIC -c fourpt.f

amos.o:
	$(MAKE) -C amos

pchip.o:
	$(MAKE) -C pchip

quadpack.o:
	$(MAKE) -C quadpack

rksuite.o:
	$(MAKE) -C rksuite

slatec.o:
	$(MAKE) -C slatec

nagtest: nagtest.f
	$(F77) $(FFLAGS) -o nagtest nagtest.f -lnag `pkg-config --libs gsl`

clean:
	rm -f *.o *.so fort.* *~
	$(MAKE) -C src clean
	$(MAKE) -C amos clean
	$(MAKE) -C pchip clean
	$(MAKE) -C rksuite clean
	$(MAKE) -C slatec clean
