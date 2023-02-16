# naglib - NAG Routine Replacements

The following shared library is intended to replace the few [NAG Library](https://www.nag.com/content/nag-library/) routines listed bellow.

## Installation

Clone repository, enter source directory and compile,

   ```  
   $ git clone https://github.com/ricardoyanez/naglib.git  
   $ cd naglib  
   $ sudo make install  
   ```
## Dependencies

Your system needs the GNU Fortran compiler, and the Standard C library and the [GSL Library](#gsl-library) development files.

    $ sudo apt-get install gfortran libc6-dev libgsl-dev

## Usage

The shared library `libnag.so` is installed in `/usr/local/lib`. Call a NAG routine with its normal syntax, and compile your code with `-lnag`,

    $ gfortan -o myprog myprof.f -lnag -lgsl -lgslcblas -lm
    
or

    $ gfortan -o myprog myprof.f -lnag `pkg-config --libs gsl`

## NAG Routine Replacements

The following approach has been adopted:

* Avoid disturbing original codes as much as possible.
* Avoid distributing external software.

External code is downloaded from official repositories before being compiled. Any necessary changes are distributed as patches to the original source code, which is applied before compilation. Patch files have the extension `.patch`, and are created by the GNU `diff` command,

    $ diff -Naur source.f.orig source.f > source.f.patch

and applied with the GNU `patch` command,

    $ patch source.f source.f.patch

Most common changes to original code include casting variables from `REAL` to `DOUBLE PRECISION`, changing intrinsic functions to their `DOUBLE PRECISION` counterparts, for example `ABS()` to `DABS()`, and parameters and numbers to `DOUBLE PRECISION`, for example, `1.0E+00` to `1.0D+00`.

The NAG routine calls of your program should not be changed. Instead, a wrapper subroutine or function is used with the exact same name and input/output parameters as the NAG routine called. The wrapper subroutines and functions create the correct conditions to call the numerical substitutes by passing or declaring new variables, defining external functions and conforming the output to coincide with the expected NAG routine output.

### C05ADF

Locates a zero of a continuous function in a given interval by a combination of the methods of linear interpolation, extrapolation and bisection.

```Fortran
SUBROUTINE C05ADF(A,B,EPS,ETA,F,X,IFAIL)
INTEGER IFAIL
REAL A,B,EPS,ETA,F,X
EXTERNAL F
```

The **C05ADF** routine is replaced by the [Numerical Recipes](#numerical-recipes-in-fortran-77) **ZBRENT** function.

### C05AVF

Attempts to locate an interval containing a simple zero of a continuous function using a binary search. It uses reverse communication for evaluating the function.

```Fortran
SUBROUTINE C05AVF(X,FX,H,BOUNDL,BOUNDU,Y,C,IND,IFAIL)
INTEGER IND,IFAIL
REAL X,FX,H,BOUNDL,BOUNDU,Y,C(11)
```

### D01AMF

Calculates an approximation to the integral of a function _f(x)_ over an infinite or semi-infinite interval.

```Fortran
SUBROUTINE D01AMF(F,BOUND,INF,EPSABS,EPSREL,RESULT,ABSERR,W,LW,IW,LIW,IFAIL)
INTEGER INF,LW,IW(LIW),LIW,IFAIL
REAL F,BOUND,EPSABS,EPSREL,RESULT,ABSERR,W(LW)
EXTERNAL F
```

The **D01AMF** routine is replaced by [QUADPACK](#quadpack---numerical-integration) routine **DQAGI**.

### D01ASF

Calculates an approximation to the sine or the cosine transform of a function _g_ over [a, ∞) for a user-specified value of _ω_.

```Fortran
 SUBROUTINE D01ASF(G,A,OMEGA,KEY,EPSABS,RESULT,ABSERR,
1 LIMLST,LST,ERLST,RSLST,IERLST,W,LW,IW,LIW,IFAIL)
 INTEGER KEY,LIMLST,LST,IERLST(LIMLST),LW,IW(LIW),LIW,IFAIL
 REAL G,A,OMEGA,EPSABS,RESULT,ABSERR,ERLST(LIMLST),RSLST(LIMLST),W(LW)
 EXTERNAL G
```
The **D01ASF** routine is replaced by [QUADPACK](#quadpack---numerical-integration) routine **QAWFE**.

### D01GAF

Integrates a function which is specified numerically at four or more points, over the whole of its specified range, using third-order finite-difference formulae with error estimates, according to a method due to Gill and Miller.

```Fortran
SUBROUTINE D01GAF(X,Y,N,ANS,ER,IFAIL)
INTEGER N,IFAIL
REAL X(N),Y(N),ANS,ER
```

The **D01GAF** routine is replaced by the [Gill-Miller Algorithm](#gill-miller-algorithm) routine **FOURPT**.

```Fortran
SUBROUTINE FOURPT(X,Y,N,ANS,ER,IFAIL)
INTEGER N,IFAIL
DOUBLE PRECISION X(N),Y(N),ANS,ER
```

### D02BAF

Solves an initial value problem for a first-order system of ordinary differential equations using Runge-Kutta methods.

```Fortran
CALL D02BAF(X,XEND,N,Y,TOL,FCN,W,IFAIL)
```

The **D02BAF** routine is replaced by [RKSUITE](#rksuite---a-suite-of-runde-kutta-codes).

### D02BBF

Solves an initial value problem for a first-order system of ordinary differential equations using Runge-Kutta methods.

```Fortran
CALL D02BBF(X,XEND,N,Y,TOL,IRELAB,FCN,OUTPUT,W,IFAIL)
```

The **D02BBF** routine is replaced by [RKSUITE](#rksuite---a-suite-of-runde-kutta-codes).

### E01BEF

Computes a monotonicity-preserving piecewise cubic Hermite interpolant to a set of data points.

```Fortran
SUBROUTINE E01BEF(N,X,F,D,IFAIL)
INTEGER N,IFAIL
REAL X(N),F(N),D(N)
```

The **E01BEF** routine is replaced by the [PCHIP](#pchip---piecewise-cubic-hermite-interpolation-package) routine **DPCHIM**.

### E01BFF

Evaluates a piecewise cubic Hermite interpolant at a set of points.

```Fortran
SUBROUTINE E01BFF(N,X,F,D,M,PX,PF,IFAIL)
INTEGER N,M,IFAIL
REAL X(N),F(N),D(N),PX(M),PF(M)
```

The **E01BFF** routine is replaced by then [PCHIP](#pchip---piecewise-cubic-hermite-interpolation-package) routine **DPCHFE**.

### E01BGF

Evaluates a piecewise cubic Hermite interpolant and its first derivative at a set of points.

```Fortran
SUBROUTINE E01BGF(N,X,F,D,M,PX,PF,PD,IFAIL)
INTEGER N,M,IFAIL
REAL X(N),F(N),D(N),PX(M),PF(M),PD(M)
```

The **E01BGF** routine is replaced by the [PCHIP](#pchip---piecewise-cubic-hermite-interpolation-package) routine **DPCHFD**.

### G05DDF

Returns a pseudo-random real number taken from a Normal (Gaussian) distribution with mean _a_ and standard deviation _b_.

```Fortran
real FUNCTION G05DDF(A,B)
real A,B
```

The **G05DDF** function is replaced with a C wrapper that returns a Gaussian random variate from the [GSL Library](#gsl-library).

### G05ECF

Sets up the reference vector _R_ for a Poisson distribution with mean _t_.

```Fortran
SUBROUTINE G05ECF(T,R,NR,IFAIL)
INTEGER NR,IFAIL
real T,R(NR)
```

The **G05ECF** routine is replaced with a C wrapper that calls functions to generate a Poisson Distribution CDF from the [GSL Library](#gsl-library).

### G05EYF

Returns a pseudo-random integer taken from a discrete distribution defined by a reference vector _R_.

```Fortran
INTEGER FUNCTION G05EYF(R, NR)
INTEGER NR
real R(NR)
```

The **G05EYF** function is replaced with a C wrapper that generates a random number _x_ from the [GSL Library](#gsl-library) and searches the CDF in the reference vector _R_ for the smallest value _y_ such that _R(y)_ &lt; x &le; _R(y+1)_ .


### S14AAF

S14AAF returns the value of the Gamma function _Γ(x)_, via the routine name.

```Fortran
REAL FUNCTION S14AAF(X,IFAIL)
INTEGER IFAIL
REAL X
```

The **S14AAF** routine is substituted with a C wrapper function that calls **tgamma()**.

### S14ABF

Returns a value for the logarithm of the Gamma function, ln _Γ(x)_, via the routine name.

```Fortran
REAL FUNCTION S14ABF(X,IFAIL)
INTEGER IFAIL
REAL X
```

The **S14ABF** routine is substituted with a C wrapper function that calls **lgamma()**.

### S15ADF

Returns the value of the complementary error function, _erfc x_, via the routine name.

```Fortran
REAL FUNCTION S15ADF(X,IFAIL)
INTEGER IFAIL
REAL X
```

The **S15ADF** routine is substituted with a C wrapper function that calls **erfc()**.

### S18AEF

Returns the value of the modified Bessel Function _I0(x)_, via the routine name.

```Fortran
REAL FUNCTION S18AEF(X,IFAIL)
INTEGER IFAIL
REAL X
```

The **S18AEF** routine is replaced by a C wrapper that calls the [GSL Library](#gsl-library)  Bessel functions.

### S18AFF

Returns the value of the modified Bessel Function _I1(x)_, via the routine name.

```Fortran
REAL FUNCTION S18AFF(X,IFAIL)
INTEGER IFAIL
REAL X
```

The **S18AFF** routine is replaced by a C wrapper that calls the [GSL Library](#gsl-library)  Bessel functions.

### S18DEF

Returns a sequence of values for the modified Bessel functions _I<sub>ν+n</sub>(z)_ for complex _z_, non-negative
_ν_ and _n_ = 0, 1, . . . , _N_ − 1, with an option for exponential scaling.

```Fortran
SUBROUTINE S18DEF(FNU,Z,N,SCALE,CY,NZ,IFAIL)
INTEGER N,NZ,IFAIL
REAL FNU
COMPLEX Z,CY(N)
CHARACTER∗1 SCALE
```

The **S18DEF** routine is replaced by the modified Bessel function of [AMOS](#amos---bessel-functions) routine **ZBESI**.

### X05BAF

Returns the amount of processor time used since an unspecified previous time, via the routine name.

```Fortran
REAL FUNCTION X05BAF()
```

The **X05BAF** routine is substituted with a C wrapper function that calls **clock()**.

## AMOS - Bessel Functions

The AMOS routines for evaluating Bessel functions are distributed by [Netlib](https://netlib.org/amos/).

<ins>REFERENCE</ins>

Amos, D. E. (1986) Algorithm 644: A portable package for Bessel functions of a complex argument and nonnegative order. *ACM Trans. Math. Software 12 265–273*.

## Gill-Miller Algorithm

The routine **FOURPT** is a Fortran port of the procedure by Gill and Miller written in ALGOL.

<ins>REFERENCE</ins>

Gill P. E. and Miller G. F. (1972) An algorithm for the integration of unequally spaced data, *Comput. J.* 15 80–83.

## Numerical Recipes in Fortran 77

Whomever has purchased a copy of *Numerical Recipes in Fortran 77: The Art of Scientific Computing* by William Press, Brian Flannery, Saul Teukolsky and William Vetterling, is entitled to use the machine readable programs for personal use. Since distributing a copy is explicitly forbidden, the Numerical Recipes source codes will have to be placed in the `nrf77` directory. It will be assumed that any interested person in GRAZING has a personal copy of this excellent book.

## PCHIP - Piecewise Cubic Hermite Interpolation Package

The PCHIP routines are part of the Slatec Library. They can be found in [Netlib](https://netlib.org/slatec/pchip/).

<ins>REFERENCE</ins>

Fritsch F. N. (1982) PCHIP final specifications Report UCID–30194 Lawrence Livermore National Laboratory.

## QUADPACK - numerical integration

[QUADPACK](https://nines.cs.kuleuven.be/software/QUADPACK/) is a set of Fortran 77 routines for integrating one-variable functions. QUADPACK is licensed as Public Domain and its source code can be downloaded from [Netlib](https://netlib.org/quadpack/).

<ins>REFERENCE</ins>

R. Piessens, E. De Doncker-Kapenga and C. W. Überhuber. QUADPACK: a subroutine package for automatic integration. Springer, ISBN: 3-540-12553-1. 1983. 

## RKSUITE - a suite of Runde-Kutta codes

[RKSUITE](https://netlib.sandia.gov/ode/rksuite/) is a suite of Runde-Kutta codes that is available free of charge to the scientific community. It has no discernable license.

## GSL Library

The [GNU Scientific Library](https://www.gnu.org/software/gsl/) (GSL) is a numerical C and C++ library licensed under the GNU General Public License. The library includes a broad range of numerical subject areas.

## Bug Reporting

Please report bugs to Ricardo Yanez &lt;ricardo.yanez@calel.org&gt;.

## License

The routines and wrappers to substitute the NAG routines were written by Ricardo Yanez &lt;ricardo.yanez@calel.org&gt; and licensed under the [GNU General Public License (GPL) version 2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html) or later.
