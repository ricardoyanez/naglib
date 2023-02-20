C
C     Copyright (c) 2022-2023 Ricardo Yanez <ricardo.yanez@calel.org>
C
C     Wrappers to the NAG Fortran Library
C
C
C     This program is free software; you can redistribute it and/or modify
C     it under the terms of the GNU General Public License as published by
C     the Free Software Foundation; either version 2 of the License, or
C     (at your option) any later version.
C
C     This program is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with this program; if not, write to the Free Software
C     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
C
C     ------------------------------------------------------------------------
C
C     C05ADF locates a zero of a continuous function in a given interval by
C     combination of the methods of linear interpolation, extrapolation and
C     bisection.
C
C     The C function c_c05adf is used to substitute C05ADF by calling GSL
C     root-finding functions using Brent's method.
C
      SUBROUTINE C05ADF(A,B,EPS,ETA,F,X,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      EXTERNAL F
      CALL c_c05adf(A,B,EPS,ETA,F,X,IFAIL)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     C05AVF attempts to locate an interval containing a simple zero of a
C     continuous function using a binary search. It uses reverse communication
C     for evaluating the function
C
      SUBROUTINE C05AVF(X,FX,H,BOUNDL,BOUNDU,Y,C,IND,IFAIL)
      WRITE(*,*) '*** Call to C05AVF'
      WRITE(*,*) 'Warning: This function has not been implemented.'
      STOP
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     D01AMF calculates an approximation to the integral of a function f(x)
C     over an infinite or semi-infinite interval.
C
C     The QUADPACK routine QAGI is used to substitute D01AMF, as implemented
C     by GSL.
C
      SUBROUTINE D01AMF(F,BOUND,INF,EPSABS,EPSREL,RESULT,ABSERR,
     &     W,LW,IW,LIW,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      EXTERNAL F
      CALL c_d01amf(F,BOUND,INF,EPSABS,EPSREL,RESULT,ABSERR,
     &     W,LW,IW,LIW,IFAIL)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     D01ASF calculates an approximation to the sine or the cosine transform
C     of a function g over [a,inf)
C
C     The QUADPACK routine QAWF is used to substitute D01ASF, as implemented
C     by GSL.
C
      SUBROUTINE D01ASF(G,A,OMEGA,KEY,EPSABS,RESULT,ABSERR,
     &     LIMLST,LST,ERLST,RSLST,IERLST,W,LW,IW,LIW,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      EXTERNAL G
      CALL c_d01asf(G,A,OMEGA,KEY,EPSABS,RESULT,ABSERR,
     &     LIMLST,LST,ERLST,RSLST,IERLST,W,LW,IW,LIW,IFAIL)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     D01GAF integrates a function which is specified numerically at four or
C     more points, over the whole of its specified range, using third-order
C     finite-difference formulae with error estimates, according to a method
C     due to Gill and Miller.
C
C     A Fortran port of procedure 4pt written in Angol by Gill and Miller
C     is used to substitute D01GAF.
C
      SUBROUTINE D01GAF(X,Y,N,ANS,ER,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      DIMENSION X(N),Y(N)
      CALL FOURPT(X,Y,N,ANS,ER,IFAIL)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     D02BAF solves an initial value problem for a first-order system of
C     ordinary differential equations using Runge-Kutta methods.
C
C     RKSUITE is used to substitute D02BAF.
C
      SUBROUTINE D02BAF(X,XEND,N,Y,TOL,FCN,W,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      EXTERNAL FCN
      DIMENSION Y(N),YPGOT(N),YMAX(N)
      DIMENSION THRES(N)
      DIMENSION WORK(32*N)
      INTEGER UFLAG
      DO I=1,N
        THRES(I)=TOL
      ENDDO
      CALL SETUP(N,X,Y,XEND,TOL,THRES,2,'U',.FALSE.,0.0D0,WORK,32*N,
     &     .TRUE.)
      CALL UT(FCN,XEND,TGOT,Y,YPGOT,YMAX,WORK,UFLAG)
      IFAIL=1
      IF (UFLAG.EQ.1) THEN
        IFAIL=0
      ENDIF
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     D02BBF solves an initial value problem for a first-order system of
C     ordinary differential equations using Runge-Kutta methods.
C
C     RKSUITE is used to substitute D02BBF.
C
      SUBROUTINE D02BBF(X,XEND,N,Y,TOL,IRELAB,FCN,OUTPUT,W,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      EXTERNAL FCN
      DIMENSION Y(N),YPGOT(N),YMAX(N)
      DIMENSION THRES(N)
      DIMENSION WORK(32*N)
      INTEGER UFLAG
      DO I=1,N
        THRES(I)=TOL
      ENDDO
      CALL SETUP(N,X,Y,XEND,TOL,THRES,2,'U',.FALSE.,0.0D0,WORK,32*N,
     &     .TRUE.)
      CALL UT(FCN,XEND,TGOT,Y,YPGOT,YMAX,WORK,UFLAG)
      IFAIL=1
      IF (UFLAG.EQ.1) THEN
        IFAIL=0
      ENDIF
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     E01BEF computes a monotonicity-preserving piecewise cubic Hermite
C     interpolant to a set of data points.
C
C     The DPCHIM routine is used to substitute E01BEF.
C
      SUBROUTINE E01BEF(N,X,F,D,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      DIMENSION X(N),F(N),D(N)
      INTEGER IFAIL,INCFG,IERR
      INCFD=1
      CALL DPCHIM(N,X,F,D,INCFD,IERR)
      IF (IERR.EQ.-1) THEN
        IFAIL=1
      ELSEIF (IERR.EQ.-2) THEN
        WRITE(*,*)'DPCHIM: INCFD < 1'
        STOP
      ELSEIF (IERR.EQ.-3) THEN
        IFAIL=2
      ELSE
        IFAIL=0
      ENDIF
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     E01BFF evaluates a piecewise cubic Hermite interpolant at a set of
C     points.
C
C     The DPCHFE routine is used to substitute E01BFF.
C
      SUBROUTINE E01BFF(N,X,F,D,M,PX,PF,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      DIMENSION X(N),F(N),D(N),PX(M),PF(M)
      INTEGER IFAIL,INCFG,IERR
      LOGICAL SKIP
      INCGF=1
      SKIP=.TRUE.
      CALL DPCHFE(N,X,F,D,INCFD,SKIP,M,PX,PF,IERR)
      IF (IERR.EQ.-1) THEN
        IFAIL=1
      ELSEIF (IERR.EQ.-2) THEN
        WRITE(*,*)'DPCHFE: INCFD < 1'
        STOP
      ELSEIF (IERR.EQ.-3) THEN
        IFAIL=2
      ELSEIF (IERR.EQ.-4) THEN
        IFAIL=3
      ELSE
        IFAIL=0
      ENDIF
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     E01BGF evaluates a piecewise cubic Hermite interpolant and its first
C     derivative at a set of points.
C
C     The DPCHFD routine is used to substitute E01BGF.
C
      SUBROUTINE E01BGF(N,X,F,D,M,PX,PF,PD,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      DIMENSION X(N),F(N),D(N),PX(M),PF(M),PD(M)
      INTEGER IFAIL,INCFD,IERR
      LOGICAL SKIP
      INCFD=1
      SKIP=.TRUE.
      CALL DPCHFD (N,X,F,D,INCFD,SKIP,M,PX,PF,PD,IERR)
      IF (IERR.EQ.-1) THEN
        IFAIL=1
      ELSEIF (IERR.EQ.-2) THEN
        WRITE(*,*)'DPCHFD: INCFD < 1'
        STOP
      ELSEIF (IERR.EQ.-3) THEN
        IFAIL=2
      ELSEIF (IERR.EQ.-4) THEN
        IFAIL=3
      ELSEIF (IERR.EQ.-5) THEN
        WRITE(*,*)'DPCHFD: Error ',IERR
        STOP
      ELSE
        IFAIL=0
      ENDIF
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     G05DDF returns a pseudo-random real number taken from a Normal
C     (Gaussian) distribution with mean a and standard deviation b.
C
C     The C function c_g05ddf is used to substitute G05DDF by calling
C     GSL functions.
C
      REAL*8 FUNCTION G05DDF(A,B)
      IMPLICIT REAL*8(A-G,O-Z)
      G05DDF=c_g05ddf(A,B)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     G05ECF sets up the reference vector R for a Poisson distribution with
C     mean t for use in G05EYF. (See g05ecf.pdf)
C
C     The C function c_g05ecf is used to substitute G05ECF by calling
C     GSL functions.
C
      SUBROUTINE G05ECF(T,R,NR,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      DIMENSION R(NR)
C     On entry, T < 0
      IF ( T < 0.0 ) THEN
        IFAIL=1
        RETURN
      END IF
C     On entry, NR is too small
      NRMIN=INT(T+7.5*DSQRT(T)+8.5)-MAX(0,INT(T-7.15*DSQRT(T)))+4
      IF ( NR .LE. NRMIN ) THEN
        IFAIL=2
        RETURN
      END IF
      CALL c_g05ecf(T,R,NR)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     G05EYF returns a pseudo-random integer taken from a discrete distribution
C     defined by a reference vector R.
C
C     The C function c_g05eyf is used to substitute G05EYF by calling
C     GSL functions.
C
      INTEGER FUNCTION G05EYF(R,NR)
      IMPLICIT REAL*8(A-G,O-Z)
      DIMENSION R(NR)
      INTEGER c_g05eyf
      G05EYF=c_g05eyf(R,NR)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     S14AAF returns the value of the Gamma function via the routine name.
C
C     The C function c_s14aaf is used to substitute S14AAF by calling tgamma().
C
      FUNCTION S14AAF(X,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      S14AAF=c_s14aaf(X,IFAIL)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     S14ABF returns a value for the logarithm of the Gamma function,
C     ln gamma(x), via the routine name.
C
C     The C function c_s14abf is used to substitute S14ABF by calling lgamma().
C
      FUNCTION S14ABF(X,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      S14ABF=c_s14abf(X,IFAIL)
      IF (IFAIL.NE.0) THEN
        S14ABF=0.0
      END IF
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     S15ADF returns the value of the complementary error function, erfc x,
C     via the routine name.
C
C     The C function c_s15adf is used to substitute S15ADF by calling erfc().
C
      FUNCTION S15ADF(X,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      S15ADF=c_s15adf(X,IFAIL)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     S18AEF returns the value of the modified Bessel Function I0(x), via the
C     routine name.
C
C     The C function c_s18aef is used to substitute S18AEF by calling GSL
C     functions.
C
      FUNCTION S18AEF(X,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      S18AEF=c_s18aef(X,IFAIL)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     S18AFF returns the value of the modified Bessel Function I1(x), via the
C     routine name.
C
C     The C function c_s18aff is used to substitute S18AFF by calling GSL
C     functions.
C
      FUNCTION S18AFF(X,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      S18AFF=c_s18aff(X,IFAIL)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     S18DEF returns a sequence of values for the modified Bessel functions
C     for complex z, non-negative orders, with an option for exponential
C     scaling.
C
C     The Amos Bessel routine ZBESI is used to substitute S18DEF
C
      SUBROUTINE S18DEF(FNU,Z,N,SCALE,CY,NZ,IFAIL)
      IMPLICIT REAL*8(A-G,O-Z)
      COMPLEX*16 Z,CY(N)
      CHARACTER*1 SCALE
      DIMENSION CYR(N),CYI(N)
      ZR=DBLE(Z)
      ZI=AIMAG(Z)
      KODE=1
      IF (SCALE.EQ.'S' .OR. SCALE.EQ.'s') THEN
        KODE=2
      ENDIF
      CALL ZBESI(ZR,ZI,FNU,KODE,N,CYR,CYI,NZ,IFAIL)
      DO I=1,N
        CY(I)=DCMPLX(CYR(I),CYI(I))
      ENDDO
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     NAG X01AAF provides the mathematical constant pi
C
      REAL*8 FUNCTION X01AAF()
      IMPLICIT REAL*8(A-G,O-Z)
      X01AAF=c_x01aaf()
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     NAG X05BAF returns the amount of processor time used since an unspecified
C     previous time, via the routine name.
C
C     The C function c_x05baf is used to substitute X05BAF by calling clock().
C
      REAL*8 FUNCTION X05BAF()
      IMPLICIT REAL*8(A-G,O-Z)
      X05BAF=c_x05baf()
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     Returns double-precision machine constants
C
      REAL*8 FUNCTION D1MACH(I)
      IMPLICIT REAL*8(A-G,O-Z)
      INTEGER I
      D1MACH=c_d1mach(I)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
C     Returns integer machine constants
C
      INTEGER FUNCTION I1MACH(I)
      IMPLICIT REAL*8(A-G,O-Z)
      INTEGER I
      I1MACH=c_i1mach(I)
      RETURN
      END
