      PROGRAM NAGTEST
      IMPLICIT REAL*8(A-H,O-Z)
      CALL TEST_C05ADF
      CALL TEST_D01AMF

C      call test_g05ddf(37.d0,7.1d0)
C      call test_g05eyf

      CALL TEST_S14AAF
      CALL TEST_S14ABF
      CALL TEST_S15ADF
      CALL TEST_S18AEF
      CALL TEST_S18AFF
      CALL TEST_S18DEF
      CALL TEST_X05BAF
      END
C
C     ------------------------------------------------------------------------
C
      FUNCTION F1(X)
      IMPLICIT REAL*8(A-H,O-Z)
      F1=DEXP(-X)-X
      RETURN
      END
C
      SUBROUTINE TEST_C05ADF
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER IFAIL
      EXTERNAL F1
      EXTERNAL C05ADF
      WRITE(*,'(/,a)') 'C05ADF Example Program Results'
      A=0.0d0
      B=1.0d0
      EPS=1.0d-5
      ETA=0.0d0
      IFAIL=1
      CALL C05ADF(A,B,EPS,ETA,F1,X,IFAIL)
      IF (IFAIL.EQ.0) THEN
        WRITE(*,'(/,A,G15.8)')'Zero =',X
      ELSE
        WRITE(*,'(/,A,I4)')'IFAIL =', IFAIL
      END IF
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      FUNCTION FST(X)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /TELNUM/KOUNT
      KOUNT = KOUNT + 1
      FST=1.0d0/((X+1.0d0)*DSQRT(X))
      RETURN
      END
C
      SUBROUTINE TEST_D01AMF
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER LW,LIW
      PARAMETER (LW=800,LIW=LW/4)
      INTEGER KOUNT
      DIMENSION W(LW)
      INTEGER IW(LIW)
      EXTERNAL FST
      COMMON /TELNUM/KOUNT
      WRITE(*,'(/,a)') 'D01AMF Example Program Results'

      EPSABS=0.0d0
      EPSREL= 1.0d-09
      A=0.0d0
      INF=1
      KOUNT=0
      IFAIL=-1
      CALL D01AMF(FST,A,INF,EPSABS,EPSREL,RESULT,ABSERR,W,LW,IW,LIW,
     + IFAIL)

      WRITE(*,999)'A - lower limit of integration = ',A
      WRITE(*,'(1X,A)')'B - upper limit of integration = infinity'
      WRITE(*,998)'EPSABS - absolute accuracy requested = ',
     + EPSABS
      WRITE(*,998)'EPSREL - relative accuracy requested = ',
     + EPSREL
      WRITE (*,*)
      IF (IFAIL.NE.0) WRITE(*,996)'IFAIL = ',IFAIL
      IF (IFAIL.LE.5) THEN
        WRITE(*,997)'RESULT - approximation to the integral = ',
     + RESULT
        WRITE(*,998)'ABSERR - estimate of the absolute error = ',
     + ABSERR
        WRITE(*,996)'KOUNT - number of function evaluations = ',
     + KOUNT
        WRITE (*,996) 'IW(1) - number of subintervals used = ',
     + IW(1)
      END IF
 999  FORMAT(/,1X,A,F10.4)
 998  FORMAT(1X,A,G11.5)
 997  FORMAT(1X,A,F11.9)
 996  FORMAT(1X,A,I4)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_G05DDF(A,B)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SPEC(1000)

      DO I=1,1000
        SPEC(I) = 0.0
      END DO

      DO I=1,1000000
        X = G05DDF(A,B)
        J = NINT(X*10.)
        IF ( J .GE. 1 .AND. J .LE. 1000 ) THEN
          SPEC(J) = SPEC(J) + 1.0;
        END IF
      END DO
      
      DO I=1,1000
        WRITE(*,*)I/10.,SPEC(I)
      END DO

      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_G05EYF
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER NR,IFAIL
      PARAMETER (NR=30)
      DIMENSION R(NR)
      INTEGER G05EYF
      T=2.7
      IFAIL=0
      CALL G05ECF(T,R,NR,IFAIL)
      IF ( IFAIL .EQ. 0 ) THEN
        DO 20 I = 1,5
          WRITE(*,*) G05EYF(R,NR)
 20     CONTINUE
      ELSE
        WRITE(*,'(A)')'Error in TEST_G05EYF'
      END IF
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_S14AAF
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER IFAIL
      DIMENSION X(8)
      DATA X /1.0,1.25,1.5,1.75,2.0,5.0,10.0,-1.5/
      WRITE(*,'(/,a)') 'S14AAF Example Program Results'
      WRITE(*,'(/,a)') '       X             Y             IFAIL'
      DO I=1,8
        IFAIL=0
        Y=S14AAF(X(I),IFAIL)
        WRITE(*,'(1P2G15.8,I8)')X(I),Y,IFAIL
      END DO
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_S14ABF
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER IFAIL
      DIMENSION X(11)
      WRITE(*,'(/,a)') 'S14ABF Example Program Results'
      WRITE(*,'(/,a)') '       X             Y             IFAIL'
      DATA X /1.0,1.25,1.5,1.75,2.0,5.0,10.0,20.0,1000.0,0.0,-5.0/
      DO I=1,11
        IFAIL=0
        Y=S14ABF(X(I),IFAIL)
        WRITE(*,'(1P2G15.8,I8)')X(I),Y,IFAIL
      END DO
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_S15ADF
      IMPLICIT REAL*8(A-H,O-Z) 
      INTEGER IFAIL
      DIMENSION X(5)
      DATA X /-10.0,-1.0,0.0,1.0,10.0/
      WRITE(*,'(/,a)') 'S15ADF Example Program Results'
      WRITE(*,'(/,a)') '       X             Y             IFAIL'
      DO I=1,5
        IFAIL=0
        Y=S15ADF(X(I),IFAIL)
        WRITE(*,'(1P2G15.8,I8)')X(I),Y,IFAIL
      END DO
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_S18AEF
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER IFAIL
      DIMENSION X(10)
      DATA X /0.0,0.5,1.0,3.0,6.0,8.0,10.0,15.0,20.0,-1.0/
      WRITE(*,'(/,a)') 'S18AEF Example Program Results'
      WRITE(*,'(/,a)') '       X             Y             IFAIL'
      DO I=1,10
        IFAIL=0
        Y=S18AEF(X(I),IFAIL)
        WRITE(*,'(1P2G15.8,I8)')X(I),Y,IFAIL
      END DO
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_S18AFF
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(10)
      DATA X /0.0,0.5,1.0,3.0,6.0,8.0,10.0,15.0,20.0,-1.0/
      WRITE(*,'(/,a)') 'S18AFF Example Program Results'
      WRITE(*,'(/,a)') '       X             Y             IFAIL'
      DO I=1,10
        IFAIL=0
        Y=S18AFF(X(I),IFAIL)
        WRITE(*,'(1P2G15.8,I8)')X(I),Y,IFAIL
      END DO
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_S18DEF
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER IFAIL
      INTEGER N
      PARAMETER (N=2)
      COMPLEX*16 Z(5)
      DATA Z /(0.3,-0.4),(2.0,0.0),(-1.0,0.0),(-6.1,9.8),(-6.1,9.8)/
      DIMENSION FNU(5)
      DATA FNU /0.0,2.3,2.12,5.5,5.5/
      CHARACTER*1 SCALE(5)
      DATA SCALE /'U','U','U','U','S'/
      COMPLEX*16 CY(N)
      WRITE(*,'(/,a)')'S18DEF Example Program Results'
      WRITE(*,99997)'FNU','Z','SCALE','CY(1)','CY(2)','NZ','IFAIL'
99997 FORMAT(/,3X,A,10X,A,9X,A,7X,A,14X,A,8X,A,2X,A)
      DO I=1,5
        IFAIL=0
        CALL S18DEF(FNU(I),Z(I),N,SCALE(I),CY,NZ,IFAIL)
        WRITE(*,99998) FNU(I),Z(I),SCALE(I),CY(1),CY(2),NZ,IFAIL
      END DO
99998 FORMAT(F7.4,' (',F7.3,',',F7.3,') ',2X,A,2(' (',F7.3,',',F7.3,
     + ') '),2I5)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_X05BAF
      IMPLICIT REAL*8(A-H,O-Z)
      WRITE(*,'(/,a)')'X05BAF Example Program Results'
      S1=X05BAF()
      E=1.0
      T=1.0
      DO N=1,1000
        T=T/N
        E=E+T
      END DO
      S2=X05BAF()
      CPTIME=S2-S1
      WRITE(*,9999) 'It took',CPTIME,' seconds to compute e =',E
 9999 FORMAT(/,A,G13.6,A,G22.16,/)
      RETURN
      END
