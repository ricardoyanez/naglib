      PROGRAM NAGTEST
      IMPLICIT REAL*8(A-H,O-Z)
      CALL TEST_C05ADF
      CALL TEST_D01AMF
      CALL TEST_D01ASF
      CALL TEST_D01GAF
      CALL TEST_E01BEF
      CALL TEST_E01BFF
      CALL TEST_E01BGF
      CALL TEST_G05DDF
      CALL TEST_G05ECF
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
      FUNCTION FDF(X)
      IMPLICIT REAL*8(A-H,O-Z)
      FDF=DEXP(-X)-X
      RETURN
      END
C
      SUBROUTINE TEST_C05ADF
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER IFAIL
      EXTERNAL FDF
      A=0.0D0
      B=1.0D0
      EPS=1.0D-5
      ETA=0.0D0
      IFAIL=1

      CALL C05ADF(A,B,EPS,ETA,FDF,X,IFAIL)

      WRITE(*,'(/,A)')'** C05ADF Example Program Results **'
      IF (IFAIL.EQ.0) THEN
        WRITE(*,'(/,A,G15.8)')' Zero =',X
      ELSE
        WRITE(*,'(/,A,I4)')' IFAIL =', IFAIL
      END IF
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      FUNCTION FST(X)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /TELNUM/KOUNT
      KOUNT=KOUNT+1
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
      EPSABS=0.0D0
      EPSREL=1.0D-09
      A=0.0D0
      INF=1
      KOUNT=0
      IFAIL=-1

      CALL D01AMF(FST,A,INF,EPSABS,EPSREL,RESULT,ABSERR,W,LW,IW,LIW,
     + IFAIL)

      WRITE(*,'(/,A)')'** D01AMF Example Program Results **'
      WRITE(*,999)'A - lower limit of integration = ',A
      WRITE(*,*)'B - upper limit of integration = infinity'
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
      FUNCTION FSF(X)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /TELNUM/KOUNT
      KOUNT=KOUNT+1
      FSF=0.0D0
      IF (X.GT.0.0D0) FSF=1.0D0/DSQRT(X)
      RETURN
      END
C
      SUBROUTINE TEST_D01ASF
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER LW,LIW,LIMLST
      PARAMETER (LW=800,LIW=LW/2,LIMLST=50)
      INTEGER KOUNT
      DIMENSION ERLST(LIMLST),RSLST(LIMLST),W(LW)
      INTEGER IERLST(LIMLST),IW(LIW)
      EXTERNAL FSF
      COMMON /TELNUM/KOUNT
      EPSABS=1.0D-09
      A=0.0D0
      INF=1
      KOUNT=0
      OMEGA=0.5D0*X01AAF()
      INTEGR=1
      IFAIL=-1

      CALL D01ASF(FSF,A,OMEGA,INTEGR,EPSABS,RESULT,ABSERR,LIMLST,LST,
     + ERLST,RSLST,IERLST,W,LW,IW,LIW,IFAIL)

      WRITE(*,'(/,A)')'** D01ASF Example Program Results **'
      WRITE(*,9999)'A - lower limit of integration = ',A
      WRITE(*,*)'B - upper limit of integration = infinity'
      WRITE(*,9998)'EPSABS - absolute accuracy requested = ',
     + EPSABS
      WRITE(*,*)
      IF (IFAIL.NE.0) WRITE(*,9996)'IFAIL = ', IFAIL
      IF (IFAIL.NE.6 .AND. IFAIL.NE.10) THEN
        WRITE(*,9997)'RESULT - approximation to the integral = ',
     + RESULT
        WRITE(*,9998)'ABSERR - estimate of the absolute error = ',
     + ABSERR
        WRITE(*,9996)'KOUNT - number of function evaluations = ',
     + KOUNT
        WRITE(*,9996) 'LST - number of intervals used = ',LST
        WRITE(*,9996)
     + 'IW(1) - max. no. of subintervals used in any one interval = ',
     + IW(1)
      END IF
 9999 FORMAT(/,1X,A,F10.4)
 9998 FORMAT(1X,A,G11.5)
 9997 FORMAT(1X,A,F11.9)
 9996 FORMAT(1X,A,I4)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_D01GAF
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER NMAX
      PARAMETER (NMAX=21)
      DIMENSION X(NMAX),Y(NMAX)
      DATA X /0.00,0.04,0.08,0.12,0.22,0.26,0.30,0.38,0.39,0.42,0.45,
     +     0.46,0.60,0.68,0.72,0.73,0.83,0.85,0.88,0.90,1.00/
      DATA Y /4.0000,3.9936,3.9746,3.9432,3.8153,3.7467,3.6697,3.4943,
     +     3.4719,3.4002,3.3264,3.3014,2.9412,2.7352,2.6344,2.6094,
     +     2.3684,2.3222,2.2543,2.2099,2.0000/
      IFAIL=1
      N=NMAX

      CALL D01GAF(X,Y,N,ANS,ERROR,IFAIL)

      WRITE(*,'(/,A)')'** D01GAF Example Program Results **'
      IF (IFAIL.EQ.0) THEN
        WRITE(*,9999)' Integral = ', ANS,'  Estimated error = ', ERROR
      ELSE IF (IFAIL.EQ.1) THEN
        WRITE(*,'(/A)')' Less than 4 points supplied'
      ELSE IF (IFAIL.EQ.2) THEN
        WRITE(*,'(/A)')' Points not in increasing or decreasing order'
      ELSE IF (IFAIL.EQ.3) THEN
        WRITE(*,'(/A)')' Points not all distinct'
      END IF
 9999 FORMAT (/A,F12.9,A,F7.4)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_E01BEF
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (N=9,M=11)
      DIMENSION X(N),F(N),D(N)
      DIMENSION PF(M),PX(M)
      DATA X /7.99,8.09,8.19,8.70,9.20,10.0,12.0,15.0,20.0/
      DATA F /0.0D+0,0.27643D-4,0.43750D-1,0.16918D+0,0.46943D+0,
     +     0.94374D+0,0.99864D+0,0.99992D+0,0.99999D+0/
      IFAIL=0

      CALL E01BEF(N,X,F,D,IFAIL)

C     Compute M equally spaced points from X(1) to X(N).
      STEP=(X(N)-X(1))/(M-1)
      DO 40 I=1,M
        PX(I)=DMIN1(X(1)+(I-1)*STEP,X(N))
 40   CONTINUE
      IFAIL=0
      CALL E01BFF(N,X,F,D,M,PX,PF,IFAIL)
      WRITE(*,'(/,A)')'** E01BEF Example Program Results **'
      WRITE(*,'(/A)')'                 Interpolated'
      WRITE(*,'(A)')'      Abscissa          Value'
      DO 60 I=1,M
        WRITE(*,9999)PX(I),PF(I)
 60   CONTINUE
 9999 FORMAT(1X,F13.4,2X,F13.4)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_E01BFF
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (N=9,M=11)
      DIMENSION X(N),F(N),D(N)
      DIMENSION PF(M),PX(M)
      DATA X /7.99,8.09,8.19,8.70,9.20,10.0,12.0,15.0,20.0/
      DATA F /0.0D+0,0.27643D-4,0.43750D-1,0.16918D+0,0.46943D+0,
     + 0.94374D+0,0.99864D+0,0.99992D+0,0.99999D+0/
      DATA D /0.0D+0,5.52510D-4,0.33587D+0,0.34944D+0,0.59696D+0,
     + 6.03260D-2,8.98335D-4,2.93954D-5,0.0D+0/
C     Compute M equally spaced points from X(1) to X(N).
      STEP=(X(N)-X(1))/(M-1)
      DO 40 I=1,M
        PX(I)=DMIN1(X(1)+(I-1)*STEP,X(N))
 40   CONTINUE
      IFAIL=0

      CALL E01BFF(N,X,F,D,M,PX,PF,IFAIL)

      WRITE(*,'(/A)')'** E01BFF Example Program Results **'
      WRITE(*,'(/A)')'                 Interpolated'
      WRITE(*,'(A)')'      Abscissa          Value'
      DO 60 I=1,M
        WRITE(*,9999)PX(I),PF(I)
 60   CONTINUE
 9999 FORMAT(1X,F13.4,2X,F13.4)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_E01BGF
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (N=9,M=11)
      DIMENSION X(N),F(N),D(N)
      DIMENSION PD(M),PF(M),PX(M)
      DATA X /7.99,8.09,8.19,8.70,9.20,10.0,12.0,15.0,20.0/
      DATA F /0.0D+0,0.27643D-4,0.43750D-1,0.16918D+0,0.46943D+0,
     + 0.94374D+0,0.99864D+0,0.99992D+0,0.99999D+0/
      DATA D /0.0D+0,5.52510D-4,0.33587D+0,0.34944D+0,0.59696D+0,
     + 6.03260D-2,8.98335D-4,2.93954D-5,0.0D+0/
C     Compute M equally spaced points from X(1) to X(N).
      STEP=(X(N)-X(1))/(M-1)
      DO 40 I=1,M
        PX(I)=DMIN1(X(1)+(I-1)*STEP,X(N))
 40   CONTINUE
      IFAIL=0

      CALL E01BGF(N,X,F,D,M,PX,PF,PD,IFAIL)

      WRITE(*,'(/A)')'** E01BGF Example Program Results **'
      WRITE(*,'(/,17X,A,A)')'Interpolated','   Interpolated'
      WRITE(*,'(6X,A,10X,A,5X,A)')'Abscissa','Value','Derivative'
      DO 60 I=1,M
        WRITE(*,9999)PX(I),PF(I),PD(I)
 60   CONTINUE
 9999 FORMAT(1X,F13.4,2X,F13.4,1P,E15.3)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_G05DDF
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SPEC(1000)
      A=1.0
      B=1.5
      WRITE(*,'(/A/)')'** G05DDF Example Program Results **'
      DO I=1,5
        X=G05DDF(A,B)
        WRITE(*,9999)X
      ENDDO
 9999 FORMAT(1X,F10.4)
      RETURN
      END
C
C     ------------------------------------------------------------------------
C
      SUBROUTINE TEST_G05ECF
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER NR,IFAIL
      PARAMETER (NR=30)
      DIMENSION R(NR)
      INTEGER G05EYF
      T=2.7
      IFAIL=0

      CALL G05ECF(T,R,NR,IFAIL)

      WRITE(*,'(/A/)')'** G05ECF Example Program Results **'
      IF (IFAIL.EQ.0) THEN
        DO 20 I=1,5
          WRITE(*,9999)G05EYF(R,NR)
 20     CONTINUE
      ELSE
        WRITE(*,'(A)')'Error in TEST_G05EYF'
      ENDIF
 9999 FORMAT(1X,I5)
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
      WRITE(*,'(/,A)')'** S14AAF Example Program Results **'
      WRITE(*,'(/,A)')'       X             Y             IFAIL'
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
      DATA X /1.0,1.25,1.5,1.75,2.0,5.0,10.0,20.0,1000.0,0.0,-5.0/
      WRITE(*,'(/,A)')'** S14ABF Example Program Results **'
      WRITE(*,'(/,A)')'       X             Y             IFAIL'
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
      WRITE(*,'(/,A)')'** S15ADF Example Program Results **'
      WRITE(*,'(/,A)')'       X             Y             IFAIL'
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
      WRITE(*,'(/,A)')'** S18AEF Example Program Results **'
      WRITE(*,'(/,A)')'       X             Y             IFAIL'
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
      WRITE(*,'(/,A)')'** S18AFF Example Program Results **'
      WRITE(*,'(/,A)')'       X             Y             IFAIL'
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
      WRITE(*,'(/,A)')'** S18DEF Example Program Results **'
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
      WRITE(*,'(/,A)')'** X05BAF Example Program Results **'
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
