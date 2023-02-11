C     Author:        Ricardo Yanez <ricardo.yanez@calel.org>
C     Date:          2022-11-07
C     Last modified: 2022-11-15
C
C     This subroutine evaluates the integral from x[1] to x[n] of a function
C     whose values at points x[i], i = 1,n are stored in y[i], i = 1,n. The
C     points x[i], which need not be equally spaced, are assumed to be
C     distinct and arranged in ascending or descending order. The integration
C     is performed using a 4-point rule over each interval.
C
C     Based of Gill P. E. and Miller G. F. (1972), An algorithm for the
C     integration of unequally spaced data, Comput. J. 15 80–83, describing
C     an Angol procedure, which is interpreted and ported to FORTRAN.
C
C     Written explicitly to replace NAG FORTRAN Library routine D01GAF.
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
      SUBROUTINE FOURPT(X,Y,N,ANS,ER,IFAIL)
      IMPLICIT NONE
      INTEGER N,IFAIL
      DOUBLE PRECISION X(N),Y(N),ANS,ER
      DOUBLE PRECISION E,H1,H2,H3,H4,R1,R2,R3,R4,D1,D2,D3,C,S,T
      INTEGER I,J,K
      ANS=0.0
      ER=0.0
      IFAIL=0
      IF (N.LT.4) THEN
        IFAIL=1
        RETURN
      ENDIF
      T=0.0
      E=0.0
      S=0.0
      C=0.0
      R4=0.0
      J=3
      K=N-1
      DO I=J,K
        IF (I.EQ.J) THEN
          H2=X(J-1)-X(J-2)
          D3=(Y(J-1)-Y(J-2))/H2
          H3=X(J)-X(J-1)
          D1=(Y(J)-Y(J-1))/H3
          H1=H2+H3
          D2=(D1-D3)/H1
          H4=X(J+1)-X(J)
          R1=(Y(J+1)-Y(J))/H4
          R2=(R1-D1)/(H4+H3)
          H1=H1+H4
          R3=(R2-D2)/H1
          T=H2*(Y(1)+H2*(D3/2.0-H2*(D2/6.0-(H2+2.0*H3)*R3/12.0)))
          S=-H2**3*(H2*(3.0*H2+5.0*H4)+10.0*H3*H1)/60.0
        ELSE
          H4=X(I+1)-X(I)
          R1=(Y(I+1)-Y(I))/H4
          R4=H4+H3
          R2=(R1-D1)/R4
          R4=R4+H2
          R3=(R2-D2)/R4
          R4=R4+H1
          R4=(R3-D3)/R4
        ENDIF
        T=T+H3*((Y(I)+Y(I-1))/2.0-H3*H3*(D2+R2+(H2-H4)*R3)/12.0)
        C=H3**3*(2.0*H3*H3+5.0*(H3*(H4+H2)+2.0*H4*H2))/120.0
        E=E+(C+S)*R4
        IF (I.EQ.J) THEN
          S=2.0*C+S
        ELSE
          S=C
        ENDIF
        IF (I.EQ.K) THEN
          T=T+H4*(Y(N)-H4*(R1/2.0+H4*(R2/6.0+(2.0*H3+H4)*R3/12.0)))
          E=E-H4**3*R4*(H4*(3.0*H4+5.0*H2)+1.0*H3*(H2+H3+H4))/60.0
          E=E+S*R4
        ELSE
          H1=H2
          H2=H3
          H3=H4
          D1=R1
          D2=R2
          D3=R3
        ENDIF
      ENDDO
      ANS=T
      ER=E
      RETURN
      END
