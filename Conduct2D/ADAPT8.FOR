CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 8 -- CIRCULAR TUBE WITH RADIAL INTERNAL FINS
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION W(NI,NJ),T(NI,NJ)
      EQUIVALENCE (F(1,1,1),W(1,1)),(F(1,1,2),T(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='CIRCULAR TUBE WITH RADIAL INTERNAL FINS'
      PRINTF='PRINT8'
      PLOTF='PLOT8'
      NFINS=6
      CALL DATA2(HRATIO,0.3,TRATIO,0.15)
      RAD=1.
      DIA=2.*RAD
      H=RAD*HRATIO
      C=RAD-H
      PI=3.14159
      ALPHA=PI/FLOAT(NFINS)
      ALPHAT=ALPHA*TRATIO
      ALPHAC=ALPHA-ALPHAT
      CALL INTA3(NZX,2,NCVX(1),3,NCVX(2),7)
      CALL INTA3(NZY,2,NCVY(1),8,NCVY(2),4)
      CALL DATA4(XZONE(1),ALPHAT,XZONE(2),ALPHAC,YZONE(1),C,
     1    YZONE(2),H)
      CALL ZGRID
      R(1)=0.
      MODE=3
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='      W/WBAR     ' 
      TITLE(2)='  (T-TW)/(TB-TW) '
      CALL INTA6(KSOLVE(1),1,KPRINT(1),1,KPRINT(2),1,KPLOT(1),1,
     1           KPLOT(2),1,LAST,6)
      CALL DATA7(AMU,1.,DEN,1.,CP,1.,COND,1.,DPDZ,-1.,DTDZ,1.,TW,0.)
      RHOCP=DEN*CP
      CNDFIN=COND*3.5 
      DO 100 J=1,M1
      DO 100 I=1,L1
         W(I,J)=0.
         T(I,J)=TW
  100 CONTINUE
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY OUTPUT
      IF(ITER.EQ.3) THEN
         KSOLVE(1)=0
         KSOLVE(2)=1
      ENDIF
      ASUM=0.
      WSUM=0.
      TSUM=0.
      DO 200 J=2,M2
      DO 200 I=2,L2
         AR=XCV(I)*YCVR(J)
         IF(X(I).LT.ALPHAT.AND.Y(J).GT.C) AR=0.
         ASUM=ASUM+AR
         WSUM=WSUM+W(I,J)*AR
         TSUM=TSUM+W(I,J)*T(I,J)*AR
  200 CONTINUE
      WBAR=WSUM/ASUM
      TB=TSUM/(WSUM+SMALL) 
      WP=ALPHAC*RAD+H+ALPHAT*C
      DH=4.*ASUM/WP 
      RE=DH*WBAR*DEN/AMU
      FRE=-2.*DPDZ*DH/(DEN*WBAR**2+SMALL)*RE
      HP=ALPHA*RAD
      QW=DTDZ*RHOCP*WSUM/HP
      ANU=QW*DIA/(COND*(TW-TB)+SMALL)
      DO 210 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,220)
  220    FORMAT(1X,'ITER',2X,'W(8,8)',4X,'W(11,4)',3X,'T(8,8)'
     1   ,4X,'T(11,4)',4X,'FRE',8X,'NU')
         WRITE(IUNIT,230) ITER,W(8,8),W(11,4),T(8,8),T(11,4),FRE,ANU
  230    FORMAT(2X,I2,1P6E10.2)
  210 CONTINUE    
      IF(ITER.EQ.LAST) THEN
         DO 240 IUNIT=IU1,IU2   
            WRITE(IUNIT,250)
  250       FORMAT(//,'  I',8X,'TH(I)',6X,'LOCAL NU (TUBE WALL)')
  240    CONTINUE    
         DO 260 I=2,L2 
            ANULOC=FLUXM1(I,2)*DIA/(COND*(TW-TB))
            DO 270 IUNIT=IU1,IU2
            WRITE(IUNIT,280) I,X(I),ANULOC
  280       FORMAT(1X,I2,5X,1PE9.2,10X,1PE9.2)
  270       CONTINUE
  260    CONTINUE    
         DO 290 J=1,M1
         DO 290 I=1,L1
            W(I,J)=W(I,J)/WBAR
            IF(W(I,J).LT.1.E-10) W(I,J)=0.0
            T(I,J)=(T(I,J)-TW)/(TB-TW)
  290    CONTINUE
         CALL PRINT
C------
COME HERE TO FILL IBLOCK(I,J) BEFORE CALLING PLOT
         DO 295 J=2,M2
         DO 295 I=2,L2
            IF(X(I).LT.ALPHAT.AND.Y(J).GT.C) IBLOCK(I,J)=1
  295    CONTINUE
         CALL PLOT
C------
      ENDIF   
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PHI
      IF(NF.EQ.1) THEN
         DO 300 J=2,M2
         DO 300 I=2,L2
            GAM(I,J)=AMU
            IF(X(I).LT.ALPHAT.AND.Y(J).GT.C) GAM(I,J)=BIG
            SC(I,J)=-DPDZ 
  300    CONTINUE
      ENDIF  
      IF(NF.EQ.2) THEN
         DO 310 J=2,M2
         DO 310 I=2,L2
            GAM(I,J)=COND 
            IF(X(I).LT.ALPHAT.AND.Y(J).GT.C) GAM(I,J)=CNDFIN
            SC(I,J)=-RHOCP*DTDZ*W(I,J)
  310    CONTINUE
      ENDIF   
COME HERE TO SPECIFY BOUNDARY CONDITIONS
      DO 320 J=2,M2
         KBCI1(J)=2
         KBCL1(J)=2
  320 CONTINUE    
      DO 330 I=2,L2
         KBCJ1(I)=2
  330 CONTINUE    
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
