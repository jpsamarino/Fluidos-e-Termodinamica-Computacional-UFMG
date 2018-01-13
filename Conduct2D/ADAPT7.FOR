CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 7 -- RECTANGULAR DUCT WITH ONE HEATED WALL
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION W(NI,NJ),T(NI,NJ)
      EQUIVALENCE (F(1,1,1),W(1,1)),(F(1,1,2),T(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='RECTANGULAR DUCT WITH ONE HEATED WALL'
      PRINTF='PRINT7'
      PLOTF='PLOT7'
      CALL INTA2(NZX,1,NCVX(1),5)
      CALL DATA2(XZONE(1),1.,POWRX(1),1.2)
      CALL INTA3(NZY,2,NCVY(1),5,NCVY(2),5)
      CALL DATA4(YZONE(1),0.5,YZONE(2),0.5,POWRY(1),1.2,POWRY(2),-1.2)
      CALL ZGRID
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)=' AXIAL VELOCITY W '
      TITLE(2)='   TEMPERATURE    '
      TITLE(3)='      W/WBAR      '
      TITLE(4)='(T-TWAV)/(TB-TWAV)'
      CALL INTA4(KSOLVE(1),1,KPLOT(3),1,KPLOT(4),1,LAST,6)
      DO 100 N=1,4
  100 KPRINT(N)=1
      CALL DATA6(AMU,1.,COND,1.,CP,1.,DEN,1.,DPDZ,-1.,QW,1.)
      RHOCP=DEN*CP
C
C-- SINCE THE ZERO DEFAULT VALUES OF W(I,J) AND T(I,J) ARE SATISFACTORY,
C   THESE ARRAYS ARE NOT FILLED HERE.
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
         AR=XCV(I)*YCV(J)
         ASUM=ASUM+AR
         WSUM=WSUM+W(I,J)*AR
         TSUM=TSUM+W(I,J)*T(I,J)*AR
  200 CONTINUE
      WBAR=WSUM/ASUM
      TB=TSUM/(WSUM+SMALL) 
      WP=2.*X(L1)+Y(M1)
      DH=4.*ASUM/WP 
      RE=DH*WBAR*DEN/AMU
      FRE=-2.*DPDZ*DH/(DEN*WBAR**2+SMALL)*RE
      TWAV=0.
      DO 210 I=2,L2
         TWAV=TWAV+XCV(I)*T(I,1)
  210 CONTINUE    
      TWAV=TWAV/X(L1)
      ANU=QW*DH/(COND*(TWAV-TB)+SMALL)
      DO 220 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,230)
  230    FORMAT(2X,'ITER',4X,'W(5,8)',4X,'W(3,7)',4X,'T(5,8)',
     1   4X,'T(3,7)',5X,'FRE',8X,'NU')
         WRITE(IUNIT,240) ITER,W(5,8),W(3,7),T(5,8),T(3,7),FRE,ANU
  240    FORMAT(3X,I2,2X,1P6E10.2)
  220 CONTINUE    
      IF(ITER.EQ.LAST) THEN
COME HERE TO CALCULATE LOCAL NU ON BOTTOM WALL
         DO 250 I=2,L2
            ANULOC=QW*DH/(COND*(T(I,1)-TB)+SMALL)
            DO 260 IUNIT=IU1,IU2
               IF(I.EQ.2) WRITE(IUNIT,270)
  270          FORMAT(//,'  I',8X,'X(I)',7X,'LOCAL NU (BOTTOM WALL)')
               WRITE(IUNIT,280) I,X(I),ANULOC
  280          FORMAT(1X,I2,5X,1PE9.2,11X,1PE9.2) 
  260       CONTINUE    
  250    CONTINUE    
         DO 290 J=1,M1
         DO 290 I=1,L1
            F(I,J,3)=W(I,J)/WBAR
            F(I,J,4)=(T(I,J)-TWAV)/(TB-TWAV)
  290    CONTINUE
         CALL PRINT
         CALL PLOT
      ENDIF
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PHI
      IF(NF.EQ.1) THEN
         DO 300 J=2,M2
         DO 300 I=2,L2
            GAM(I,J)=AMU
            SC(I,J)=-DPDZ
  300    CONTINUE
      ENDIF
      IF(NF.EQ.2) THEN
         DTDZ=QW*X(L1)/(WSUM*RHOCP)
         DO 320 J=2,M2
         DO 320 I=2,L2
            GAM(I,J)=COND
            SC(I,J)=-RHOCP*DTDZ*W(I,J)
  320    CONTINUE    
      ENDIF  
COME HERE TO SPECIFY BOUNDARY CONDITIONS
         DO 310 J=2,M2
            KBCL1(J)=2
  310    CONTINUE    
      IF(NF.EQ.2) THEN
         DO 330 J=2,M2
            KBCI1(J)=2
  330    CONTINUE    
         DO 340 I=2,L2
            KBCJ1(I)=2
            FLXCJ1(I)=QW
            KBCM1(I)=2
  340    CONTINUE
      ENDIF   
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
