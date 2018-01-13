CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 9 -- ANNULAR SECTOR DUCT                              
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION W(NI,NJ),T(NI,NJ)
      EQUIVALENCE (F(1,1,1),W(1,1)),(F(1,1,2),T(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='ANNULAR SECTOR DUCT'
      PRINTF='PRINT9'
      PLOTF='PLOT9'
      CALL DATA4(ROUT,1.,RIN,0.2,ALPHA,60.,PI,3.14159)
      XL=0.5*ALPHA*PI/180.
      CALL DATA2(R(1),RIN,YL,ROUT-RIN)
      CALL INTA3(NCVLX,5,NCVLY,10,MODE,3)
      CALL EZGRID
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='      W/WBAR    ' 
      TITLE(2)='  (T-TW)/(TB-TW)'
      CALL INTA6(KSOLVE(1),1,KPRINT(1),1,KPRINT(2),1,
     1           KPLOT(1),1,KPLOT(2),1,LAST,10)
      CALL DATA7(AMU,1.,COND,1.,CP,1.,DEN,1.,DPDZ,-1.,TW,0.,DTBDZ,1.)
      RHOCP=DEN*CP
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
         ASUM=ASUM+AR
         WSUM=WSUM+W(I,J)*AR
         TSUM=TSUM+W(I,J)*T(I,J)*AR
  200 CONTINUE
      WBAR=WSUM/ASUM
      TB=TSUM/(WSUM+SMALL)
      WP=XL*(ROUT+RIN)+YL
      HP=XL*RIN+YL
      DH=4.*ASUM/WP 
      RE=DH*WBAR*DEN/AMU
      FRE=-2.*DPDZ*DH/(DEN*WBAR**2+SMALL)*RE
      QW=DTBDZ*RHOCP*WSUM/HP
      ANU=QW*YL/(COND*(TW-TB)+SMALL)
      DO 210 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,220)
  220    FORMAT(1X,'ITER',2X,'W(6,8)',4X,'W(4,11)',3X,'T(6,8)'
     1   ,4X,'T(4,11)',4X,'FRE',8X,'NU')
         WRITE(IUNIT,230) ITER,W(6,8),W(4,11),T(6,8),T(4,11),FRE,ANU
  230    FORMAT(2X,I2,1P7E10.2)
  210 CONTINUE
      IF(ITER.EQ.LAST) THEN
COME HERE TO CALCULATE LOCAL NU
         DO 240 I=2,L2
            ANUI=FLUXJ1(I,2)*YL/(COND*(TW-TB)+SMALL)
            DO 250 IUNIT=IU1,IU2
               IF(I.EQ.2) WRITE(IUNIT,260) 
  260          FORMAT(//,'  I',8X,'TH(I)',6X,'LOCAL NU (INNER WALL)') 
               WRITE(IUNIT,265) I,X(I),ANUI
  265          FORMAT(1X,I2,5X,1PE9.2,10X,1PE9.2)
  250       CONTINUE
  240    CONTINUE
         DO 270 J=M2,2,-1
            ANULFT=FLUXI1(J,2)*YL/(COND*(TW-TB)+SMALL)
            DO 275 IUNIT=IU1,IU2
               IF(J.EQ.M2) WRITE(IUNIT,280) 
  280          FORMAT(//,'  J',8X,'Y(J)',7X,'LOCAL NU (SIDE WALL)')
               WRITE(IUNIT,285) J,Y(J),ANULFT
  285          FORMAT(1X,I2,5X,1PE9.2,10X,1PE9.2)
  275       CONTINUE
  270    CONTINUE
         DO 290 J=1,M1
         DO 290 I=1,L1
            W(I,J)=W(I,J)/WBAR
            T(I,J)=(T(I,J)-TW)/(TB-TW)
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
         DO 310 J=2,M2
         DO 310 I=2,L2
            GAM(I,J)=COND 
            TEMP=(T(I,J)-TW+SMALL)/(TB-TW+SMALL)
            DTDZ=TEMP*DTBDZ
            SC(I,J)=-RHOCP*DTDZ*W(I,J)
  310    CONTINUE       
      ENDIF
COME HERE TO SPECIFY BOUNDARY CONDITIONS
         DO 320 J=2,M2
            KBCL1(J)=2
  320    CONTINUE
      IF(NF.EQ.2) THEN
         DO 330 I=2,L2
            KBCM1(I)=2
  330    CONTINUE
      ENDIF
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
