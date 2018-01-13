CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 12 -- DUCT FLOW WITH TEMPERATURE-DEPENDENT VISCOSITY
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION W(NI,NJ),T(NI,NJ)
      EQUIVALENCE (F(1,1,1),W(1,1)),(F(1,1,2),T(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='DUCT FLOW WITH TEMPERATURE-DEPENDENT VISCOSITY'
      PRINTF='PRINT12'
      PLOTF='PLOT12'
      CALL DATA4(ROUT,1.,RIN,0.2,ALPHA,60.,PI,3.14159)
      XL=0.5*ALPHA*PI/180.
      CALL DATA2(R(1),RIN,YL,ROUT-RIN)
      CALL INTA3(NCVLX,5,NCVLY,10,MODE,3)
      CALL EZGRID
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='      W/WBAR    ' 
      TITLE(2)='  (T-T2)/(T1-T2)'
      CALL INTA6(KSOLVE(2),1,KPRINT(1),1,KPRINT(2),1,
     1           KPLOT(1),1,KPLOT(2),1,LAST,6)
      CALL DATA8(AMU,1.,BMU,0.04,COND,1.,CP,1.,DEN,1.,DPDZ,-1.,T1,100.,
     1           T2,0.)
      RHOCP=DEN*CP
      DO 100 J=1,M1
      DO 100 I=1,L1
         W(I,J)=0.
         T(I,J)=T1
         T(I,M1)=T2
  100 CONTINUE
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY OUTPUT
      IF(ITER.EQ.3) THEN
         KSOLVE(1)=1
         KSOLVE(2)=0
      ENDIF
      ASUM=0.
      WSUM=0.
      DO 200 J=2,M2
      DO 200 I=2,L2
         AR=XCV(I)*YCVR(J)
         ASUM=ASUM+AR
         WSUM=WSUM+W(I,J)*AR
  200 CONTINUE
      WBAR=WSUM/ASUM
      WP=XL*(ROUT+RIN)+YL
      DH=4.*ASUM/WP 
      RE=DH*WBAR*DEN/AMU
      FRE=-2.*DPDZ*DH/(DEN*WBAR**2+SMALL)*RE
      DO 210 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,220)
  220    FORMAT(/,1X,'ITER',2X,'W(6,8)',5X,'W(4,11)',4X,'T(6,8)'
     1   ,5X,'T(4,11)',6X,'FRE')
         WRITE(IUNIT,230) ITER,W(6,8),W(4,11),T(6,8),T(4,11),FRE
  230    FORMAT(2X,I2,1P6E11.3)
  210 CONTINUE
      IF(ITER.EQ.LAST) THEN
         DO 240 J=1,M1
         DO 240 I=1,L1
            W(I,J)=W(I,J)/WBAR
            T(I,J)=(T(I,J)-T2)/(T1-T2)
  240    CONTINUE
         CALL PRINT
         CALL PLOT
      ENDIF   
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PHI
      IF(NF.EQ.1) THEN
         DO 300 J=2,M2
         DO 300 I=2,L2
            GAM(I,J)=AMU+BMU*T(I,J)
            SC(I,J)=-DPDZ 
  300    CONTINUE
      ENDIF
      IF(NF.EQ.2) THEN
         DO 310 J=2,M2
         DO 310 I=2,L2
            GAM(I,J)=COND 
  310    CONTINUE       
      ENDIF
COME HERE TO SPECIFY BOUNDARY CONDITIONS
      DO 320 J=2,M2
         KBCL1(J)=2
  320 CONTINUE
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
