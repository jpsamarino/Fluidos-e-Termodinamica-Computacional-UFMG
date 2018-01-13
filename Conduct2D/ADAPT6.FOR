CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 6 -- UNSTEADY CONDUCTION AROUND AN UNDERGROUND BUILDING 
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION T(NI,NJ)
      EQUIVALENCE (F(1,1,1),T(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='UNSTEADY CONDUCTION AROUND AN UNDERGROUND BUILDING'
      PRINTF='PRINT6'
      PI=3.14159
      CALL INTA3(NZX,2,NCVX(1),9,NCVX(2),3)
      CALL DATA2(XZONE(1),8.,XZONE(2),2.)
      CALL INTA3(NZY,2,NCVY(1),9,NCVY(2),3)
      CALL DATA2(YZONE(1),8.,YZONE(2),2.)
      CALL DATA2(POWRX(1),-1.5,POWRY(1),-1.5)
      CALL ZGRID
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='   TEMPERATURE '
      CALL INTA2(KSOLVE(1),1,LAST,120)
      CALL DATA7(COND,2.,RHOCP,3.,HE,5.,P,12.,DT,0.5,TGR,50.,TW,70.)
      DO 100 J=1,M1
      DO 100 I=1,L1
         T(I,J)=TGR
  100 CONTINUE
      DO 110 I=2,L2
         IF(X(I).GT.8.) T(I,M1)=TW
  110 CONTINUE
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY OUTPUT
      DO 200 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,210)
  210    FORMAT(1X,'ITER',3X,'TIME',5X,'T(2,14)',3X,'T(2,13)',3X,
     1       'T(2,12)',3X,'T(2,11)',3X,'T(2,10)')
         IF(MOD(ITER,3).EQ.0) THEN
            WRITE(IUNIT,240) ITER,TIME,T(2,14),T(2,13),T(2,12),T(2,11),
     1                       T(2,10)
  240       FORMAT(1X,I3,1P6E10.2)
            IF(MOD(ITER,24).EQ.0) WRITE(IUNIT,250)
  250       FORMAT(1X,69('*')) 
         ENDIF   
  200 CONTINUE       
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PHI
      DO 300 J=2,M2
      DO 300 I=2,L2
         ALAM(I,J)=RHOCP
         GAM(I,J)=COND
         IF(X(I).GT.8.0.AND.Y(J).GT.8.0) GAM(I,J)=BIG
  300 CONTINUE
COME HERE TO SPECIFY BOUNDARY CONDITIONS
      DO 310 J=2,M2
         KBCI1(J)=2
         KBCL1(J)=2
  310 CONTINUE    
      ARGU=2.*PI*(TIME+DT)/P
      TAIR=50.+50.*SIN(ARGU)
      DO 320 I=2,L2
         IF(X(I).LT.8.) THEN
            KBCM1(I)=2
            FLXCM1(I)=HE*TAIR
            FLXPM1(I)=-HE
         ENDIF
  320 CONTINUE    
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
