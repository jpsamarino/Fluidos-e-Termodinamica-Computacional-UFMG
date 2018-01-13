CcCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 5 -- UNSTEADY CONDUCTION WITH HEAT GENERATION
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION T(NI,NJ)
      EQUIVALENCE (F(1,1,1),T(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='UNSTEADY CONDUCTION WITH HEAT GENERATION'
      PRINTF='PRINT5'
      PLOTF='PLOT5'
      MODE=3
      PI=3.14159
      CALL INTA5(NZX,1,NCVX(1),10,NZY,2,NCVY(1),3,NCVY(2),9)
      CALL DATA4(XZONE(1),0.5*PI,POWRX(1),1.2,YZONE(1),0.25,
     1    YZONE(2),0.75)
      CALL ZGRID
      R(1)=0.5
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='   TEMPERATURE '
      CALL INTA4(KSOLVE(1),1,KPRINT(1),1,KPLOT(1),1,LAST,35)
      CALL DATA6(COND,1.,RHOCP,1.,SOURCE,1000.,HE,5.,QB,60.,DT,0.001)
      CALL DATA3(TINF,20.,TZERO,50.,TI,100.)
      DO 100 J=1,M1
      DO 100 I=1,L1
         T(I,J)=TZERO
  100 CONTINUE   
      DO 110 I=2,L2
         T(I,1)=TI
  110 CONTINUE    
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY OUTPUT
         QBTM=0.
         QTOP=0.
         DO 200 I=2,L2
            QBTM=QBTM+XCV(I)*RV(2)*FLUXJ1(I,1)
            QTOP=QTOP+XCV(I)*RV(M1)*FLUXM1(I,1) 
  200    CONTINUE
      DO 210 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,220)
  220    FORMAT(1X,'ITER',3X,'TIME',4X,'T(4,2)',3X,'T(10,4)',
     1   2X,'T(7,10)',1X,'T(10,10)',4X,'QBTM',6X,'QTOP')
         WRITE(IUNIT,230) ITER,TIME,T(4,2),T(10,4),
     1   T(7,10),T(10,10),QBTM,QTOP
  230    FORMAT(2X,I2,1X,1P5E9.2,1P2E10.2)
  210 CONTINUE 
     	DT=1.2*DT
      IF(ITER.EQ.LAST) THEN
         CALL PRINT
         CALL PLOT
      ENDIF 
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PHI
      DO 300 J=2,M2
      DO 300 I=2,L2
         ALAM(I,J)=RHOCP
         GAM(I,J)=COND
         IF(Y(J).LT.0.25) SC(I,J)=SOURCE
  300 CONTINUE 
COME HERE TO SPECIFY BOUNDARY CONDITIONS
      DO 310 J=2,M2
         KBCI1(J)=2
         KBCL1(J)=2
         FLXCI1(J)=QB
  310 CONTINUE
      DO 320 I=2,L2
         KBCM1(I)=2
         FLXCM1(I)=HE*TINF
         FLXPM1(I)=-HE
  320 CONTINUE
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
