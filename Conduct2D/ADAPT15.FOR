CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 15 -- WATER SEEPAGE UNDER A DAM
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION P(NI,NJ),U(NI,NJ),V(NI,NJ)
      EQUIVALENCE (F(1,1,1),P(1,1)),(F(1,1,2),U(1,1)),
     1    (F(1,1,3),V(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='WATER SEEPAGE UNDER A DAM'
      PRINTF='PRINT15'
      PLOTF='PLOT15'
      CALL INTA4(NZX,3,NCVX(1),5,NCVX(2),2,NCVX(3),5)
      CALL DATA5(XZONE(1),3.,XZONE(2),.5,XZONE(3),3.,POWRX(1),-1.2,
     1           POWRX(3),1.2)
      CALL INTA3(NZY,2,NCVY(1),6,NCVY(2),3)
      CALL DATA2(YZONE(1),3.,YZONE(2),1.)
      CALL ZGRID
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='     PRESSURE  '
      TITLE(2)='    U VELOCITY '
      TITLE(3)='    V VELOCITY '
      CALL INTA8(KSOLVE(1),1,KPRINT(1),1,KPLOT(1),1,
     1           KPRINT(2),1,KPLOT(2),1,KPRINT(3),1,KPLOT(3),1,LAST,3)
      DC=1.
      DO 100 J=1,M1
      DO 100 I=1,L1
         P(I,J)=0.	
  100 CONTINUE
      DO 110 I=2,L2
         IF(X(I).LT.3.) P(I,M1)=100.
  110 CONTINUE
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY OUTPUT
CALCULATE U AND V VELOCITIES AT THEIR STAGGERED LOCATIONS
      DO 200 J=3,M2
      DO 200 I=2,L2
         V(I,J)=-DC*(P(I,J)-P(I,J-1))/(Y(J)-Y(J-1))
  200 CONTINUE
      DO 210 I=2,L2
         V(I,M1)=-FLUXM1(I,1)
  210 CONTINUE
      DO 220 J=2,M2
      DO 220 I=3,L2
         U(I,J)=-DC*(P(I,J)-P(I-1,J))/(X(I)-X(I-1))
  220 CONTINUE
      DO 230 J=2,M2
      DO 230 I=2,L2
         IF(X(I).GT.3..AND.X(I).LT.3.5.AND.Y(J).GT.3.) THEN
           U(I,J)=0.
           U(I+1,J)=0.
           V(I,J)=0.
           V(I,J+1)=0.
         ENDIF
  230 CONTINUE
      DO 240 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,250)
  250    FORMAT(1X,'ITER',2X,'P(3,5)',5X,'U(3,5)',5X,
     1   'V(3,5)',5X,'P(5,5)',5X,'U(5,5)',5X,'V(5,5)')
         WRITE(IUNIT,260) ITER,P(3,5),U(3,5),V(3,5),
     1                         P(5,5),U(5,5),V(5,5)
  260    FORMAT(1X,I2,1P6E11.2)
  240 CONTINUE
      IF(ITER.EQ.LAST) THEN
        CALL PRINT
C------
COME HERE TO FILL IBLOCK(I,J) BEFORE CALLING PLOT
        DO 270 J=2,M2
        DO 270 I=2,L2
           IF(X(I).GT.3..AND.X(I).LT.3.5.AND.Y(J).GT.3.) IBLOCK(I,J)=1
  270   CONTINUE
        CALL PLOT
C------
      ENDIF   
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PHI
      DO 300 J=2,M2
      DO 300 I=2,L2
         GAM(I,J)=DC
         IF(X(I).GT.3..AND.X(I).LT.3.5.AND.Y(J).GT.3.) GAM(I,J)=0.
  300 CONTINUE
COME HERE TO SPECIFY BOUNDARY CONDITIONS
      DO 310 I=2,L2
         KBCJ1(I)=2
  310 CONTINUE
      DO 320 J=2,M2
         KBCI1(J)=2
         KBCL1(J)=2
  320 CONTINUE    
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
