CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 14 -- POTENTIAL FLOW OVER A BLOCK
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION POT(NI,NJ),U(NI,NJ),V(NI,NJ)
      EQUIVALENCE (F(1,1,1),POT(1,1)),(F(1,1,2),U(1,1)),
     1    (F(1,1,3),V(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='POTENTIAL FLOW OVER A BLOCK'
      PRINTF='PRINT14'
      PLOTF='PLOT14'
      CALL INTA2(NCVLX,12,NCVLY,10)
      CALL DATA2(XL,9.,YL,4.)
      CALL EZGRID
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='     VEL. POT. '
      TITLE(2)='    U VELOCITY '
      TITLE(3)='    V VELOCITY '
      CALL INTA8(KSOLVE(1),1,KPRINT(1),1,KPLOT(1),1,
     1           KPRINT(2),1,KPLOT(2),1,KPRINT(3),1,KPLOT(3),1,LAST,3)
      DO 100 J=1,M1
      DO 100 I=1,L1
         POT(I,J)=-X(I)
  100 CONTINUE
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY OUTPUT
CALCULATE U AND V VELOCITIES AT THEIR STAGGERED LOCATIONS
      DO 200 J=3,M2
      DO 200 I=2,L2
         V(I,J)=-(POT(I,J)-POT(I,J-1))/(Y(J)-Y(J-1))
  200 CONTINUE
      DO 210 J=2,M2
      DO 220 I=3,L2
         U(I,J)=-(POT(I,J)-POT(I-1,J))/(X(I)-X(I-1))
  220 CONTINUE
      U(2,J)=FLUXI1(J,1)
      U(L1,J)=-FLUXL1(J,1)
  210 CONTINUE
      DO 230 J=2,M2
      DO 230 I=2,L2
         IF(X(I).GT.3..AND.X(I).LT.6..AND.Y(J).LT.2.) THEN
           U(I,J)=0.
           U(I+1,J)=0.
           V(I,J)=0.
           V(I,J+1)=0.
         ENDIF
  230 CONTINUE
      DO 240 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,250)
  250    FORMAT(1X,'ITER',2X,'PHI(3,5)',3X,'U(3,5)',4X,
     1   'V(3,5)',3X,'PHI(5,5)',3X,'U(5,5)',4X,'V(5,5)')
         WRITE(IUNIT,260) ITER,POT(3,5),U(3,5),V(3,5),
     1                         POT(5,5),U(5,5),V(5,5)
  260    FORMAT(1X,I2,2X,1P6E10.2)
  240 CONTINUE
      IF(ITER.EQ.LAST) THEN
        CALL PRINT
C------
COME HERE TO FILL IBLOCK(I,J) BEFORE CALLING PLOT
        DO 270 J=2,M2
        DO 270 I=2,L2
           IF(X(I).GT.3..AND.X(I).LT.6..AND.Y(J).LT.2.) IBLOCK(I,J)=0.
  270   CONTINUE
C------
        CALL PLOT
      ENDIF   
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PHI
      DO 300 J=2,M2
      DO 300 I=2,L2
         GAM(I,J)=1.
         IF(X(I).GT.3..AND.X(I).LT.6..AND.Y(J).LT.2.) GAM(I,J)=0.
  300 CONTINUE
COME HERE TO SPECIFY BOUNDARY CONDITIONS
      DO 310 I=2,L2
         KBCJ1(I)=2
         KBCM1(I)=2
  310 CONTINUE
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
