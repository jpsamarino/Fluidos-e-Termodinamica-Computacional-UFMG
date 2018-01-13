CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 4 -- CONDUCTION IN A COMPLEX CYLINDRICAL GEOMETRY 
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION T(NI,NJ)
      EQUIVALENCE (F(1,1,1),T(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='CONDUCTION IN A COMPLEX CYLINDRICAL GEOMETRY'
      PRINTF='PRINT4'
      PLOTF='PLOT4'
      MODE=2
      R(1)=0.5
      L1=14
      M1=11
      YV(2)=0.
      DY=1.5/FLOAT(M1-2)
      DO 10 J=3,M1
         YV(J)=YV(J-1)+DY
   10 CONTINUE
      XU(2)=0.
      DX=1./6.
      DO 20 I=3,8
         XU(I)=XU(I-1)+DX
   20 CONTINUE
CONSTRUCT CONTROL-VOLUME FACES TO MATCH CIRCULAR BOUNDARY
      DO 30 I=9,13
         J=19-I
         YM=0.5*(YV(J)+YV(J-1))
         XU(I)=2.-SQRT(1.-(YM-1.5)**2.)
   30 CONTINUE    
      XU(L1)=2.
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='   TEMPERATURE '
      CALL INTA4(KSOLVE(1),1,KPRINT(1),1,KPLOT(1),1,LAST,3)
      CALL DATA6(COND,2.,TW1,200.,TW2,100.,QW,1.,HE,5.,TINF,20.)
      DO 100 J=1,M1
      DO 100 I=1,L1
         T(I,J)=TW2
 100  CONTINUE
      DO 110 J=2,M2
         T(1,J)=TW1
 110  CONTINUE
      DO 120 I=2,L2
         T(I,1)=TW1
 120  CONTINUE
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY OUTPUT
      DO 200 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,210) 
  210    FORMAT(2X,'ITER',2X,'T(4,5)',4X,'T(6,10)',3X,'T(10,4)',
     1        3X,'T(12,4)')       
         WRITE(IUNIT,220) ITER,T(4,5),T(6,10),T(10,4),T(12,4)
  220    FORMAT(3X,I2,1P4E10.2)
  200 CONTINUE    
      IF(ITER.EQ.LAST) THEN
         CALL PRINT
C------
COME HERE TO FILL IBLOCK(I,J) BEFORE CALLING PLOT
         DO 230 J=2,M2
         DO 230 I=2,L2
            IF(X(I).LT.0.5.AND.Y(J).GT.1.0) IBLOCK(I,J)=1
            IF(X(I).LT.0.5.AND.Y(J).LT.0.5) IBLOCK(I,J)=1
            IF(X(I).GT.1.0.AND.Y(J).GT.0.5) IBLOCK(I,J)=1
  230    CONTINUE
         DO 240 J=5,9
         DO 240 I=8,17-J
            IBLOCK(I,J)=0
  240    CONTINUE
         CALL PLOT
C------
      ENDIF   
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PHI
      DO 300 J=2,M2
      DO 300 I=2,L2
         GAM(I,J)=COND
         IF(X(I).LT.0.5.AND.Y(J).GT.1.0) GAM(I,J)=0.
         IF(X(I).LT.0.5.AND.Y(J).LT.0.5) GAM(I,J)=0.
         IF(X(I).GT.1.0.AND.Y(J).GT.0.5) GAM(I,J)=BIG
         SC(I,J)=50.
         SP(I,J)=-4.
  300 CONTINUE
      DO 310 J=5,9
      DO 310 I=8,17-J
         GAM(I,J)=COND
  310 CONTINUE
COME HERE TO SPECIFY BOUNDARY CONDITIONS
      DO 320 J=8,M2
         SC(5,J)=SC(5,J)+QW/XCV(5)
  320 CONTINUE
      DO 330 J=2,4
         ABYV=1./XCV(5)
         RES=1./HE+0.5*XCV(5)/GAM(5,J)
         CONS=ABYV/RES
         SC(5,J)=SC(5,J)+CONS*TINF
         SP(5,J)=SP(5,J)-CONS
  330 CONTINUE
      DO 340 I=2,4
         GBYD=GAM(I,7)/(0.5*YCV(7))
         ABYV=RV(8)/YCVR(7)
         SC(I,7)=SC(I,7)+ABYV*GBYD*TW1
         SP(I,7)=SP(I,7)-ABYV*GBYD
         ABYV=RV(5)/YCVR(5)
         RES=1./HE+0.5*YCV(5)/GAM(I,5)
         CONS=ABYV/RES
         SC(I,5)=SC(I,5)+CONS*TINF
         SP(I,5)=SP(I,5)-CONS
  340 CONTINUE    
      DO 350 I=2,L2
         KBCM1(I)=2
 350  CONTINUE
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
