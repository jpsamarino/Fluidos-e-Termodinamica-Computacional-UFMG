CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 3 -- STEADY CONDUCTION IN IRREGULAR GEOMETRY  
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION T(NI,NJ)
      EQUIVALENCE (F(1,1,1),T(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='STEADY CONDUCTION IN IRREGULAR GEOMETRY'
      PRINTF='PRINT3'
      PLOTF='PLOT3'  
      CALL INTA5(NZX,4,NCVX(1),2,NCVX(2),4,NCVX(3),2,NCVX(4),4)
	CALL DATA4(XZONE(1),0.5,XZONE(2),0.9,XZONE(3),0.6,XZONE(4),1.)
	CALL INTA6(NZY,5,NCVY(1),2,NCVY(2),2,NCVY(3),2,NCVY(4),4,
	1NCVY(5),2)
	CALL DATA5(YZONE(1),0.4,YZONE(2),0.4,YZONE(3),0.6,YZONE(4),1.0,
	1YZONE(5),0.6)
      CALL ZGRID
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='   TEMPERATURE '
      CALL INTA4(KSOLVE(1),1,KPRINT(1),1,KPLOT(1),1,LAST,10)
	CALL DATA6(TW1,400.,TW2,500.,TINF,300.,AQ,50.,BQ,2.E-8,AK,12.)
C      READ(3,50)TW1,TW2,TINF,AQ,BQ,AK
C 50   FORMAT(///,4(/53X,F6.1),/53X,D5.0,/53X,F6.1)
      DO 100 J=1,M1
      DO 100 I=1,L1
         T(I,J)=TW2
  100 CONTINUE   
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY OUTPUT
      HTFLY=0.
      DO 200 I=2,L2
         HTFLY=HTFLY+XCV(I)*FLUXJ1(I,1)
  200 CONTINUE    
      DO 210 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,220)
  220    FORMAT(2X,'ITER',3X,'T(2,12)',3X,'T(5,4)',3X,'T(10,12)'
     1   ,4X,'HEAT FLOW (BOTTOM FACE)')
         WRITE(IUNIT,230) ITER,T(2,12),T(5,4),T(10,12),HTFLY
  230    FORMAT(2X,I2,2X,1P3E10.2,9X,1PE11.3)
  210 CONTINUE
      IF(ITER.EQ.LAST) THEN
         CALL PRINT
C------
COME HERE TO FILL IBLOCK(I,J) BEFORE CALLING PLOT
         DO 240 J=2,M2
         DO 240 I=2,L2
            IF(X(I).GT.0.5.AND.X(I).LT.1.4.AND.Y(J).GT.0.8.AND.
     1         Y(J).LT.2.4) IBLOCK(I,J)=1
            IF(X(I).GT.2..AND.Y(J).GT.0.4.AND.Y(J).LT.1.4) IBLOCK(I,J)=1
  240    CONTINUE
         CALL PLOT
C------
      ENDIF
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PHI
      DO 300 J=2,M2
      DO 300 I=2,L2
         GAM(I,J)=AK                   
         IF(X(I).GT.0.5.AND.X(I).LT.1.4.AND.Y(J).GT.0.8.AND.
     1   Y(J).LT.2.4) GAM(I,J)=1.E12
         IF(X(I).GT.2.0.AND.Y(J).GT.0.4.AND.Y(J).LT.1.4) GAM(I,J)=1.E12
  300 CONTINUE
      SC(4,6)=BIG*TW1
      SP(4,6)=-BIG
COME HERE TO SPECIFY BOUNDARY CONDITIONS
      DO 310 I=2,L2
         KBCM1(I)=2
         KBCJ1(I)=2
         FLXCJ1(I)=AQ*TINF+BQ*TINF**4+3.*BQ*T(I,1)**4
         FLXPJ1(I)=-(AQ+4.*BQ*T(I,1)**3)
  310 CONTINUE    
      DO 320 J=2,M2
         KBCI1(J)=2
  320 CONTINUE    
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
