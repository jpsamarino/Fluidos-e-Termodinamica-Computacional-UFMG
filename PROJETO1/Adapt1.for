CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 1 -- STEADY CONDUCTION WITH HEAT GENERATION 
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION T(NI,NJ)
      EQUIVALENCE (F(1,1,1),T(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='STEADY CONDUCTION WITH HEAT GENERATION'
      PRINTF='PRINT1'
      PLOTF='PLOT1'
      CALL DATA2(XL,2.,YL,2.)
      CALL INTA2(NCVLX,3,NCVLY,3)
	CALL EZGRID              
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='   TEMPERATURE '
      CALL INTA4(KSOLVE(1),1,KPRINT(1),1,KPLOT(1),1,LAST,15)
      CALL DATA3(TW,0.,COND,1.,SOURCE,5.)
      DO 5 J=1,M1
      DO 5 I=1,L1
         T(I,J)=TW
	T(2,1)=10

  5	CONTINUE   
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY OUTPUT
      DO 200 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,210)
  210    FORMAT(2X,'ITER',3X,'T(2,2)',4X,'T(4,2)',4X,
     1   'T(6,3)')
         WRITE(IUNIT,220) ITER,T(2,2),T(4,2),T(6,3) 
  220    FORMAT(2X,I2,2X,1P3E10.2)
  200 CONTINUE
      IF(ITER.EQ.LAST) THEN
         CALL PRINT
         CALL PLOT
      ENDIF
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PHI
      DO 300 J=2,M2
	DO 300 I=2,L2 
           GAM(I,J)=COND
	     SC(I,J)=SOURCE
	GAM(2,2)=0.00000001
300	CONTINUE
	RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
