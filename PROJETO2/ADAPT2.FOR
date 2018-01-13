CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----TRABALHO FINAL--JOAO PEDRO SAMARINO
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION T(NI,NJ)
      EQUIVALENCE (F(1,1,1),T(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='STEADYYY CONDUCTION WITH MIXED BOUNDARY CONDITIONS'
      PRINTF='PRINTP2'
      PLOTF='PLOTP2'
      CALL DATA2(XL,11.,YL,6.)
      CALL INTA2(NCVLX,22,NCVLY,12)
      CALL EZGRID
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='   TEMPERATURE '
      CALL INTA5(KSOLVE(1),1,KPRINT(1),1,KPLOT(1),1,ITRMIN,3,LAST,15)
      CALL DATA6(AK1,15.,TW1,350.,TW2,435.,TINF,260.,HE,50.,HR,25.E-9) 
      DO 100 J=1,M1
      DO 100 I=1,L1
         T(I,J)=TW2
  100 CONTINUE   
	T(1,1)=260
	T(1,14)=380

      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY OUTPUT
      HTFLX=0.
      DO 200 J=2,M2
         HTFLX=HTFLX+ARX(J)*FLUXL1(J,1)
  200 CONTINUE    
      DO 210 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,220)
  220    FORMAT(2X,'ITER',3X,'T(3,3)',4X,'T(5,4)',4X,'T(10,7)'
     1   ,4X,'HEAT FLOW (FACE LESTE)')
         WRITE(IUNIT,230) ITER,T(3,3),T(5,4),T(10,7),HTFLX
  230    FORMAT(2X,I2,2X,1P3E10.2,8X,1PE11.3)
  210 CONTINUE
CREATE A CONVERGENCE CRITERION
      IF(ITER.LT.ITRMIN) RETURN
      DIFF=ABS((HTFLX-HTFLXO)/(HTFLX+SMALL))
      HTFLXO=HTFLX
      IF(DIFF.LE.1.E-5.OR.ITER.EQ.LAST) THEN
CALCULATE QUANTITIES FOR OVERALL HEAT BALANCE
      HTR=0.
      DO 231 J=2,M2
         HTR=HTR+ARX(J)*FLUXL1(J,1)
  231 CONTINUE
      HTL=HTFLX
      HTB=0
      HTT=0.
      DO 232 I=2,L2
         IF(X(I).GT.0.7) HTT=HTT+XCV(I)*FLUXM1(I,1)
  232 CONTINUE
      HTOUT=-(HTL+HTR+HTB+HTT)
      GEN=0.
      DO 233 J=2,M2
      DO 233 I=2,L2
         IF(X(I).GT.0.7.AND.Y(J).GT.0.25) GO TO 233
         GEN=GEN+(1000.-4.E-5*T(I,J)**3)*XCV(I)*YCV(J)
  233 CONTINUE
      HTBAL=HTOUT-GEN
CONSTRUCT FINAL PRINTOUT
         DO 240 IUNIT=IU1,IU2
            WRITE(IUNIT,250) 
  250       FORMAT(1X,/,'  J',8X,'Y(J)',5X,'FLUX(LEFT)',4X,
     1      'FLUX(RIGHT)')
            DO 260 J=M2,2,-1
               WRITE(IUNIT,270) J,Y(J),FLUXI1(J,1),FLUXL1(J,1)
  270          FORMAT(1X,I2,5X,1PE9.2,3X,1PE9.2,5X,1PE9.2)
  260       CONTINUE
            WRITE(IUNIT,280)
  280       FORMAT(1X,/,'  I',8X,'X(I)',5X,'FLUX(BOTTOM)',2X,
     1      'FLUX(TOP)')
            DO 290 I=2,L2
               WRITE(IUNIT,270) I,X(I),FLUXJ1(I,1),FLUXM1(I,1)
  290       CONTINUE
            WRITE(IUNIT,291) HTOUT,GEN,HTBAL
  291       FORMAT(/1X,'OVERALL HEAT BALANCE'/1X,20('*')/1X,
     1      'HEAT OUTFLOW',5X,'GENERATION',6X,'DIFFERENCE'/2X,
     2      1PE10.3,6X,1PE10.3,5X,1PE10.3)
  240    CONTINUE    
         CALL PRINT
         CALL PLOT
         KSTOP=1
      ENDIF
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PHI
      DO 300 J=2,M2
      DO 300 I=2,L2
          GAM(I,J)=AK1                   
         IF(X(I).GT.7.AND.X(I).LT.10.AND.Y(J).GT.3.AND.
     1   Y(J).LT.5) GAM(I,J)=1E12
         IF(X(I).GT.0.AND.X(I).LT.3.AND.Y(J).GT.0.AND.
     1   Y(J).LT.4) GAM(I,J)=0.
  300 CONTINUE
	SC(18,9)=BIG*TW1
      SP(18,9)=-BIG
COME HERE TO SPECIFY BOUNDARY CONDITIONS
      
	DO 310 I=2,L2
         IF(X(I).GT.0.AND.X(I).LT.3) THEN
		KBCJ1(I)=2
	    FLXCJ1(I)=HE*TINF
		FLXPJ1(I)=-HE
	   ELSE
	    KBCJ1(I)=1
	   ENDIF
		KBCM1(I)=2
  310 CONTINUE
      DO 320 J=2,M2
	   IF(Y(J).GT.0.AND.Y(J).LT.4) THEN
		KBCI1(J)=2
		FLXCI1(J)=HE*TINF
		FLXPI1(J)=-HE
	   ELSE
		KBCI1(J)=2
	
	   ENDIF
	   
	   KBCL1(J)=2
	   FLXCL1(J)=HE*TINF+HR*TINF**4+3.*HR*T(L2,J)**4
         FLXPL1(J)=-(HE+4.*HR*T(L2,J)**3)

  320 CONTINUE     
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC