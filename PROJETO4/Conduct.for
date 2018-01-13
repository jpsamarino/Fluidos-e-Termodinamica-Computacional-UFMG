CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PROGRAM CONDUCT
COMPUTER PROGRAM 'CONDUCT' BY DR. SUHAS V. PATANKAR
COPYRIGHT (C) 1991 INNOVATIVE RESEARCH, INC.
C***********************************************************************
$INCLUDE:'COMMON'
C***********************************************************************
CALCULATIONS IN THE GETTING-READY PHASE 
      CALL DEFLT
      CALL GRID
      CALL READY
      CALL BEGIN
   10 CONTINUE
COME HERE TO START THE ITERATION OR TIME-STEP LOOP  
      CALL OUTPUT
      IF(KSTOP.NE.0) STOP
      CALL HEART
      GO TO 10
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE DEFRD
C***********************************************************************
$INCLUDE:'COMMON'
C***********************************************************************
C
      ENTRY DEFLT
C
COME HERE TO SET THE DEFAULT VALUES
C
      HEADER='USE THE CHARACTER VARIABLE HEADER TO SPECIFY A PROBLEM TIT
     1LE'
      PRINTF='PRINT1'
      PLOTF='PLOT1'    
      CALL INTA7(KSTOP,0,LAST,5,ITER,0,KORD,2,MODE,1,KPGR,1,KOUT,3)
      CALL DATA5(SMALL,1.E-20,BIG,1.E+20,TIME,0.,DT,1.E+20,R(1),0.)
      CALL DATA2(POWERX,1.,POWERY,1.)
      DO 10 NZ=1,NZMAX
      POWRX(NZ)=1.
   10 POWRY(NZ)=1. 
      DO 20 N=1,NFMAX
      CRIT(N)=1.E-5
      KSOLVE(N)=0
      NTIMES(N)=10 
      KBLOC(N)=1
      RELAX(N)=1.
      TITLE(N)='                  '
      KPRINT(N)=0
      KPLOT(N)=0
      DO 30 I=2,NI
      FLUXJ1(I,N)=0.
   30 FLUXM1(I,N)=0.
      DO 40 J=2,NJ
      FLUXI1(J,N)=0.
   40 FLUXL1(J,N)=0.
   20 CONTINUE
      DO 50 J=1,NJ
      DO 50 I=1,NI
      CON(I,J)=0.
      AP(I,J)=0.
      ALAM(I,J)=1. 
      GAM(I,J)=1.
      IBLOCK(I,J)=0
      DO 60 N=1,NFMAX
   60 F(I,J,N)=0.     
   50 CONTINUE
      DO 70 I=2,NI
      KBCJ1(I)=1
      KBCM1(I)=1
      FLXCJ1(I)=0.
      FLXCM1(I)=0.
      FLXPJ1(I)=0.
      FLXPM1(I)=0.
   70 CONTINUE
      DO 80 J=2,NJ
      KBCI1(J)=1
      KBCL1(J)=1
      FLXCI1(J)=0.
      FLXCL1(J)=0.
      FLXPI1(J)=0.
      FLXPL1(J)=0.
   80 CONTINUE
C
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY READY
C
      IF(KOUT.NE.1) OPEN(UNIT=7,FILE=PRINTF)
      IU1=6
      IF(KOUT.EQ.2) IU1=7
      IU2=7
      IF(KOUT.EQ.1) IU2=6
CREATE INITIAL OUTPUT
      DO 100 IUNIT=IU1,IU2 
C
      IF(MODE.EQ.1) WRITE(IUNIT,1)
    1 FORMAT(1X,'RESULTS OF CONDUCT FOR CARTESIAN COORDINATE SYSTEM'
     1/1X,50(1H*)//)
      IF(MODE.EQ.2) WRITE(IUNIT,2)
    2 FORMAT(1X,'RESULTS OF CONDUCT FOR AXISYMMETRIC COORDINATE SYSTEM'
     1/1X,53(1H*)//)
      IF(MODE.EQ.3) WRITE(IUNIT,3)
    3 FORMAT(1X,'RESULTS OF CONDUCT FOR POLAR COORDINATE SYSTEM'
     1/1X,46(1H*)//)
      WRITE(IUNIT,5) HEADER
    5 FORMAT(1X,64('-')/1X,A64/1X,64('-')//)
      IF(L1.GT.NI.OR.M1.GT.NJ.OR.L1.LT.4.OR.M1.LT.4) THEN
      WRITE(IUNIT,6) 
    6 FORMAT(1X,'EXECUTION TERMINATED DUE TO ONE(OR MORE) OF THE FOLLOWI
     1NG REASON(S)'/2X,'1) L1 GREATER THAN NI'/2X,'2) M1 GREATER THAN NJ
     2'/2X,'3) L1 LESS THAN 4'/2X,'4) M1 LESS THAN 4'/)
      KSTOP=1
      ENDIF      
  100 CONTINUE 
      IF(KSTOP.NE.0) STOP
CALCULATE GEOMETRICAL QUANTITIES
      L2=L1-1
      L3=L2-1
      M2=M1-1
      M3=M2-1
      X(1)=XU(2)
      DO 110 I=2,L2
  110 X(I)=0.5*(XU(I+1)+XU(I))
      X(L1)=XU(L1)
      Y(1)=YV(2)
      DO 120 J=2,M2
  120 Y(J)=0.5*(YV(J+1)+YV(J))
      Y(M1)=YV(M1)
      DO 130 I=2,L2
  130 XCV(I)=XU(I+1)-XU(I)
      DO 140 J=2,M2
  140 YCV(J)=YV(J+1)-YV(J)
      IF(MODE.EQ.1) THEN
      DO 150 J=1,M1
      RV(J)=1
  150 R(J)=1
      ELSE
      RY1=R(1)-Y(1) 
      DO 160 J=2,M1
  160 R(J)=Y(J)+RY1
      RV(2)=R(1)
      DO 170 J=3,M2
  170 RV(J)=RV(J-1)+YCV(J-1)
      RV(M1)=R(M1)
      ENDIF
      IF(MODE.EQ.3) THEN
      DO 180 J=1,M1
  180 SX(J)=R(J)
      ELSE
      DO 190 J=1,M1
      SX(J)=1.
  190 CONTINUE
      ENDIF
      DO 200 J=2,M2
      YCVR(J)=R(J)*YCV(J)
      IF(MODE.EQ.3) THEN
      ARX(J)=YCV(J)
      ELSE
      ARX(J)=YCVR(J) 
      ENDIF
  200 CONTINUE
C
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE HEART
C***********************************************************************
$INCLUDE:'COMMON'
C***********************************************************************
CONSTRUCT LOOP FOR ALL EQUATIONS
      DO 999 N=1,NFMAX
      NF=N  
      IF(KSOLVE(NF).EQ.0) GO TO 999
C
      CALL PHI
C
CALCULATE COEFFICIENTS IN THE DISCRETIZATION EQUATION
C
      BETA=4./3.
      IF(KORD.EQ.1) BETA=1.
      RLX=(1.-RELAX(NF))/RELAX(NF)
CONSIDER VOLUMETRIC TERMS
      DO 10 J=2,M2 
      DO 10 I=2,L2 
      VOL=YCVR(J)*XCV(I)
      APT=ALAM(I,J)/DT
      CON(I,J)=(CON(I,J)+APT*F(I,J,NF))*VOL
      AP(I,J)=(APT-AP(I,J))*VOL
   10 CONTINUE
COEFFICIENTS FOR X-DIRECTION DIFFUSION
      DO 20 J=2,M2
      DO 20 I=2,L3
      DIFF=ARX(J)*2.*GAM(I,J)*GAM(I+1,J)/((XCV(I)*GAM(I+1,J)+
     1 XCV(I+1)*GAM(I,J)+SMALL)*SX(J))
      AIP(I,J)=DIFF+SMALL
      AIM(I+1,J)=AIP(I,J)
   20 CONTINUE
      DO 30 J=2,M2
CONSIDER I=1 BOUNDARY
      DIFF=GAM(2,J)/(0.5*XCV(2)*SX(J))+SMALL
      AIM(2,J)=BETA*DIFF
      AIP(1,J)=AIM(2,J)
      AIM(2,J)=AIM(2,J)*ARX(J)
      AIM(1,J)=(BETA-1.)*AIP(2,J)/ARX(J)
      AIP(2,J)=AIP(2,J)+AIM(1,J)*ARX(J) 
      IF(KBCI1(J).EQ.1) THEN
      CON(2,J)=CON(2,J)+AIM(2,J)*F(1,J,NF)
      ELSE
      AP(1,J)=AIP(1,J)-FLXPI1(J)
      CON(1,J)=FLXCI1(J)  
      TEMP=AIM(2,J)/AP(1,J)
      AP(2,J)=AP(2,J)-AIP(1,J)*TEMP
      AIP(2,J)=AIP(2,J)-AIM(1,J)*TEMP
      CON(2,J)=CON(2,J)+CON(1,J)*TEMP
      ENDIF
      AP(2,J)=AP(2,J)+AIM(2,J)
      AIM(2,J)=0.
CONSIDER I=L1 BOUNDARY
      DIFF=GAM(L2,J)/(0.5*XCV(L2)*SX(J))+SMALL
      AIP(L2,J)=BETA*DIFF
      AIM(L1,J)=AIP(L2,J)
      AIP(L2,J)=AIP(L2,J)*ARX(J)
      AIP(L1,J)=(BETA-1.)*AIM(L2,J)/ARX(J)
      AIM(L2,J)=AIM(L2,J)+AIP(L1,J)*ARX(J)
      IF(KBCL1(J).EQ.1) THEN
      CON(L2,J)=CON(L2,J)+AIP(L2,J)*F(L1,J,NF)
      ELSE
      AP(L1,J)=AIM(L1,J)-FLXPL1(J)
      CON(L1,J)=FLXCL1(J)
      TEMP=AIP(L2,J)/AP(L1,J) 
      AP(L2,J)=AP(L2,J)-AIM(L1,J)*TEMP
      AIM(L2,J)=AIM(L2,J)-AIP(L1,J)*TEMP
      CON(L2,J)=CON(L2,J)+CON(L1,J)*TEMP
      ENDIF
      AP(L2,J)=AP(L2,J)+AIP(L2,J)
      AIP(L2,J)=0.
   30 CONTINUE
COEFFICIENTS FOR Y-DIRECTION DIFFUSION
      DO 40 J=2,M3
      DO 40 I=2,L2
      AREA=RV(J+1)*XCV(I)
      DIFF=AREA*2.*GAM(I,J)*GAM(I,J+1)/(YCV(J)*GAM(I,J+1)+
     1 YCV(J+1)*GAM(I,J)+SMALL)
      AJP(I,J)=DIFF+SMALL 
      AJM(I,J+1)=AJP(I,J)
   40 CONTINUE
      DO 50 I=2,L2
CONSIDER J=1 BOUNDARY
      AREA=RV(2)*XCV(I)
      DIFF=GAM(I,2)/(0.5*YCV(2))+SMALL
      AJM(I,2)=BETA*DIFF
      AJP(I,1)=AJM(I,2)
      AJM(I,2)=AJM(I,2)*AREA
      AJM(I,1)=(BETA-1.)*AJP(I,2)/(RV(3)*XCV(I))
      AJP(I,2)=AJP(I,2)+AJM(I,1)*AREA
      IF(KBCJ1(I).EQ.1) THEN
      CON(I,2)=CON(I,2)+AJM(I,2)*F(I,1,NF)
      ELSE
      AP(I,1)=AJP(I,1)-FLXPJ1(I)
      CON(I,1)=FLXCJ1(I)  
      TEMP=AJM(I,2)/AP(I,1)
      AP(I,2)=AP(I,2)-AJP(I,1)*TEMP
      AJP(I,2)=AJP(I,2)-AJM(I,1)*TEMP
      CON(I,2)=CON(I,2)+CON(I,1)*TEMP
      ENDIF
      AP(I,2)=AP(I,2)+AJM(I,2)
      AJM(I,2)=0.
CONSIDER J=M1 BOUNDARY
      AREA=RV(M1)*XCV(I) 
      DIFF=GAM(I,M2)/(0.5*YCV(M2))+SMALL
      AJP(I,M2)=BETA*DIFF
      AJM(I,M1)=AJP(I,M2)
      AJP(I,M2)=AJP(I,M2)*AREA
      AJP(I,M1)=(BETA-1.)*AJM(I,M2)/(RV(M2)*XCV(I))
      AJM(I,M2)=AJM(I,M2)+AJP(I,M1)*AREA
      IF(KBCM1(I).EQ.1) THEN
      CON(I,M2)=CON(I,M2)+AJP(I,M2)*F(I,M1,NF)
      ELSE
      AP(I,M1)=AJM(I,M1)-FLXPM1(I)
      CON(I,M1)=FLXCM1(I)
      TEMP=AJP(I,M2)/AP(I,M1) 
      AP(I,M2)=AP(I,M2)-AJM(I,M1)*TEMP
      AJM(I,M2)=AJM(I,M2)-AJP(I,M1)*TEMP
      CON(I,M2)=CON(I,M2)+CON(I,M1)*TEMP
      ENDIF
      AP(I,M2)=AP(I,M2)+AJP(I,M2)
      AJP(I,M2)=0.
   50 CONTINUE
COME HERE TO INTRODUCE UNDERRELAXATION
CONSTRUCT AP(I,J) AND CON(I,J) IN THEIR FINAL FORM
      DO 60 J=2,M2
      DO 60 I=2,L2
      ANB=AIP(I,J)+AIM(I,J)+AJP(I,J)+AJM(I,J)
      AINR=ANB*RLX
      AP(I,J)=AP(I,J)+ANB+AINR
      CON(I,J)=CON(I,J)+AINR*F(I,J,NF)
   60 CONTINUE
C
CALL THE SOLVE ROUTINE TO OBTAIN THE SOLUTION OF THE DISCRETIZATION 
C  EQUATIONS
C
      CALL SOLVE
  999 CONTINUE
C
      TIME=TIME+DT
      ITER=ITER+1
      IF(ITER.GE.LAST) KSTOP=1  
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE SOLVE
C***********************************************************************
$INCLUDE:'COMMON'
      DIMENSION RT(6)
C***********************************************************************
      BIG1=1.E+10
      SMALL1=1.0E-5
      LL2=2*L2
      LL=LL2-2
      MM2=2*M2
      MM=MM2-2
      N=NF  
      NTM=NTIMES(N)
      DO 999 NT=1,NTM
      NTT=NT
      ICON=1
COME HERE TO PERFORM THE I-DIRECTION BLOCK CORRECTION
C-----------------------------------------------------------------------
      PTX(1)=0.
      QTX(1)=0.
      DO 10 I=2,L2  
      BL=SMALL
      BLP=0.
      BLM=0.
      BLC=0.
      DO  20 J=2,M2
      IF(AP(I,J).LT.BIG1) THEN
      BL=BL+AP(I,J) 
      IF(AP(I,J+1).LT.BIG1) BL=BL-AJP(I,J)
      IF(AP(I,J-1).LT.BIG1) BL=BL-AJM(I,J)
      IF(AP(I+1,J).LT.BIG1) BLP=BLP+AIP(I,J)
      IF(AP(I-1,J).LT.BIG1) BLM=BLM+AIM(I,J)
CONVERGENCE CRITERION FOR THE SOLUTION ROUTINE
      RT(1)=AIP(I,J)*F(I+1,J,N)
      RT(2)=AIM(I,J)*F(I-1,J,N)
      RT(3)=AJP(I,J)*F(I,J+1,N)
      RT(4)=AJM(I,J)*F(I,J-1,N)
      RT(5)=-AP(I,J)*F(I,J,N) 
      RT(6)=CON(I,J)
      RES=0
      TERM=1.0E-8
      DO 30 IRT=1,6 
      RES=RES+RT(IRT)
   30 TERM=MAX(TERM,ABS(RT(IRT)))
      IF(ABS(RES/TERM).GT.CRIT(N))ICON=0
      BLC=BLC+RES
      ENDIF
   20 CONTINUE
      DENOM=BL-PTX(I-1)*BLM
      IF(ABS(DENOM/BL).LT.SMALL1) DENOM=BIG
      PTX(I)=BLP/DENOM
      QTX(I)=(BLC+BLM*QTX(I-1))/DENOM
   10 CONTINUE
      IF(NTT.NE.1.AND.ICON.EQ.1) GO TO 990
      IF(KBLOC(NF).EQ.0) GO TO 80
      BL=0.
      DO 40 I=L2,2,-1
      BL=BL*PTX(I)+QTX(I)
      DO 40 J=2,M2  
      IF(AP(I,J).LT.BIG1) F(I,J,N)=F(I,J,N)+BL
   40 CONTINUE
COME HERE TO PERFORM THE J-DIRECTION BLOCK CORRECTION
C-----------------------------------------------------------------------
      PTY(1)=0.
      QTY(1)=0.
      DO 50 J=2,M2  
      BL=SMALL
      BLP=0.
      BLM=0.
      BLC=0.
      DO 60  I=2,L2
      IF(AP(I,J).LT.BIG1) THEN
      BL=BL+AP(I,J) 
      IF(AP(I+1,J).LT.BIG1) BL=BL-AIP(I,J)
      IF(AP(I-1,J).LT.BIG1) BL=BL-AIM(I,J)
      IF(AP(I,J+1).LT.BIG1) BLP=BLP+AJP(I,J)
      IF(AP(I,J-1).LT.BIG1) BLM=BLM+AJM(I,J)
      BLC=BLC+CON(I,J)+AIP(I,J)*F(I+1,J,N)+AIM(I,J)*F(I-1,J,N)
     1   +AJP(I,J)*F(I,J+1,N)+AJM(I,J)*F(I,J-1,N)-AP(I,J)*F(I,J,N)
      ENDIF
   60 CONTINUE
      DENOM=BL-PTY(J-1)*BLM
      IF(ABS(DENOM/BL).LT.SMALL1) DENOM=BIG
      PTY(J)=BLP/DENOM
      QTY(J)=(BLC+BLM*QTY(J-1))/DENOM
   50 CONTINUE
      BL=0.
      DO 70 J=M2,2,-1
      BL=BL*PTY(J)+QTY(J)
      DO 70 I=2,L2  
      IF(AP(I,J).LT.BIG1) F(I,J,N)=F(I,J,N)+BL
   70 CONTINUE
   80 CONTINUE
CARRY OUT THE I-DIRECTION TDMA  
C-----------------------------------------------------------------------
      DO 90 JJ=2,MM
      J=MIN(JJ,MM2-JJ)
      PTX(1)=0.
      QTX(1)=0
      DO 100 I=2,L2  
      DENOM=AP(I,J)-PTX(I-1)*AIM(I,J)
      PTX(I)=AIP(I,J)/DENOM
      TEMP=CON(I,J)+AJP(I,J)*F(I,J+1,N)+AJM(I,J)*F(I,J-1,N) 
      QTX(I)=(TEMP+AIM(I,J)*QTX(I-1))/DENOM
  100 CONTINUE
      DO 110 I=L2,2,-1
  110 F(I,J,N)=F(I+1,J,N)*PTX(I)+QTX(I)
   90 CONTINUE
CARRY OUT THE J-DIRECTION TDMA  
C-----------------------------------------------------------------------
      DO 120 II=2,LL
      I=MIN(II,LL2-II)
      PTY(1)=0.
      QTY(1)=0
      DO 130 J=2,M2
      DENOM=AP(I,J)-PTY(J-1)*AJM(I,J)
      PTY(J)=AJP(I,J)/DENOM
      TEMP=CON(I,J)+AIP(I,J)*F(I+1,J,N)+AIM(I,J)*F(I-1,J,N) 
      QTY(J)=(TEMP+AJM(I,J)*QTY(J-1))/DENOM
  130 CONTINUE
      DO 140 J=M2,2,-1
  140 F(I,J,N)=F(I,J+1,N)*PTY(J)+QTY(J)
  120 CONTINUE
C-----------------------------------------------------------------------
  999 CONTINUE
      NTC(N)=NTT
      GO TO 991
  990 NTC(N)=NTT-1
  991 CONTINUE
CALCULATE THE UNKNOWN BOUNDARY VALUES AND FLUXES
C-----------------------------------------------------------------------
      DO 160 I=2,L2  
      TEMP=AJM(I,1)*(F(I,3,N)-F(I,2,N))
      IF(KBCJ1(I).EQ.2)
     1 F(I,1,N)=(AJP(I,1)*F(I,2,N)-TEMP+CON(I,1))/AP(I,1)
      FLUXJ1(I,N)=AJP(I,1)*(F(I,1,N)-F(I,2,N))+TEMP 
      TEMP=AJP(I,M1)*(F(I,M3,N)-F(I,M2,N))
      IF(KBCM1(I).EQ.2)
     1 F(I,M1,N)=(AJM(I,M1)*F(I,M2,N)-TEMP+CON(I,M1))/AP(I,M1)
  160 FLUXM1(I,N)=AJM(I,M1)*(F(I,M1,N)-F(I,M2,N))+TEMP
      DO 170 J=2,M2  
      TEMP=AIM(1,J)*(F(3,J,N)-F(2,J,N))
      IF(KBCI1(J).EQ.2)
     1 F(1,J,N)=(AIP(1,J)*F(2,J,N)-TEMP+CON(1,J))/AP(1,J)
      FLUXI1(J,N)=AIP(1,J)*(F(1,J,N)-F(2,J,N))+TEMP 
      TEMP=AIP(L1,J)*(F(L3,J,N)-F(L2,J,N))
      IF(KBCL1(J).EQ.2)
     1 F(L1,J,N)=(AIM(L1,J)*F(L2,J,N)-TEMP+CON(L1,J))/AP(L1,J)
  170 FLUXL1(J,N)=AIM(L1,J)*(F(L1,J,N)-F(L2,J,N))+TEMP
C
COME HERE TO RESET CON,AP,KBC,FLXC, AND FLXP
C-----------------------------------------------------------------------
      DO 180 J=2,M2 
      KBCI1(J)=1
      KBCL1(J)=1
      FLXCI1(J)=0.
      FLXCL1(J)=0.
      FLXPI1(J)=0.
      FLXPL1(J)=0.
      DO 180 I=2,L2 
      CON(I,J)=0.
      AP(I,J)=0.
  180 CONTINUE
      DO 190 I=2,L2
      KBCJ1(I)=1
      KBCM1(I)=1
      FLXCJ1(I)=0.
      FLXCM1(I)=0.
      FLXPJ1(I)=0.
      FLXPM1(I)=0.
  190 CONTINUE
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE TOOLS
C***********************************************************************
$INCLUDE:'COMMON'
C***********************************************************************
      ENTRY EZGRID
C
CONSTRUCT THE X-DIRECTION GRID
      L1=NCVLX+2
      XU(2)=0.
      XU(L1)=XL
      L2=L1-1
      FCVLX=FLOAT(NCVLX)
      DO 20 I=3,L2
      DD=FLOAT(I-2)/FCVLX
      IF(POWERX.GT.0.) THEN
      XU(I)=XL*DD**POWERX
      ELSE
      XU(I)=XL*(1.-(1.-DD)**(-POWERX))
      ENDIF
   20 CONTINUE
CONSTRUCT THE Y-DIRECTION GRID
      M1=NCVLY+2
      YV(2)=0.
      YV(M1)=YL
      M2=M1-1
      FCVLY=FLOAT(NCVLY)
      DO 30 J=3,M2
      DD=FLOAT(J-2)/FCVLY
      IF(POWERY.GT.0.) THEN
      YV(J)=YL*DD**POWERY
      ELSE
      YV(J)=YL*(1.-(1.-DD)**(-POWERY))
      ENDIF
   30 CONTINUE   
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY ZGRID
CONSTRUCT THE GRID ZONE-BY-ZONE
C
CONSIDER THE X DIRECTION
      XU(2)=0.
      I2=2
      DO 100 NZ=1,NZX
      FCVLX=FLOAT(NCVX(NZ))
      ILAST=I2
      I1=ILAST+1
      I2=ILAST+NCVX(NZ)
      DO 100 I=I1,I2
      DD=FLOAT(I-ILAST)/FCVLX
      IF(POWRX(NZ).GT.0.) THEN
      XU(I)=XU(ILAST)+XZONE(NZ)*DD**POWRX(NZ)
      ELSE
      XU(I)=XU(ILAST)+XZONE(NZ)*(1.-(1.-DD)**(-POWRX(NZ)))
      ENDIF
  100 CONTINUE
      L1=I2
C
CONSIDER THE Y DIRECTION
      YV(2)=0.
      J2=2
      DO 110 NZ=1,NZY
      FCVLY=FLOAT(NCVY(NZ))
      JLAST=J2
      J1=JLAST+1
      J2=JLAST+NCVY(NZ)
      DO 110 J=J1,J2
      DD=FLOAT(J-JLAST)/FCVLY
      IF(POWRY(NZ).GT.0.) THEN
      YV(J)=YV(JLAST)+YZONE(NZ)*DD**POWRY(NZ)
      ELSE
      YV(J)=YV(JLAST)+YZONE(NZ)*(1.-(1.-DD)**(-POWRY(NZ)))
      ENDIF
  110 CONTINUE
      M1=J2
      RETURN  
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PRINT
C
      DO 999 IUNIT=IU1,IU2   
C
COME HERE TO ARRANGE THE PRINTOUT OF TWO-DIMENSIONAL FIELDS 
      IF(KPGR.NE.0) THEN
C
CREATE PRINTOUT FOR GRID
C
      WRITE(IUNIT,1)
    1 FORMAT(' ')
      IBEG=1
      IEND=L1
      IREP=(IEND-IBEG+7)/7
      DO 200 K=1,IREP
      INCR=MIN(6,IEND-IBEG)
      ISTOP=IBEG+INCR
      WRITE(IUNIT,2) (I,I=IBEG,ISTOP)  
    2 FORMAT(/2X,'I =',2X,7(I4,5X))
      IF(MODE.EQ.3) THEN
      WRITE(IUNIT,3) (X(I),I=IBEG,ISTOP)
    3 FORMAT(1X,'TH =',1P7E9.2)
      ELSE
      WRITE(IUNIT,4) (X(I),I=IBEG,ISTOP)
    4 FORMAT(2X,'X =',1P7E9.2)
      ENDIF
      IBEG=ISTOP+1
  200 CONTINUE
C
      WRITE(IUNIT,1)
      JBEG=1
      JEND=M1
      JREP=(JEND-JBEG+7)/7
      DO 210 K=1,JREP
      INCR=MIN(6,JEND-JBEG)
      JSTOP=JBEG+INCR
      WRITE(IUNIT,5) (J,J=JBEG,JSTOP)
    5 FORMAT(/2X,'J =',2X,7(I4,5X))
      WRITE(IUNIT,6) (Y(J),J=JBEG,JSTOP)
    6 FORMAT(2X,'Y =',1P7E9.2)
      JBEG=JSTOP+1
  210 CONTINUE
      ENDIF
CREATE PRINTOUT FOR THE VALUES OF DEPENDENT VARIABLES
      DO 220 N=1,NFMAX
      IF(KPRINT(N).NE.0) THEN
      WRITE(IUNIT,7) TITLE(N)
    7 FORMAT(/1X,6(1H*),3X,A18,3X,6(1H*)/9X,20(1H-))
      IBEG=1
      JBEG=1
      IEND=L1
      JEND=M1
      IREP=(IEND-IBEG+7)/7
      DO 230 K=1,IREP
      INCR=MIN(6,IEND-IBEG)
      ISTOP=IBEG+INCR
      WRITE(IUNIT,8) (I,I=IBEG,ISTOP)
    8 FORMAT(/'  I =',I6,6I9) 
      WRITE(IUNIT, 9)
    9 FORMAT('  J') 
      DO 240 J=JEND,JBEG,-1
      WRITE(IUNIT,10) J,(F(I,J,N),I=IBEG,ISTOP)
   10 FORMAT(1X,I2,3X,1P7E9.2)
  240 CONTINUE
      IBEG=ISTOP+1
  230 CONTINUE
      ENDIF
  220 CONTINUE
  999 CONTINUE 
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PLOT
      OPEN(UNIT=8,FILE=PLOTF)
COME HERE TO CREATE DATA FOR PLOTTING 
C 
      KFLOW=2
      WRITE(8,300) HEADER
  300 FORMAT(A64)
      WRITE(8,310) KFLOW,L1,M1,NFMAX,MODE,(KPLOT(I),I=1,NFMAX)
  310 FORMAT(18I5)
      IBLOK=0
      DO 320 J=2,M2
      DO 320 I=2,L2
         IF(IBLOCK(I,J).EQ.1) THEN
            IBLOK=1
            GO TO 330
         ENDIF
  320 CONTINUE
  330 CONTINUE
      WRITE(8,310) IBLOK
      WRITE(8,340) (TITLE(N),N=1,NFMAX)
  340 FORMAT(4A18)
      WRITE(8,350) (X(I),I=1,L1),(Y(J),J=1,M1),(XU(I),I=2,L1)
     1,(YV(J),J=2,M1),(R(J),J=1,M1)
  350 FORMAT(5E12.6)
      DO 360 N=1,NFMAX
      IF(KPLOT(N).NE.0) WRITE(8,350) ((F(I,J,N),I=1,L1),J=1,M1)
  360 CONTINUE
      IF(IBLOK.EQ.1) THEN
         WRITE(8,310) ((IBLOCK(I,J),I=1,L1),J=1,M1)
      ENDIF
      CLOSE(8)
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE VALUES
C***********************************************************************
C
CREATE A FACILITY TO ASSIGN VALUES TO REAL VARIABLES
C
      ENTRY DATA9(A1,C1,A2,C2,A3,C3,A4,C4,A5,C5,A6,C6,A7,C7,A8,C8,A9,C9)
      A9=C9
      ENTRY DATA8(A1,C1,A2,C2,A3,C3,A4,C4,A5,C5,A6,C6,A7,C7,A8,C8)
      A8=C8
      ENTRY DATA7(A1,C1,A2,C2,A3,C3,A4,C4,A5,C5,A6,C6,A7,C7)
      A7=C7
      ENTRY DATA6(A1,C1,A2,C2,A3,C3,A4,C4,A5,C5,A6,C6)
      A6=C6
      ENTRY DATA5(A1,C1,A2,C2,A3,C3,A4,C4,A5,C5)
      A5=C5
      ENTRY DATA4(A1,C1,A2,C2,A3,C3,A4,C4)
      A4=C4
      ENTRY DATA3(A1,C1,A2,C2,A3,C3)
      A3=C3
      ENTRY DATA2(A1,C1,A2,C2)
      A2=C2
      ENTRY DATA1(A1,C1)
      A1=C1
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
CREATE A FACILITY TO ASSIGN VALUES TO INTEGER VARIABLES
C
      ENTRY INTA9(I1,J1,I2,J2,I3,J3,I4,J4,I5,J5,I6,J6,I7,J7,I8,J8,I9,J9)
      I9=J9
      ENTRY INTA8(I1,J1,I2,J2,I3,J3,I4,J4,I5,J5,I6,J6,I7,J7,I8,J8)
      I8=J8
      ENTRY INTA7(I1,J1,I2,J2,I3,J3,I4,J4,I5,J5,I6,J6,I7,J7)
      I7=J7
      ENTRY INTA6(I1,J1,I2,J2,I3,J3,I4,J4,I5,J5,I6,J6)
      I6=J6
      ENTRY INTA5(I1,J1,I2,J2,I3,J3,I4,J4,I5,J5)
      I5=J5
      ENTRY INTA4(I1,J1,I2,J2,I3,J3,I4,J4)
      I4=J4
      ENTRY INTA3(I1,J1,I2,J2,I3,J3)
      I3=J3
      ENTRY INTA2(I1,J1,I2,J2)
      I2=J2
      ENTRY INTA1(I1,J1)
      I1=J1
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	SUBROUTINE SAIDA
$INCLUDE:'COMMON'
      DIMENSION T(NI,NJ)
      EQUIVALENCE (F(1,1,1),T(1,1))
	DIMENSION XZ(NI,NJ),YZ(NI,NJ),XUZ(NI,NJ),YVZ(NI,NJ)
	CHARACTER*8 NAME
	CHARACTER*2 AUX1,AUX2
C.....SAÍDA DE DADOS PARA PLOT NO MATLAB
C.....RAIO
	OPEN(10,FILE='RAIO.DAT')
	DO JJ=1,NJ
	   WRITE(10,491) R(JJ)
	END DO
C.....EIXOS X, Y E Z 
      OPEN(10,FILE='X.DAT')
      OPEN(11,FILE='Y.DAT')
      OPEN(13,FILE='XU.DAT')
      OPEN(14,FILE='YV.DAT')
	DO I=1,L1
	    WRITE(10,491)X(I)
	    WRITE(13,491)XU(I)
	END DO
	DO J=1,M1
	    WRITE(11,491)Y(J)
	    WRITE(14,491)YV(J)
	END DO
	CLOSE(10)
	CLOSE(11)
	CLOSE(13)
	CLOSE(14)
C.....PLANO XY
	DO I=1,L1
	DO J=1,M1
		XZ(I,J) = X(I)
		YZ(I,J) = Y(J)
		XUZ(I,J) = XU(I)
		YVZ(I,J) = YV(J)
		IF(MODE.EQ.3) THEN
			XZ(I,J) = (R(1)+Y(J))*COS(X(I))
			YZ(I,J) = (R(1)+Y(J))*SIN(X(I))
			XUZ(I,J) = (R(1)+YV(J))*COS(XU(I))
			YVZ(I,J) = (R(1)+YV(J))*SIN(XU(I))
		END IF
	END DO
	END DO
      OPEN(10,FILE='XZ.DAT')
      OPEN(11,FILE='YZ.DAT')
      OPEN(12,FILE='XUZ.DAT')
      OPEN(13,FILE='YVZ.DAT')
	OPEN(20,FILE='MZ.DAT')
	DO I=1,L1
	   WRITE(10,492)
	   WRITE(11,492)
	   WRITE(12,492)
	   WRITE(13,492)
	   WRITE(20,492)
         DO J=M1,1,-1   
			WRITE(10,491)XZ(I,J)
			WRITE(11,491)YZ(I,J)
			WRITE(12,491)XUZ(I,J)
			WRITE(13,491)YVZ(I,J)
			WRITE(20,491)FLOAT(IBLOCK(I,J))
		END DO
	END DO
	CLOSE(10)
	CLOSE(11)
	CLOSE(12)
	CLOSE(13)
	CLOSE(20)
C.....IMPRESSÃO DOS CAMPOS (TEMPERATURA, CONCENTRAÇÃO, ETC.)
C.....DECORA QUINAS
	DO NF1=1,NFMAX
		DO I=1,L1
			F(I,1,NF1) = (F(I,1,NF1)+F(I,2,NF1))/2.
			F(I,M1,NF1) = (F(I,M1,NF1)+F(I,M2,NF1))/2.
			F(I,1,NF1) = (F(I,1,NF1)+F(I,2,NF1))/2.
			F(I,M1,NF1) = (F(I,M1,NF1)+F(I,M2,NF1))/2.
		END DO
		DO J=1,M1
			F(1,J,NF1) = (F(1,J,NF1)+F(2,J,NF1))/2.
			F(1,J,NF1) = (F(1,J,NF1)+F(2,J,NF1))/2.
			F(L1,J,NF1) = (F(L1,J,NF1)+F(L2,J,NF1))/2.
			F(L1,J,NF1) = (F(L1,J,NF1)+F(L2,J,NF1))/2.
		END DO
	END DO
C.....PLANO XY
	WRITE(AUX1,'(I2)')10
	WRITE(AUX2,'(I2)')IPRINT+10
	NAME = 'TZ'//AUX1//'.D'//AUX2
	OPEN(20,FILE=NAME)
	DO I=1,L1
		WRITE(20,492)
         DO J=M1,1,-1   
			WRITE(20,491) T(I,J)
		END DO
	END DO
	CLOSE(20)
	IPRINT = IPRINT + 1
  491 FORMAT(1X,F28.20,\)
  492 FORMAT('')
	RETURN
	END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
