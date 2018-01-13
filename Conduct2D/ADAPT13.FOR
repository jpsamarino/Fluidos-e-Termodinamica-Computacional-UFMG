CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 13 -- TURBULENT FLOW IN A SQUARE DUCT
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION W(NI,NJ),T(NI,NJ),AMUT(NI,NJ),XPLUS(NI,NJ),YPLUS(NI,NJ)
      EQUIVALENCE (F(1,1,1),W(1,1)),(F(1,1,2),T(1,1)),
     1            (F(1,1,3),AMUT(1,1)),(F(1,1,4),XPLUS(1,1)),
     2            (F(1,1,5),YPLUS(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      DFACT(ARG)=1.-EXP(-ARG/26.)
      CAPL(A1,A2)=A1*(0.14-0.08*(1.-A2/A1)**2-0.06*(1.-A2/A1)**4)
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='TURBULENT FLOW IN A SQUARE DUCT'
      PRINTF='PRINT13'
      PLOTF='PLOT13'
      CALL INTA2(NCVLX,10,NCVLY,10)
      CALL DATA4(XL,0.5,YL,0.5,POWERX,3.,POWERY,3.)
      CALL EZGRID
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='      W/WBAR      '
      TITLE(2)='(T-TWAV)/(TB-TWAV)'
      TITLE(3)=' TURB. VISCOSITY  '
      CALL INTA8(KSOLVE(1),1,KPRINT(1),1,KPLOT(1),1,KPRINT(2),1,
     1           KPLOT(2),1,KPRINT(3),1,KPLOT(3),1,LAST,18)
      CALL DATA5(AMU,1.,DEN,1.,DPDZ,-1.,RE,1.E5,REGAM,0.8)
      CALL DATA4(PR,0.7,CP,1.,PRT,0.9,QW,1.)
      COND=CP*AMU/PR
      RHOCP=DEN*CP
      GAMAVP=AMU
C
C-- SINCE THE ZERO DEFAULT VALUES OF W(I,J) AND T(I,J) ARE SATISFACTORY,
C   THESE ARRAYS ARE NOT FILLED HERE.
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY OUTPUT
      IF(ITER.EQ.15)THEN
         KSOLVE(1)=0
         KSOLVE(2)=1
         COND=CP*AMU/PR
      ENDIF
      ASUM=0.
      WSUM=0.
      TSUM=0.
      DO 200 J=2,M2
      DO 200 I=2,L2
         AR=XCV(I)*YCV(J)
         ASUM=ASUM+AR
         WSUM=WSUM+W(I,J)*AR
         TSUM=TSUM+W(I,J)*T(I,J)*AR
  200 CONTINUE
      WBAR=WSUM/ASUM
      TB=TSUM/(WSUM+SMALL) 
      WP=XL+YL
      DH=4.*ASUM/WP 
      FRIC=-2.*DPDZ*DH/(DEN*WBAR**2+SMALL)
      HP=YL
      DTDZ=QW*HP/(RHOCP*WSUM+SMALL)
      TWAV=0.
      DO 210 J=2,M2
         TWAV=TWAV+T(1,J)*YCV(J)
  210 CONTINUE
      TWAV=TWAV/YL
      ANU=QW*DH/(COND*(TWAV-TB)+SMALL)
      IF(ITER.GE.1) AMU=DH*WBAR*DEN/RE
      DO 220 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,230)
  230    FORMAT(1X,'ITER',3X,'W(8,8)',4X,'T(8,8)'
     1   ,3X,'  AMU',7X,' F',9X,'NU')
         WRITE(IUNIT,240)ITER,W(8,8),T(8,8),AMU,FRIC,ANU
  240    FORMAT(1X,I3,1X,1P5E10.2)
  220 CONTINUE
      IF(ITER.EQ.LAST) THEN
        DO 250 I=1,L1
        DO 250 J=1,M1
           W(I,J)=W(I,J)/WBAR    
           T(I,J)=(T(I,J)-TWAV)/(TB-TWAV)
 250    CONTINUE
        CALL PRINT
        CALL PLOT
      ENDIF   
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PHI
      IF(NF.EQ.1) THEN
         REL=1.-REGAM
         DO 300 J=2,M2
         DO 300 I=2,L2
            IF(ITER.EQ.0) THEN
               GAM(I,J)=AMU
            ELSE
               TAUW=ABS(FLUXI1(J,1))
               XPLUS(I,J)=X(I)*SQRT(TAUW*DEN)/AMU
               TAUW=ABS(FLUXJ1(I,1))
               YPLUS(I,J)=Y(J)*SQRT(TAUW*DEN)/AMU
               RLX=DFACT(XPLUS(I,J))*CAPL(XL,X(I))
               RLY=DFACT(YPLUS(I,J))*CAPL(YL,Y(J))
               RL=(RLX**(-20)+RLY**(-20))**(-0.05)
               FP=1.
               IF(I.NE.L2) FP=XCV(I)/(XCV(I)+XCV(I+1))
               WP=FP*W(I+1,J)+(1.-FP)*W(I,J)
               FM=1.
               IF(I.NE.2) FM=XCV(I)/(XCV(I)+XCV(I-1))
               WM=FM*W(I-1,J)+(1.-FM)*W(I,J)
               DWDX=(WP-WM)/XCV(I)
               FP=1.
               IF(J.NE.M2) FP=YCV(J)/(YCV(J)+YCV(J+1))
               WP=FP*W(I,J+1)+(1.-FP)*W(I,J)
               FM=1.
               IF(J.NE.2) FM=YCV(J)/(YCV(J)+YCV(J-1))
               WM=FM*W(I,J-1)+(1.-FM)*W(I,J)
               DWDY=(WP-WM)/YCV(J)
               GAMT=DEN*RL**2*SQRT(DWDX**2+DWDY**2)
               IF(ITER.EQ.1) AMUT(I,J)=GAMT
               AMUT(I,J)=REGAM*GAMT+REL*AMUT(I,J)
               GAM(I,J)=AMU+AMUT(I,J)
            ENDIF
  300    CONTINUE
COME HERE TO ADJUST THE PRESSURE GRADIENT
         IF(ITER.EQ.1) THEN
            GAMAV=0.
            DO 305 J=2,M2
            DO 305 I=2,L2
               GAMAV=GAMAV+GAM(I,J)*XCV(I)*YCV(J)
  305       CONTINUE
            GAMAV=GAMAV/ASUM
            DPDZ=DPDZ*GAMAV/GAMAVP
            GAMAVP=GAMAV
         ENDIF
         DO 307 J=2,M2
         DO 307 I=2,L2
            SC(I,J)=-DPDZ
  307    CONTINUE
      ENDIF
C 
      IF(NF.EQ.2) THEN
         DO 310 J=2,M2
         DO 310 I=2,L2
            GAM(I,J)=COND+CP*AMUT(I,J)/PRT
            SC(I,J)=-RHOCP*W(I,J)*DTDZ
  310    CONTINUE
      ENDIF
COME HERE TO SPECIFY BOUNDARY CONDITIONS
      DO 320 I=2,L2
         KBCM1(I)=2
  320 CONTINUE
      DO 330 J=2,M2
         KBCL1(J)=2
  330 CONTINUE
C
      IF(NF.EQ.2) THEN
         DO 340 I=2,L2
            KBCJ1(I)=2
  340    CONTINUE
         DO 350 J=2,M2
            KBCI1(J)=2
            FLXCI1(J)=QW
  350    CONTINUE
      ENDIF   
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


