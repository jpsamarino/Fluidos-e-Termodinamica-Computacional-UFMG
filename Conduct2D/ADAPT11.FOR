CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 11 -- NON-NEWTONIAN FLOW IN A SEMICIRCULAR DUCT
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION W(NI,NJ),T(NI,NJ)
      EQUIVALENCE (F(1,1,1),W(1,1)),(F(1,1,2),T(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='NON-NEWTONIAN FLOW IN A SEMICIRCULAR DUCT'
      PRINTF='PRINT11'
      PLOTF='PLOT11'
      CALL DATA2(R(1),0.,PI,3.14159)
      CALL DATA2(XL,0.5*PI,YL,1.)
      CALL INTA3(MODE,3,NCVLX,10,NCVLY,16)
      CALL EZGRID
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='      W/WBAR      '
      TITLE(2)='(T-TWAV)/(TB-TWAV)'
      CALL INTA6(KSOLVE(1),1,KPRINT(1),1,KPRINT(2),1,
     1           KPLOT(1),1,KPLOT(2),1,LAST,18)
      CALL DATA9(DEN,1.,REGAM,0.8,RK,1.,POWER,0.5,AMU,1.,COND,1.,
     1           CP,1.,DPDZ,-1.,QW,1.)
      RHOCP=DEN*CP
C
C-- SINCE THE ZERO DEFAULT VALUES OF W(I,J) AND T(I,J) ARE SATISFACTORY,
C   THESE ARRAY ARE NOT FILLED HERE.
C
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY OUTPUT
      IF(ITER.EQ.15) THEN
         KSOLVE(1)=0
         KSOLVE(2)=1
      ENDIF
      ASUM=0.
      WSUM=0.
      TSUM=0.
      DO 200 J=2,M2 
      DO 200 I=2,L2 
         AR=XCV(I)*YCVR(J)
         ASUM=ASUM+AR
         WSUM=WSUM+W(I,J)*AR
         TSUM=TSUM+W(I,J)*T(I,J)*AR
  200 CONTINUE
      WBAR=WSUM/ASUM
      TB=TSUM/(WSUM+SMALL)
      WP=R(M1)+0.5*PI*R(M1)
      HP=0.5*PI*R(M1)
      DH=4.*ASUM/WP
      AMUR=RK*(WBAR/DH+SMALL)**(POWER-1.)
      RE=DEN*WBAR*DH/AMUR
      FRE=-DPDZ*DH/(0.5*DEN*WBAR**2+SMALL)*RE
      DTDZ=QW*HP/(RHOCP*WSUM+SMALL)
      TWAV=0.
      DO 220 I=2,L2 
         TWAV=TWAV+T(I,M1)*XCV(I)
  220 CONTINUE    
      TWAV=TWAV/XL
      ANU=QW*DH/(COND*(TWAV-TB)+SMALL)
      DO 240 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,250)
  250    FORMAT(1X,'ITER',2X,'W(3,4)',5X,'F.RE',5X,'T(5,4)',6X,'NU')
         WRITE(IUNIT,260) ITER,W(3,4),FRE,T(5,4),ANU
  260    FORMAT(2X,I2,1P4E10.2)
  240 CONTINUE    
      IF(ITER.EQ.LAST) THEN 
         DO 270 J=1,M1 
         DO 270 I=1,L1 
            W(I,J)=W(I,J)/WBAR
            T(I,J)=(T(I,J)-TWAV)/(TB-TWAV)
  270    CONTINUE    
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
               WP=0.5*(W(I+1,J)+W(I,J))
               WM=0.5*(W(I,J)+W(I-1,J))
               IF(I.EQ.2) WM=W(I-1,J)
               IF(I.EQ.L2) WP=0.
               DWDX=(WP-WM)/(XCV(I)*SX(J))
               WP=0.5*(W(I,J+1)+W(I,J))
               WM=0.5*(W(I,J)+W(I,J-1))
               IF(J.EQ.2) WM=0.
               IF(J.EQ.M2) WP=0.
               DWDY=(WP-WM)/YCV(J)
               GAMT=RK*(DWDX**2+DWDY**2)**((POWER-1.)*0.5)
               IF(ITER.EQ.1) GAM(I,J)=GAMT
               GAM(I,J)=REGAM*GAMT+REL*GAM(I,J)
            ENDIF   
  300    CONTINUE
COME HERE TO ADJUST THE PRESSURE GRADIENT
         IF(ITER.EQ.1) THEN
            GAMAV=0.
            DO 310 J=2,M2
            DO 310 I=2,L2
               GAMAV=GAMAV+GAM(I,J)*XCV(I)*YCVR(J)
  310       CONTINUE
            GAMAV=GAMAV/ASUM
            DPDZ=DPDZ*GAMAV/AMU
         ENDIF
         DO 320 J=2,M2
         DO 320 I=2,L2
            SC(I,J)=-DPDZ
  320    CONTINUE
      ENDIF
      IF(NF.EQ.2) THEN
         DO 330 J=2,M2
         DO 330 I=2,L2
            GAM(I,J)=COND 
            SC(I,J)=-RHOCP*W(I,J)*DTDZ
  330    CONTINUE
      ENDIF
COME HERE TO SPECIFY BOUNDARY CONDITIONS      
         DO 340 J=2,M2
            KBCI1(J)=2
  340    CONTINUE
         DO 350 I=2,L2
            KBCJ1(I)=2
  350    CONTINUE
      IF(NF.EQ.2) THEN
         DO 360 J=2,M2
            KBCL1(J)=2
  360    CONTINUE
         DO 370 I=2,L2
            KBCM1(I)=2
            FLXCM1(I)=QW 
  370    CONTINUE
      ENDIF         
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
