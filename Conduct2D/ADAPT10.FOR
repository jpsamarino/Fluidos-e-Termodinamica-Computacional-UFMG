CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ADAPT
C-----------------------------------------------------------------------
C-----EXAMPLE 10 -- STAGGERED FIN ARRAY 
C-----------------------------------------------------------------------
$INCLUDE:'COMMON'
C***********************************************************************
      DIMENSION W(NI,NJ),T(NI,NJ)
      EQUIVALENCE (F(1,1,1),W(1,1)),(F(1,1,2),T(1,1))
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY GRID
      HEADER='STAGGERED FIN ARRAY'
      PRINTF='PRINT10'
      PLOTF='PLOT10'
      H=1.  
      S=H 
      SHALF=0.5*S
      TH=0.2*S
      X1=0.5*TH
      X2=SHALF-X1
      HFIN=0.75*H
      C=H-HFIN
      CALL INTA4(NZX,3,NCVX(1),2,NCVX(2),8,NCVX(3),2)
      CALL DATA3(XZONE(1),X1,XZONE(2),X2-X1,XZONE(3),X1)
      CALL INTA4(NZY,3,NCVY(1),3,NCVY(2),5,NCVY(3),3)
      CALL DATA3(YZONE(1),C,YZONE(2),HFIN-C,YZONE(3),C)
      CALL ZGRID
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY BEGIN
      TITLE(1)='      W/WBAR      '  
      TITLE(2)='(T-TINF)/(TB-TINF)'
      CALL INTA6(KSOLVE(1),1,KPRINT(1),1,KPRINT(2),1,
     1           KPLOT(1),1,KPLOT(2),1,LAST,10)
      CALL DATA9(AMU,1.,COND,1.,CP,1.,DEN,1.,DPDZ,-1.,TINF,0.,
     1           DTBDZ,1.,BITOP,5.,BIBOT,0.2)
      HTOP=BITOP*COND/H 
      HBOT=BIBOT*COND/H 
      RHOCP=DEN*CP
      CNDFIN=1.8*COND
      DO 100 J=1,M1
      DO 100 I=1,L1
         W(I,J)=0.
         T(I,J)=TINF+1.
  100 CONTINUE
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY OUTPUT
      IF(ITER.EQ.3) THEN
         KSOLVE(1)=0
         KSOLVE(2)=1
      ENDIF
      ASUM=0.
      WSUM=0.
      TSUM=0.
      DO 200 J=2,M2
      DO 200 I=2,L2
         AR=XCV(I)*YCV(J)
         IF(X(I).LT.X1.AND.Y(J).LT.HFIN) AR=0.
         IF(X(I).GT.X2.AND.Y(J).GT.C) AR=0.
         ASUM=ASUM+AR
         WSUM=WSUM+W(I,J)*AR
         TSUM=TSUM+W(I,J)*T(I,J)*AR
  200 CONTINUE
      WBAR=WSUM/ASUM
      TB=TSUM/(WSUM+SMALL) 
      WP=S+2.*HFIN
      DH=4.*ASUM/WP 
      RE=DH*WBAR*DEN/AMU
      FRE=-2.*DPDZ*DH/(DEN*WBAR**2+SMALL)*RE
      HP=S  
      QW=DTBDZ*RHOCP*WSUM/HP
      ANU=QW*H/(COND*(TINF-TB)+SMALL)
      QTOP=0.
      QBOT=0.
      DO 210 I=2,L2 
         QTOP=QTOP+FLUXM1(I,2)*XCV(I)
         QBOT=QBOT+FLUXJ1(I,2)*XCV(I)
  210 CONTINUE
      ANUT=QTOP*H/(SHALF*COND*(TINF-TB)+SMALL)
      ANUB=QBOT*H/(SHALF*COND*(TINF-TB)+SMALL)
      DO 220 IUNIT=IU1,IU2
         IF(ITER.EQ.0) WRITE(IUNIT,230)
  230    FORMAT(1X,'ITER',2X,'W(8,8)',5X,'T(8,8)',6X,
     1   'FRE',8X,'NU',7X,'NU(TOP)',3X,'NU(BOTTOM)')
         WRITE(IUNIT,240) ITER,W(8,8),T(8,8),FRE,ANU,ANUT,ANUB
  240    FORMAT(1X,I2,1P6E11.3)
  220 CONTINUE   
      IF(ITER.EQ.LAST) THEN
         DO 250 J=1,M1
         DO 250 I=1,L1
            W(I,J)=W(I,J)/WBAR
            IF(W(I,J).LT.1.E-10) W(I,J)=0.
            T(I,J)=(T(I,J)-TINF)/(TB-TINF)
  250    CONTINUE
         CALL PRINT
C------
COME HERE TO FILL IBLOCK(I,J) BEFORE CALLING PLOT
         DO 260 J=2,M2
         DO 260 I=2,L2
            IF(X(I).LT.X1.AND.Y(J).LT.HFIN) IBLOCK(I,J)=1
            IF(X(I).GT.X2.AND.Y(J).GT.C) IBLOCK(I,J)=1
  260    CONTINUE
C------
         CALL PLOT
      ENDIF   
      RETURN
C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      ENTRY PHI
      IF(NF.EQ.1) THEN
         DO 300 J=2,M2
         DO 300 I=2,L2
            GAM(I,J)=AMU
            IF(X(I).LT.X1.AND.Y(J).LT.HFIN) GAM(I,J)=BIG
            IF(X(I).GT.X2.AND.Y(J).GT.C) GAM(I,J)=BIG
            SC(I,J)=-DPDZ 
  300    CONTINUE
      ENDIF
      IF(NF.EQ.2) THEN  
         DO 310 J=2,M2
         DO 310 I=2,L2
            GAM(I,J)=COND 
            IF(X(I).LT.X1.AND.Y(J).LT.HFIN) GAM(I,J)=CNDFIN
            IF(X(I).GT.X2.AND.Y(J).GT.C) GAM(I,J)=CNDFIN
            TEMP=(T(I,J)-TINF)/(TB-TINF+SMALL)
            DTDZ=TEMP*DTBDZ
            SC(I,J)=-RHOCP*DTDZ*W(I,J)
  310    CONTINUE    
      ENDIF   
COME HERE TO SPECIFY BOUNDARY CONDITIONS
      DO 320 J=2,M2
         KBCI1(J)=2
         KBCL1(J)=2
  320 CONTINUE    
      IF(NF.EQ.2) THEN
         DO 330 I=2,L2
            KBCM1(I)=2
            KBCJ1(I)=2
            FLXCM1(I)=HTOP*TINF
            FLXPM1(I)=-HTOP
            FLXCJ1(I)=HBOT*TINF
            FLXPJ1(I)=-HBOT 
  330    CONTINUE
      ENDIF   
      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
