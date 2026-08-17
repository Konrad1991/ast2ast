C     SUBROUTINE H12 (MODE,LPIVOT,L1,M,U,IUE,UP,C,ICE,ICV,NCV)  
C   
C  CONSTRUCTION AND/OR APPLICATION OF A SINGLE   
C  HOUSEHOLDER TRANSFORMATION..     Q = I + U*(U**T)/B   
C   
c  The original version of this code was developed by
c  Charles L. Lawson and Richard J. Hanson at Jet Propulsion Laboratory
c  1973 JUN 12, and published in the book
c  "SOLVING LEAST SQUARES PROBLEMS", Prentice-HalL, 1974.
c  Revised FEB 1995 to accompany reprinting of the book by SIAM.
C     ------------------------------------------------------------------
c                     Subroutine Arguments
c
C     MODE   = 1 OR 2   Selects Algorithm H1 to construct and apply a
c            Householder transformation, or Algorithm H2 to apply a
c            previously constructed transformation.
C     LPIVOT IS THE INDEX OF THE PIVOT ELEMENT. 
C     L1,M   IF L1 .LE. M   THE TRANSFORMATION WILL BE CONSTRUCTED TO   
C            ZERO ELEMENTS INDEXED FROM L1 THROUGH M.   IF L1 GT. M     
C            THE SUBROUTINE DOES AN IDENTITY TRANSFORMATION.
C     U(),IUE,UP    On entry with MODE = 1, U() contains the pivot
c            vector.  IUE is the storage increment between elements.  
c            On exit when MODE = 1, U() and UP contain quantities
c            defining the vector U of the Householder transformation.
c            on entry with MODE = 2, U() and UP should contain
c            quantities previously computed with MODE = 1.  These will
c            not be modified during the entry with MODE = 2.   
C     C()    ON ENTRY with MODE = 1 or 2, C() CONTAINS A MATRIX WHICH
c            WILL BE REGARDED AS A SET OF VECTORS TO WHICH THE
c            HOUSEHOLDER TRANSFORMATION IS TO BE APPLIED.
c            ON EXIT C() CONTAINS THE SET OF TRANSFORMED VECTORS.
C     ICE    STORAGE INCREMENT BETWEEN ELEMENTS OF VECTORS IN C().  
C     ICV    STORAGE INCREMENT BETWEEN VECTORS IN C().  
C     NCV    NUMBER OF VECTORS IN C() TO BE TRANSFORMED. IF NCV .LE. 0  
C            NO OPERATIONS WILL BE DONE ON C(). 
C     ------------------------------------------------------------------
      SUBROUTINE H12 (MODE,LPIVOT,L1,M,U,IUE,UP,C,ICE,ICV,NCV)  
C     ------------------------------------------------------------------
      integer I, I2, I3, I4, ICE, ICV, INCR, IUE, J
      integer L1, LPIVOT, M, MODE, NCV
      double precision B, C(*), CL, CLINV, ONE, SM
c     double precision U(IUE,M)
      double precision U(IUE,*)
      double precision UP
      parameter(ONE = 1.0d0)
C     ------------------------------------------------------------------
      IF (0.GE.LPIVOT.OR.LPIVOT.GE.L1.OR.L1.GT.M) RETURN    
      CL=abs(U(1,LPIVOT))   
      IF (MODE.EQ.2) GO TO 60   
C                            ****** CONSTRUCT THE TRANSFORMATION. ******
          DO 10 J=L1,M  
   10     CL=MAX(abs(U(1,J)),CL)  
      IF (CL) 130,130,20
   20 CLINV=ONE/CL  
      SM=(U(1,LPIVOT)*CLINV)**2   
          DO 30 J=L1,M  
   30     SM=SM+(U(1,J)*CLINV)**2 
      CL=CL*SQRT(SM)   
      IF (U(1,LPIVOT)) 50,50,40     
   40 CL=-CL
   50 UP=U(1,LPIVOT)-CL 
      U(1,LPIVOT)=CL    
      GO TO 70  
C            ****** APPLY THE TRANSFORMATION  I+U*(U**T)/B  TO C. ******
C   
   60 IF (CL) 130,130,70
   70 IF (NCV.LE.0) RETURN  
      B= UP*U(1,LPIVOT)
C                       B  MUST BE NONPOSITIVE HERE.  IF B = 0., RETURN.
C   
      IF (B) 80,130,130 
   80 B=ONE/B   
      I2=1-ICV+ICE*(LPIVOT-1)   
      INCR=ICE*(L1-LPIVOT)  
          DO 120 J=1,NCV
          I2=I2+ICV     
          I3=I2+INCR    
          I4=I3 
          SM=C(I2)*UP
              DO 90 I=L1,M  
              SM=SM+C(I3)*U(1,I)
   90         I3=I3+ICE 
          IF (SM) 100,120,100   
  100     SM=SM*B   
          C(I2)=C(I2)+SM*UP
              DO 110 I=L1,M 
              C(I4)=C(I4)+SM*U(1,I)
  110         I4=I4+ICE 
  120     CONTINUE  
  130 RETURN
      END   

