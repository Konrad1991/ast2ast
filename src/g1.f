      SUBROUTINE G1 (A,B,CTERM,STERM,SIG)   
c
C     COMPUTE ORTHOGONAL ROTATION MATRIX..  
c
c  The original version of this code was developed by
c  Charles L. Lawson and Richard J. Hanson at Jet Propulsion Laboratory
c  1973 JUN 12, and published in the book
c  "SOLVING LEAST SQUARES PROBLEMS", Prentice-HalL, 1974.
c  Revised FEB 1995 to accompany reprinting of the book by SIAM.
C   
C     COMPUTE.. MATRIX   (C, S) SO THAT (C, S)(A) = (SQRT(A**2+B**2))   
C                        (-S,C)         (-S,C)(B)   (   0          )    
C     COMPUTE SIG = SQRT(A**2+B**2) 
C        SIG IS COMPUTED LAST TO ALLOW FOR THE POSSIBILITY THAT 
C        SIG MAY BE IN THE SAME LOCATION AS A OR B .
C     ------------------------------------------------------------------
      double precision A, B, CTERM, ONE, SIG, STERM, XR, YR, ZERO
      parameter(ONE = 1.0d0, ZERO = 0.0d0)
C     ------------------------------------------------------------------
      if (abs(A) .gt. abs(B)) then
         XR=B/A
         YR=sqrt(ONE+XR**2)
         CTERM=sign(ONE/YR,A)
         STERM=CTERM*XR
         SIG=abs(A)*YR     
         RETURN
      endif

      if (B .ne. ZERO) then
         XR=A/B
         YR=sqrt(ONE+XR**2)
         STERM=sign(ONE/YR,B)
         CTERM=STERM*XR
         SIG=abs(B)*YR     
         RETURN
      endif

      SIG=ZERO  
      CTERM=ZERO  
      STERM=ONE   
      RETURN
      END   


