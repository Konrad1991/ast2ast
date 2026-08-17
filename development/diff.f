       double precision FUNCTION DIFF(X,Y)
c
c  Function used in tests that depend on machine precision.
c
c  The original version of this code was developed by
c  Charles L. Lawson and Richard J. Hanson at Jet Propulsion Laboratory
c  1973 JUN 7, and published in the book
c  "SOLVING LEAST SQUARES PROBLEMS", Prentice-HalL, 1974.
c  Revised FEB 1995 to accompany reprinting of the book by SIAM.
C
      double precision X, Y
      DIFF=X-Y  
      RETURN
      END   
