PROGRAM MAIN
    USE mod_function_interface, ONLY : BESSELJ0
    IMPLICIT NONE
    INTEGER      :: ni, i
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: x, y
    REAL(KIND=8)                            :: dx, xspan

    ni    = 10000
    xspan = 70
    
    dx    = xspan/(REAL(ni - 1,KIND=8))
    
    ALLOCATE(x(ni))
    ALLOCATE(y(ni))


    DO i = 1, ni
        x(i) = REAL(i-1,KIND=8)*dx
        y(i) = BESSELJ0(x(i))
    END DO 
  
    
    OPEN(1,FILE='bessel.x',FORM='UNFORMATTED',ACCESS='STREAM',ACTION='WRITE',STATUS='REPLACE')
    WRITE(1) ni
    WRITE(1) x
    WRITE(1) y
    CLOSE(1)    
    

    
    

END PROGRAM MAIN
