PROGRAM MAIN
    USE mod_function_interface, ONLY : BESSELJ0
    IMPLICIT NONE
    REAL(KIND=8) :: x, y

    WRITE(*,*) ' Input value for x'
    READ(*,*) x
    
    y = BESSELJ0(x)

    WRITE(*,*) 'Output from BESSELJ0: ', y
    
    

END PROGRAM MAIN
