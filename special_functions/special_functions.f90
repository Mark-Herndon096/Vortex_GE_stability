FUNCTION BESSELJ0(x)
    USE mod_function_interface, ONLY : j01
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN) :: x
    REAL(KIND=8)             :: BESSELJ0
   
    WRITE(*,*) 'Value of j01: ', j01 
    BESSELJ0 = x 
END FUNCTION BESSELJ0
