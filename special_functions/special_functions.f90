!==================================================================
!==================================================================
FUNCTION BESSELJ0(x)
    USE mod_function_interface, ONLY : j01, j02, j03, j04, j05, &
                                       j06,                     &
                                       RATIONAL_FIT
    IMPLICIT NONE
    REAL(KIND=8), INTENT(INOUT) :: x
    REAL(KIND=8)                :: BESSELJ0
    REAL(KIND=8)                :: np, dp, y
    INTEGER                     :: n = 7
    INTEGER                     :: m = 5

    IF ( ABS(x) .LE. 8.0 ) THEN
        CALL RATIONAL_FIT(x, j01, j02, n, np, dp, y)
        BESSELJ0 = y
    END IF

END FUNCTION BESSELJ0

 
!==================================================================
!==================================================================
SUBROUTINE RATIONAL_FIT(x, r, s, n, np, dp, y)
    REAL(KIND=8),               INTENT(IN) :: x
    REAL(KIND=8), DIMENSION(n), INTENT(IN) :: r
    REAL(KIND=8), DIMENSION(n), INTENT(IN) :: s
    INTEGER,                    INTENT(IN) :: n
    REAL(KIND=8), INTENT(OUT)              :: np, dp, y
    INTEGER                                :: i
    y  = x**2
    z  = 64.d0 - y
    np = r(n)
    dp = s(n)

    DO i = n-1, 1, -1
        np = np*z + r(i)
        dp = dp*y + s(i)
    END DO


END SUBROUTINE RATIONAL_FIT
!==================================================================
!==================================================================
