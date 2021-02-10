!=============================================================================
!   Author: Mark A. Herndon
!   Lehigh University, Department of Mechanical Engineering and Mechanics
!   
!   Ideas, methods, and numerical constants based from the following
!   References:
!
!   John F. Hart
!   Computer Approximations
!   year: 1978
!   
!   William H. Press, Saul A. Teuukolsky, William T. Vetterling, 
!   Brian P. Flannery
!   NUMERICAL RECIPES: The Art of Scientific Computing
!   ISBN: 978-0-521-88068-8
!
!   Milton Abramowitz, Irene Stegun,
!   Handbook of Mathematical Functions,
!   National Bureau of Standards, 1964,
!   ISBN: 0-486-61272-4,
!=============================================================================
FUNCTION BESSELJ0(x)
    USE mod_function_interface, ONLY : j01, j02, j03, j04, j05,  &
                                       j06,                      &
                                       xj00, xj10, pio4, twoopi, &
                                       RATIONAL_FIT,             &
                                       ASYMPTOTIC_APPROX
    IMPLICIT NONE
    REAL(KIND=8), INTENT(INOUT) :: x
    REAL(KIND=8)                :: BESSELJ0
    REAL(KIND=8)                :: ax, xx, np, dp, nq, dq, y, ff
    INTEGER                     :: n = 7
    INTEGER                     :: m = 5

    IF ( ABS(x) .LE. 8.0 ) THEN
        CALL RATIONAL_FIT(x, j01, j02, n, np, dp, y)
        BESSELJ0 = np*(y-xj00)*(y-xj10)/dp
    END IF
    IF ( ABS(x) .GT. 8.0 ) THEN
        ax = ABS(x); xx = ax - pio4; ff = 1.d0;
        CALL ASYMPTOTIC_APPROX(x, j03, j04, j05, j06, m, np, dp, nq, dq, ff)
        BESSELJ0 = SQRT(twoopi/ax)*(COS(xx)*np/dp - (8.d0/ax)*SIN(xx)*nq/dq)
    END IF
END FUNCTION BESSELJ0
!=============================================================================
!=============================================================================
FUNCTION BESSELJ1(x)
    USE mod_function_interface, ONLY : j11, j12, j13, j14, j15,  &
                                       j16,                      &
                                       xj01, xj11, pio4, twoopi, &
                                       RATIONAL_FIT,             &
                                       ASYMPTOTIC_APPROX
    IMPLICIT NONE
    REAL(KIND=8), INTENT(INOUT) :: x
    REAL(KIND=8)                :: BESSELJ1
    REAL(KIND=8)                :: ax, xx, np, dp, nq, dq, y, ff
    INTEGER                     :: n = 7
    INTEGER                     :: m = 5

    IF ( ABS(x) .LE. 8.0 ) THEN
        CALL RATIONAL_FIT(x, j11, j12, n, np, dp, y)
        BESSELJ1 = x*np*(y-xj01)*(y-xj11)/dp
    END IF
    IF ( ABS(x) .GT. 8.0 ) THEN
        ax = ABS(x); ff = 3.d0; xx = ax - ff*pio4;
        CALL ASYMPTOTIC_APPROX(x, j13, j14, j15, j16, m, np, dp, nq, dq, ff)
        BESSELJ1 = SQRT(twoopi/ax)*(COS(xx)*np/dp - (8.d0/ax)*SIN(xx)*nq/dq)
    END IF
END FUNCTION BESSELJ1
!=============================================================================
!=============================================================================
SUBROUTINE RATIONAL_FIT(x, r, s, n, np, dp, y)
    IMPLICIT NONE
    REAL(KIND=8),               INTENT(IN)  :: x
    REAL(KIND=8), DIMENSION(n), INTENT(IN)  :: r
    REAL(KIND=8), DIMENSION(n), INTENT(IN)  :: s
    INTEGER,                    INTENT(IN)  :: n
    REAL(KIND=8),               INTENT(OUT) :: np, dp, y
    INTEGER                                 :: i
    REAL(KIND=8)                            :: z

    y  = x**2
    z  = 64.d0 - y
    np = r(n)
    dp = s(n)

    DO i = n-1, 1, -1
        np = np*z + r(i)
        dp = dp*y + s(i)
    END DO


END SUBROUTINE RATIONAL_FIT
!=============================================================================
!=============================================================================
SUBROUTINE ASYMPTOTIC_APPROX(x, rn, rd, sn, sd, n, np, dp, nq, dq, ff)
    USE mod_function_interface, ONLY : pio4
    IMPLICIT NONE
    REAL(KIND=8),               INTENT(IN)  :: x, ff
    REAL(KIND=8), DIMENSION(n), INTENT(IN)  :: rn, rd
    REAL(KIND=8), DIMENSION(n), INTENT(IN)  :: sn, sd
    INTEGER,                    INTENT(IN)  :: n
    REAL(KIND=8),               INTENT(OUT) :: np, dp, nq, dq
    REAL(KIND=8)                            :: ax, xx, y, z
    INTEGER                                 :: i

    ax = ABS(x); xx = ax - ff*pio4;
    
    z  = 8.d0/ax
    y  = z**2
    
    np = rn(5); dp = rd(5);
    nq = sn(5); dq = sd(5); 

    DO i = 4, 1, -1
        np = np*y + rn(i)
        dp = dp*y + rd(i)
        nq = nq*y + sn(i)
        dq = dq*y + sd(i)
    END DO

END SUBROUTINE ASYMPTOTIC_APPROX
!=============================================================================
!=============================================================================
