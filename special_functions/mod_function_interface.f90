MODULE mod_function_interface
    IMPLICIT NONE
    ! FUNCTION INTERFACES -- SPECIAL FUNCTIONS 
    INTERFACE
        FUNCTION BESSELJ0(x)
            REAL(KIND=8), INTENT(INOUT) :: x
            REAL(KIND=8)                :: BESSELJ0
        END FUNCTION BESSELJ0
    END INTERFACE

    ! UTILITY PROGRAM INTERFACES 
    INTERFACE
        SUBROUTINE RATIONAL_FIT(x, r, s, n, np, dp, y)
            REAL(KIND=8),               INTENT(IN)  :: x
            REAL(KIND=8), DIMENSION(n), INTENT(IN)  :: r
            REAL(KIND=8), DIMENSION(n), INTENT(IN)  :: s
            INTEGER,                    INTENT(IN)  :: n
            REAL(KIND=8),               INTENT(OUT) :: np, dp, y
        END SUBROUTINE RATIONAL_FIT
    END INTERFACE

    INTERFACE
        SUBROUTINE ASYMPTOTIC_APPROX(x, rn, rd, sn, sd, n, np, dp, nq, dq, ff)
            REAL(KIND=8),               INTENT(IN)  :: x, ff
            REAL(KIND=8), DIMENSION(n), INTENT(IN)  :: rn, rd
            REAL(KIND=8), DIMENSION(n), INTENT(IN)  :: sn, sd
            INTEGER,                    INTENT(IN)  :: n
            REAL(KIND=8),               INTENT(OUT) :: np, dp, nq, dq
        END SUBROUTINE ASYMPTOTIC_APPROX
    END INTERFACE

    ! GLOBAL CONSTANTS FOR USE IN FUNCTION ROUTINES
    REAL(KIND=8), PARAMETER :: xj00   =  5.783185962946785
    REAL(KIND=8), PARAMETER :: xj10   =  3.047126234366209e1
    REAL(KIND=8), PARAMETER :: xj01   =  1.468197064212389e1
    REAL(KIND=8), PARAMETER :: xj11   =  4.921845632169460e1
    REAL(KIND=8), PARAMETER :: twoopi =  0.6366197723675813
    REAL(KIND=8), PARAMETER :: pio4   =  0.7853981633974483

    REAL(KIND=8), DIMENSION(7) :: j01 = (/1.682397144220462e-4,   &
                                          2.058861258868952e-5,   &
                                          5.288947320067750e-7,   &
                                          5.557173907680151e-9,   &
                                          2.865540042042604e-11,  &
    	                                  7.398972674152181e-14,  & 
                                          7.925088479679688e-17/)

    REAL(KIND=8), DIMENSION(7) :: j02 = (/1.0,                    &
                                          1.019685405805929e-2,   &
                                          5.130296867064666e-5,   &
     	                                  1.659702063950243e-7,   &
                                          3.728997574317067e-10,  &
     	                                  5.709292619977798e-13,  &
                                          4.932979170744996e-16/)

    REAL(KIND=8), DIMENSION(5) :: j03 = (/9.999999999999999e-1,   & 
                                          1.039698629715637,      &
    	                                  2.576910172633398e-1,   & 
                                          1.504152485749669e-2,   & 
                                          1.052598413585270e-4/)

    REAL(KIND=8), DIMENSION(5) :: j04  = (/1.0,                   & 
                                           1.040797262528109,     &
                                           2.588070904043728e-1,  &
    	                                   1.529954477721284e-2,  &
                                           1.168931211650012e-4/)

    REAL(KIND=8), DIMENSION(5) :: j05  = (/-1.562499999999992e-2, &
                                           -1.920039317065641e-2, &
    	                                   -5.827951791963418e-3, &
                                           -4.372674978482726e-4, &
                                           -3.895839560412374e-6/)

    REAL(KIND=8), DIMENSION(5) :: j06  = (/1.0,                   &
                                           1.237980436358390,     & 
                                           3.838793938147116e-1,  &
    	                                   3.100323481550864e-2,  &
                                           4.165515825072393e-4/)
!==================================================================
!==================================================================
!==================================================================
END MODULE mod_function_interface
