MODULE mod_function_interface
    IMPLICIT NONE
    ! FUNCTION INTERFACES 
    INTERFACE
        FUNCTION BESSELJ0(x)
            REAL(KIND=8), INTENT(IN) :: x
            REAL(KIND=8)             :: BESSELJ0
        END FUNCTION BESSELJ0
    END INTERFACE
    ! GLOBAL CONSTANTS FOR USE IN FUNCTION ROUTINES
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

    REAL(KIND=8), DIMENSION(5) ::j05  = (/-1.562499999999992e-2,  &
                                          -1.920039317065641e-2,  &
    	                                  -5.827951791963418e-3,  &
                                          -4.372674978482726e-4,  &
                                          -3.895839560412374e-6/)

const double  Bessel::j0qd[]  = {1.0,1.237980436358390,3.838793938147116e-1,
    	                         3.100323481550864e-2,4.165515825072393e-4};

END MODULE mod_function_interface
