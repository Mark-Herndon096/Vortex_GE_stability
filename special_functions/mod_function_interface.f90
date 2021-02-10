!=================================================================================
!   Author: Mark A. Herndon
!   Lehigh University, Department of Mechanical Engineering and Mechanics
!=================================================================================
MODULE mod_function_interface
    IMPLICIT NONE
    ! FUNCTION INTERFACES -- SPECIAL FUNCTIONS 
    INTERFACE
        FUNCTION BESSELJ0(x)
            REAL(KIND=8), INTENT(INOUT) :: x
            REAL(KIND=8)                :: BESSELJ0
        END FUNCTION BESSELJ0
    END INTERFACE

    INTERFACE
        FUNCTION BESSELJ1(x)
            REAL(KIND=8), INTENT(INOUT) :: x
            REAL(KIND=8)                :: BESSELJ1
        END FUNCTION BESSELJ1
    END INTERFACE

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

!   GLOBAL CONSTANTS FOR USE IN FUNCTION ROUTINES
!   CONSTANTS ARE COEFFICIENTS DERIVED FROM RATIONAL APPROXIMATIONS AND
!   ASYMPTOTIC APPROXIMATIONS OF BESSEL FUNCTIONS
!      References:
!      John F. Hart
!      Computer Approximations
!      year: 1978
!     
!      William H. Press, Saul A. Teuukolsky, William T. Vetterling, 
!      Brian P. Flannery
!      NUMERICAL RECIPES: The Art of Scientific Computing
!      ISBN: 978-0-521-88068-8
!  
!      Milton Abramowitz, Irene Stegun,
!      Handbook of Mathematical Functions,
!      National Bureau of Standards, 1964,
!      ISBN: 0-486-61272-4,
!      LC: QA47.A34.

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

    REAL(KIND=8), DIMENSION(5) :: j04 = (/1.0,                   & 
                                          1.040797262528109,     &
                                          2.588070904043728e-1,  &
    	                                  1.529954477721284e-2,  &
                                          1.168931211650012e-4/)

    REAL(KIND=8), DIMENSION(5) :: j05 = (/-1.562499999999992e-2, &
                                          -1.920039317065641e-2, &
    	                                  -5.827951791963418e-3, &
                                          -4.372674978482726e-4, &
                                          -3.895839560412374e-6/)

    REAL(KIND=8), DIMENSION(5) :: j06 = (/1.0,                   &
                                          1.237980436358390,     & 
                                          3.838793938147116e-1,  &
    	                                  3.100323481550864e-2,  &
                                          4.165515825072393e-4/)

    REAL(KIND=8), DIMENSION(7) :: j11 = (/7.309637831891357e-5,  &
                                          3.551248884746503e-6,  & 
    	                                  5.820673901730427e-8,  &
                                          4.500650342170622e-10, &
                                          1.831596352149641e-12, &
    	                                  3.891583573305035e-15, &
                                          3.524978592527982e-18/)

    REAL(KIND=8), DIMENSION(7) :: j12 = (/1.0,                   &
                                          9.398354768446072e-3,  &
                                          4.328946737100230e-5,  &
    	                                  1.271526296341915e-7,  & 
                                          2.566305357932989e-10, &
    	                                  3.477378203574266e-13, &
                                          2.593535427519985e-16/)

    REAL(KIND=8), DIMENSION(5) :: j13 = (/1.0,                   &
                                          1.014039111045313,     &
                                          2.426762348629863e-1,  &
    	                                  1.350308200342000e-2,  &
                                          9.516522033988099e-5/)

    REAL(KIND=8), DIMENSION(5) :: j14 = (/1.0,                   &
                                          1.012208056357845,     &
                                          2.408580305488938e-1,  &
    	                                  1.309511056184273e-2,  &
                                          7.746422941504713e-5/)

    REAL(KIND=8), DIMENSION(5) :: j15 = (/4.687499999999991e-2,  &
                                          5.652407388406023e-2,  &
    	                                  1.676531273460512e-2,  &
                                          1.231216817715814e-3,  &
                                          1.178364381441801e-5/)

    REAL(KIND=8), DIMENSION(5) :: j16 = (/1.0,                   &
                                          1.210119370463693,     &
                                          3.626494789275638e-1,  &
    	                                  2.761695824829316e-2,  &
                                          3.240517192670181e-4/)

    
    REAL(KIND=8), DIMENSION(5) :: k01 = (/1.0,                   &
                                          2.346487949187396e-1,  &
                                          1.187082088663404e-2,  &
        	                              2.150707366040937e-4,  &
                                          1.425433617130587e-6/)
    
    REAL(KIND=8), DIMENSION(3) :: k02 = (/9.847324170755358e-1,  &
                                          1.518396076767770e-2,  &
        	                              8.362215678646257e-5/)
    
    REAL(KIND=8), DIMENSION(5) :: k03 = (/1.159315156584126e-1,  &
                                          2.770731240515333e-1,  &
        	                              2.066458134619875e-2,  &
                                          4.574734709978264e-4,  &
                                          3.454715527986737e-6/)
    
    REAL(KIND=8), DIMENSION(3) :: k04 = (/9.836249671709183e-1,  &
                                          1.627693622304549e-2,  &
        	                              9.809660603621949e-5/)
    
    REAL(KIND=8), DIMENSION(8) :: k05 = (/1.253314137315499,     &
                                          1.475731032429900e1,   &
        	                              6.123767403223466e1,   &
                                          1.121012633939949e2,   &
                                          9.285288485892228e1,   &
        	                              3.198289277679660e1,   &
                                          3.595376024148513,     &
                                          6.160228690102976e-2/)
    
    REAL(KIND=8), DIMENSION(8) :: k06 = (/1.0,                   &
                                          1.189963006673403e1,   &
                                          5.027773590829784e1,   &
        	                              9.496513373427093e1,   &
                                          8.318077493230258e1,   &
                                          3.181399777449301e1,   &
        	                              4.443672926432041,     &
                                          1.408295601966600e-1/)
    
    REAL(KIND=8), DIMENSION(5) :: k11 = (/0.5,                   &
                                          5.598072040178741e-2,  &
                                          1.818666382168295e-3,  &
        	                              2.397509908859959e-5,  &
                                          1.239567816344855e-7/)
    
    REAL(KIND=8), DIMENSION(3) :: k12 = (/9.870202601341150e-1,  &
                                          1.292092053534579e-2,  &
        	                              5.881933053917096e-5/)
    
    REAL(KIND=8), DIMENSION(5) :: k13 = (/-3.079657578292062e-1, &
                                          -8.109417631822442e-2, &
        	                              -3.477550948593604e-3, &
                                          -5.385594871975406e-5, &
                                          -3.110372465429008e-7/)
    
    REAL(KIND=8), DIMENSION(3) :: k14 = (/9.861813171751389e-1,  &
                                          1.375094061153160e-2,  &
        	                              6.774221332947002e-5/)
    
    REAL(KIND=8), DIMENSION(8) :: k15 = (/1.253314137315502,     &
                                          1.457171340220454e1,   &
                 		 	              6.063161173098803e1,   &
                                          1.147386690867892e2,   &
                                          1.040442011439181e2,   &
    			                          4.356596656837691e1,   &
                                          7.265230396353690,     &
                                          3.144418558991021e-1/)
    
    REAL(KIND=8), DIMENSION(8) :: k16 = (/1.0,                   &
                                          1.125154514806458e1,   &
                                          4.427488496597630e1,   &
    	                                  7.616113213117645e1,   &
                                          5.863377227890893e1,   &
                                          1.850303673841586e1,   &
    	                                  1.857244676566022,     &
                                          2.538540887654872e-2/)
!==================================================================
!==================================================================
END MODULE mod_function_interface
