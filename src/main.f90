! [[file:../PROJECT.org::*MAIN.F90][MAIN.F90:1]]
!=================================================================================
! Written by Mark A. Herndon
! Lehigh University, Department of Mechanical Engineering and Mechanics
!=================================================================================
PROGRAM MAIN
    USE mod_file_io,                ONLY : read_input_data, WRITE_SOLUTION_FILE
    USE mod_global,                 ONLY : nt, dt, nv, nvt, Y_0, Z_0, GE, GAM, Y, &
                                           Z, VORT_0, VORT_new, n, m, tau, ka,    &
                                           eta, zeta, eta_0, zeta_0
    USE mod_numerical_routines,     ONLY : DERIVATIVE, RK5
    USE special_function_interface, ONLY : BESSELJ0, BESSELJ1
    IMPLICIT NONE
    PROCEDURE(DERIVATIVE) :: VORTEX_DERIV
    INTEGER               :: i

    CALL read_input_data
    IF ( GE == .TRUE. ) THEN
        CALL SET_GROUND_EFFECT
    END IF
    ka = 0.3d0
    CALL CALC_OMEGA

    tau(1) = 0.0

    DO i = 1, nvt
        VORT_0(i)       = Y_0(i)
        VORT_0(i+nvt  ) = Z_0(i)
        VORT_0(i+2*nvt) = eta_0(i)
        VORT_0(i+3*nvt) = zeta_0(i)
        Y(i,1)          = Y_0(i)
        Z(i,1)          = Z_0(i)
        eta(i,1)        = eta_0(i)
        zeta(i,1)       = zeta_0(i)
   END DO

    DO n = 1, nt
        CALL Rk5(VORT_0,VORT_new,dt,m,VORTEX_DERIV)
        DO i = 1, nvt
            VORT_0(i)       = VORT_new(i)
            VORT_0(i+nvt)   = VORT_new(i+nvt)
            VORT_0(i+2*nvt) = VORT_new(i+2*nvt)
            VORT_0(i+3*nvt) = VORT_new(i+3*nvt)
            Y(i,n+1)        = VORT_new(i)
            Z(i,n+1)        = VORT_new(i+nvt)
            eta(i,n+1)      = VORT_new(i+2*nvt)
            zeta(i,n+1)     = VORT_new(i+3*nvt)
        END DO
        tau(n+1) = dt*REAL(n,KIND=8)
    END DO

    CALL WRITE_SOLUTION_FILE

END PROGRAM MAIN
!=================================================================================
SUBROUTINE SET_GROUND_EFFECT
    USE mod_global, ONLY : nv, nvt, Y_0, Z_0, GAM, eta_0, zeta_0
    IMPLICIT NONE
    INTEGER :: j

    DO j = nv+1, nvt
        Y_0(j)    =  Y_0(j-nv)
        Z_0(j)    = -Z_0(j-nv)
        eta_0(j)  = eta_0(j-nv)
        zeta_0(j) = eta_0(j-nv)
        GAM(j)    = -GAM(j-nv)
    END DO

END SUBROUTINE SET_GROUND_EFFECT
!=================================================================================
SUBROUTINE CALC_OMEGA
    USE mod_numerical_routines, ONLY : BISECTION_METHOD, root_function
    USE mod_global, ONLY : omega, ka 
    IMPLICIT NONE
    PROCEDURE(root_function) :: BESSEL_ROOT
    PROCEDURE(root_function) :: DISPERSION
    PROCEDURE(root_function) :: VALIDATE
    REAL(KIND=8) :: x, y, a, b, tol, eps, bessel_root_val
    eps = 1E-10;
    tol = 1E-14;
    a   = 1.d0;
    b   = 5.d0;

    CALL BISECTION_METHOD(BESSEL_ROOT, x, a, b, tol)
    WRITE(*,*) 'Root found at x = ', x
    bessel_root_val = x;
    b = bessel_root_val - eps;
    WRITE(*,*) 'Finding roots of dispersion relation'
    CALL BISECTION_METHOD(DISPERSION, x, a, b, tol)
    WRITE(*,*) 'Root found at x = ', x, 'For ka = ', ka
    WRITE(*,*) 'Validating root by explicit calculation'
    y = VALIDATE(x)
    WRITE(*,*) 'The value of f(beta_root) = ', y
    omega = ((2*ka/SQRT(ka**2 + x**2)) - 1.d0)

END SUBROUTINE CALC_OMEGA
!=================================================================================
! VORTEX_DERIV (y_1, y_2, ... , y_nvt, z_1, z_2, ... z_nvt, eta_1, eta_2, ... , eta_nvt
!               zeta_1, zeta_2, ... , zeta_nvt)
!=================================================================================
FUNCTION VORTEX_DERIV(x_0,m,h,ch)
    USE mod_global, ONLY : GE, pi, nv, nvt, GAM, mutual_induction, ka, omega
    IMPLICIT NONE
    INTEGER,                    INTENT(IN)    :: m
    REAL(KIND=8),               INTENT(IN)    :: h, ch
    REAL(KIND=8), DIMENSION(m), INTENT(IN)    :: x_0
    REAL(KIND=8), DIMENSION(m)                :: VORTEX_DERIV
    ! FUNCTION SPECIFIC VARIABLES AND PARAMETERS
    PROCEDURE(mutual_induction)               :: PSI, PHI    !< Special functions
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:)   :: y_temp      !< Temporary y array
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:)   :: z_temp      !< Temporary z array
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:)   :: eta_temp    !< Temporary eta array
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:)   :: zeta_temp   !< Temporary zeta array

    REAL(KIND=8), ALLOCATABLE, DIMENSION(:)   :: y_deriv     !< y derivative array
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:)   :: z_deriv     !< z derivative array
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:)   :: eta_deriv   !< eta derivative array
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:)   :: zeta_deriv  !< zeta derivative array
    INTEGER                                   :: i, j        !< Loop index integers
    REAL(KIND=8)                              :: y_mn, z_mn  !< Relative y and z coordinates
    REAL(KIND=8)                              :: r_mn        !< Relative radius
    REAL(KIND=8)                              :: sum_y       !< Sum holder for y eq
    REAL(KIND=8)                              :: sum_z       !< Sum holder for y eq
    REAL(KIND=8)                              :: sum_eta     !< Sum holder for y eq
    REAL(KIND=8)                              :: sum_zeta    !< Sum holder for y eq
    REAL(KIND=8)                              :: V1_mn       !< First term for eta equation
    REAL(KIND=8)                              :: V2_mn       !< Second term for eta equation
    REAL(KIND=8)                              :: V3_mn       !< Third term for eta equation
    REAL(KIND=8)                              :: V4_mn       !< Fourth term for eta equation
    REAL(KIND=8)                              :: W1_mn       !< First term for zeta equation
    REAL(KIND=8)                              :: W2_mn       !< Second term for zeta equation
    REAL(KIND=8)                              :: W3_mn       !< Third term for zeta equation
    REAL(KIND=8)                              :: W4_mn       !< Fourth term for zeta equation
    REAL(KIND=8)                              :: a, k
    
    a = 0.1d0; k = ka/a;

    ALLOCATE(y_temp(nvt))
    ALLOCATE(z_temp(nvt))
    ALLOCATE(eta_temp(nvt))
    ALLOCATE(zeta_temp(nvt))

    ALLOCATE(y_deriv(nvt))
    ALLOCATE(z_deriv(nvt))
    ALLOCATE(eta_deriv(nvt))
    ALLOCATE(zeta_deriv(nvt))

    ! Initialize sum holder values to 0.0
    sum_y = 0.0; sum_z = 0.0; sum_eta = 0.0; sum_zeta = 0.0;

    ! Map x_0 values into temporary arrays
    DO i = 1, nvt
        y_temp(i)    = x_0(i)
        z_temp(i)    = x_0(i+nvt)
        eta_temp(i)  = x_0(i+2*nvt)
        zeta_temp(i) = x_0(i+3*nvt)
    END DO

    DO i = 1, nvt
        DO j = 1, nvt
            IF ( i .EQ. j ) THEN
                CYCLE
            ELSEIF ( i .NE. j ) THEN
                z_mn  = z_temp(j) - z_temp(i)
                y_mn  = y_temp(j) - y_temp(i)
                r_mn  = SQRT(y_mn**2 + z_mn**2)
                


                sum_y = sum_y + (GAM(j)/(2.0*pi))*z_mn/r_mn**2
                sum_z = sum_z - (GAM(j)/(2.0*pi))*y_mn/r_mn**2

                V1_mn =  (GAM(j)/(2.d0*pi))*(2.d0*(z_mn**2)/r_mn**4 - 1/r_mn**2)*zeta_temp(i)
                W1_mn = -(GAM(j)/(2.d0*pi))*(2.d0*(y_mn**2)/r_mn**4 - 1/r_mn**2)*eta_temp(i)

                V2_mn =  (GAM(j)/(2.d0*pi*r_mn**2))*(PSI(k*r_mn) - (z_mn/r_mn**2)*PHI(k*r_mn))*zeta_temp(i)
                W2_mn = -(GAM(j)/(2.d0*pi*r_mn**2))*(PSI(k*r_mn) - (z_mn/r_mn**2)*PHI(k*r_mn))*eta_temp(i)

                V3_mn =  (GAM(j)/(pi*r_mn**4))*y_mn*z_mn*eta_temp(i)
                W3_mn = -(GAM(j)/(pi*r_mn**4))*y_mn*z_mn*zeta_temp(i)

                V4_mn = (GAM(j)/(2.d0*pi*r_mn**2))*(-y_mn*z_mn/r_mn**2)*PHI(k*r_mn)*eta_temp(j)
                W4_mn = (GAM(j)/(2.d0*pi*r_mn**2))*(y_mn*z_mn/r_mn**2)*PHI(k*r_mn)*zeta_temp(j)

                sum_eta  = sum_eta  + V1_mn + V3_mn + V4_mn + V2_mn
                sum_zeta = sum_zeta + W1_mn + W3_mn + W4_mn + W2_mn
            END IF
        END DO
        y_deriv(i)    = sum_y
        z_deriv(i)    = sum_z
        eta_deriv(i)  = sum_eta  + GAM(i)/(2.d0*pi*a**2)*0.01d0*zeta_temp(i) 
        zeta_deriv(i) = sum_zeta - GAM(i)/(2.d0*pi*a**2)*0.01d0*eta_temp(i) 
        sum_y         = 0.d0
        sum_z         = 0.d0
        sum_eta       = 0.d0
        sum_zeta      = 0.d0
    END DO

    DO i = 1, nvt
        VORTEX_DERIV(i)        = y_deriv(i)
        VORTEX_DERIV(i+nvt)    = z_deriv(i)
        VORTEX_DERIV(i+2*nvt) = eta_deriv(i)
        VORTEX_DERIV(i+3*nvt) = zeta_deriv(i)
    END DO

    DEALLOCATE(y_temp)
    DEALLOCATE(z_temp)
    DEALLOCATE(eta_temp)
    DEALLOCATE(zeta_temp)
    DEALLOCATE(y_deriv)
    DEALLOCATE(z_deriv)
    DEALLOCATE(eta_deriv)
    DEALLOCATE(zeta_deriv)
END FUNCTION VORTEX_DERIV
!=================================================================================
!===========================================================================
FUNCTION BESSEL_ROOT(x)
    USE special_function_interface, ONLY : BESSELJ1
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN) :: x
    REAL(KIND=8)             :: BESSEL_ROOT
    REAL(KIND=8)             :: y

    y = BESSELJ1(x)
    BESSEL_ROOT = y    
END FUNCTION
!===========================================================================
!===========================================================================
FUNCTION DISPERSION(beta)
    USE special_function_interface, ONLY : BESSELJ0, BESSELJ1, BESSELJN, &
                                           BESSELK0, BESSELK1, BESSELKN
    USE mod_global, ONLY : ka
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN) :: beta
    REAL(KIND=8)             :: DISPERSION
    REAL(KIND=8)             :: J0, J1, J2, J1_p 
    REAL(KIND=8)             :: K0, K1, K2, K1_p 

    J0 = BESSELJ0(beta); J1 = BESSELJ1(beta); J2 = BESSELJN(2,beta);
    K0 = BESSELK0(ka);   K1 = BESSELK1(ka);   K2 = BESSELKN(2,ka);

    J1_p = (J0 - J2)/2.d0;
    K1_p = (K0 + K2)/2.d0;

    DISPERSION = (1.d0/beta)*(J1_p/J1) + K1_p/(ka*K1) + SQRT(beta**2 + (ka)**2)/(ka*beta**2) 
END FUNCTION
!===========================================================================
!===========================================================================
FUNCTION VALIDATE(beta)
    USE special_function_interface, ONLY : BESSELJ0, BESSELJ1, BESSELJN, &
                                           BESSELK0, BESSELK1, BESSELKN
    USE mod_global, ONLY : ka
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN) :: beta
    REAL(KIND=8)             :: VALIDATE
    REAL(KIND=8)             :: J0, J1, J2, J1_p 
    REAL(KIND=8)             :: K0, K1, K2, K1_p 

    J0 = BESSELJ0(beta); J1 = BESSELJ1(beta); J2 = BESSELJN(2,beta);
    K0 = BESSELK0(ka);   K1 = BESSELK1(ka);   K2 = BESSELKN(2,ka);

    J1_p = (J0 - J2)/2.d0;
    K1_p = (K0 + K2)/2.d0;

    VALIDATE = (1.d0/beta)*(J1_p/J1) + K1_p/(ka*K1) + SQRT(beta**2 + (ka)**2)/(ka*beta**2) 
END FUNCTION
!===========================================================================
FUNCTION PSI(beta)
    USE special_function_interface, ONLY : BESSELK0, BESSELK1
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN) :: beta
    REAL(KIND=8)             :: PSI
    IF ( beta == 0.d0 ) THEN
        PSI = 1.d0
    ELSE
        PSI = (beta**2)*BESSELK0(ABS(beta)) + ABS(beta)*BESSELK1(ABS(beta))
    END IF
END FUNCTION 
!===========================================================================
FUNCTION PHI(beta)
    USE special_function_interface, ONLY : BESSELKN
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN) :: beta
    REAL(KIND=8)             :: PHI
    
    IF ( beta == 0.d0 ) THEN
        PHI = 1.d0
    ELSE
        PHI = (beta**2)*BESSELKN(2,ABS(beta))
    END IF
END FUNCTION 
!===========================================================================
! MAIN.F90:1 ends here
