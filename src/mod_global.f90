!=================================================================================
! Written by Mark A. Herndon
! Lehigh University, Department of Mechanical Engineering and Mechanics
!=================================================================================
MODULE mod_global
    IMPLICIT NONE
    ! CONSTANTS
    REAL(KIND=8), PARAMETER :: pi = 4.0*ATAN(1.0)

    ! USER-SPECIFIED PARAMETERS 
    INTEGER :: nt   !< # of time steps
    INTEGER :: nv   !< # of vortices in real plane
    INTEGER :: nvt  !< Total # of vortices in ground-image system
    LOGICAL :: GE   !< In Ground Effect Logical 
    
    REAL(KIND=8) :: dt  !< Time step 
    REAL(KIND=8) :: a   !< Vortex core radius (Will be an array for unequal core radii in future studies)

    ! USER-SPECIFIED INITIAL CONDITIONS
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: Y_0    !< Y initial locations 
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: Z_0    !< Z initial locations
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: eta_0  !< eta inital perturbation amplitude
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: zeta_0 !< zeta initial perturbation amplitude

    ! VORTEX POSITION AND PERTURBATION AMPLITUDE ARRAYS
    ! DIMENSION(nvt,nt) --> Ex. Y(vortex index, time index) == position of 
    ! vortex (vortex index) at t = (time index)
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: Y    !< Y locations 
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: Z    !< Z locations
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: eta  !< eta perturbation amplitude
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: zeta !< zeta perturbation amplitude
    
    ! VORTEX CIRCULATION STRENGTH AND ORIENTATION
    ! DIMENSION(nvt) --> Ex. GAM(vortex index 1) ...
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: GAM    !< Array containing vortex circulation

    ! DERIVED PARAMETERS
    REAL(KIND=8) :: b     !< Initial vortex separation (relevant to vortex pair cases)
    REAL(KIND=8) :: h     !< Initial vortex height relative to global coordinate system
    INTEGER      :: m     !< Dimension of VORT array
    ! GLOBAL VARIABLES 
    INTEGER :: n    !< Time integration indexing integer
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: VORT_0
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: VORT_new
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: tau
CONTAINS

!=================================================================================
SUBROUTINE ALLOCATE_VARIABLES
    IMPLICIT NONE

    ! ALLOCATE INITIAL POSITION ARRAYS
    ALLOCATE(Y_0(nvt))
    ALLOCATE(Z_0(nvt))
    ALLOCATE(eta_0(nvt))
    ALLOCATE(zeta_0(nvt))
    
    ! ALLOCATE VORTEX POSITION AND PERTURBATION AMPLITUDE HISTORY ARRAYS
    ALLOCATE(Y(nvt,nt))
    ALLOCATE(Z(nvt,nt))
    ALLOCATE(eta(nvt,nt))
    ALLOCATE(zeta(nvt,nt))

    ! ALLOCATE VORTEX CIRCULATION ARRAY
    ALLOCATE(GAM(nvt))

    ! ALLOCATE VORT ARRAYS FOR RK5 INTEGRATOR
    m = nvt*2
    ALLOCATE(VORT_0(m))
    ALLOCATE(VORT_new(m))
    ALLOCATE(tau(nt))

END SUBROUTINE ALLOCATE_VARIABLES
!=================================================================================
END MODULE mod_global
