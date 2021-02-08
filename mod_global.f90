MODULE mod_global
    IMPLICIT NONE
    ! USER-SPECIFIED PARAMETERS 
    INTEGER :: nt   !< # of time steps
    INTEGER :: nv   !< # of vortices in real plane
    INTEGER :: nvt  !< Total # of vortices in ground-image system
    INTEGER :: n    !< Time integration indexing integer

    REAL(KIND=8) :: tspan !< Final integration time
    REAL(KIND=8) :: dt    !< Time step 
    
    ! VORTEX POSITION AND PERTURBATION AMPLITUDE ARRAYS
    ! DIMENSION(nvt,nt) --> Ex) Y(vortex index, time index) == position of 
    ! vortex (vortex index at t = (time index)
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: Y    !< Y locations 
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: Z    !< Z locations
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: eta  !< eta perturbation amplitude
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: zeta !< zeta perturbation amplitude

    REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: Y_0    !< Y initial locations 
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: Z_0    !< Z initial locations
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: eta_0  !< eta inital perturbation amplitude
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: zeta_0 !< zeta initial perturbation amplitude
END MODULE mod_global
