!=================================================================================
! Written by Mark A. Herndon
! Lehigh University, Department of Mechanical Engineering and Mechanics
!=================================================================================
PROGRAM MAIN
    USE mod_file_io, ONLY : read_input_data
    USE mod_global,  ONLY : nt, dt, nv, nvt, Y_0, Z_0, GE, GAM
    IMPLICIT NONE

    INTEGER :: i

    CALL read_input_data
    IF ( GE == .TRUE. ) THEN
        CALL SET_GROUND_EFFECT
    END IF    

END PROGRAM MAIN
!=================================================================================
SUBROUTINE SET_GROUND_EFFECT
    USE mod_global, ONLY : nv, nvt, Y_0, Z_0, GAM
    IMPLICIT NONE
    INTEGER :: j

    DO j = nv+1, nvt
        Y_0(j) =  Y_0(j-nv)
        Z_0(j) = -Z_0(j-nv)
        GAM(j) = -GAM(j-nv)
    END DO

END SUBROUTINE SET_GROUND_EFFECT
!=================================================================================
