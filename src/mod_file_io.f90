!=================================================================================
! Written by Mark A. Herndon
! Lehigh University, Department of Mechanical Engineering and Mechanics
!=================================================================================
MODULE mod_file_io
    IMPLICIT NONE
    INTEGER :: SOL_WRITE_FREQ, s
    CHARACTER(:), ALLOCATABLE :: input_file_contents
CONTAINS

!=================================================================================
SUBROUTINE read_input_data
    USE mod_global, ONLY : nt, dt, nv, nvt, GE, &
                           Y_0, Z_0, eta_0, zeta_0, GAM, a, &
                           ALLOCATE_VARIABLES
    IMPLICIT NONE
    INTEGER :: i, i2

    NAMELIST /CODE_DATA/ nt, dt, nv, nvt, GE
    NAMELIST /VORTEX_DATA/ Y_0, Z_0, eta_0, zeta_0, GAM, a

    INQUIRE ( FILE = 'input_parameters.dat', SIZE=s )
    ALLOCATE ( CHARACTER(LEN=s) :: input_file_contents )

    OPEN(UNIT=3,FILE='input_parameters.dat',ACCESS='STREAM',ACTION='READ',STATUS='OLD')
    READ(3) input_file_contents
    CLOSE(3)

    DO i = 1, s
        IF (input_file_contents(i:i) == '!' ) THEN
           i2 = i + INDEX(input_file_contents(i:),ACHAR(10))-2
            input_file_contents(i:i2) = ''
        END IF
    END DO

    READ ( input_file_contents, NML=CODE_DATA )

    CALL ALLOCATE_VARIABLES

    READ ( input_file_contents, NML=VORTEX_DATA )

END SUBROUTINE read_input_data
!=================================================================================
SUBROUTINE WRITE_SOLUTION_FILE
    USE mod_global, ONLY : nv, nvt, nt, Y, Z, tau, eta, zeta
    IMPLICIT NONE

    OPEN(1,FILE='DATA/vortices.x',FORM='UNFORMATTED',ACCESS='STREAM',STATUS='REPLACE',ACTION='WRITE')
    WRITE(1) nvt, nt
    WRITE(1) Y
    WRITE(1) Z
    WRITE(1) eta
    WRITE(1) zeta
    WRITE(1) tau
    CLOSE(1)

END SUBROUTINE WRITE_SOLUTION_FILE
!=================================================================================
END MODULE mod_file_io
