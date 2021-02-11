PROGRAM MAIN
    USE mod_special_function_interface, ONLY : BESSELJ0, BESSELJ1, BESSELJN, &
                                               BESSELY0, BESSELY1, BESSELYN, &
                                               BESSELI0, BESSELI1, BESSELIN, &
                                               BESSELK0, BESSELK1, BESSELKN
    IMPLICIT NONE
    INTEGER :: ni, i
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: x, j0, j1, j2, &
                                                  y0, y1, y2, &
                                                  i0, i1, i2, &
                                                  k0, k1, k2
    REAL(KIND=8) :: xspan, dx
    xspan = 40.d0
    ni = 100000; dx = xspan/(REAL(ni-1,KIND=8));
    ALLOCATE(x(ni))
    ALLOCATE(j0(ni))
    ALLOCATE(j1(ni))
    ALLOCATE(j2(ni))
    ALLOCATE(y0(ni))
    ALLOCATE(y1(ni))
    ALLOCATE(y2(ni))
    ALLOCATE(i0(ni))
    ALLOCATE(i1(ni))
    ALLOCATE(i2(ni))
    ALLOCATE(k0(ni))
    ALLOCATE(k1(ni))
    ALLOCATE(k2(ni))
    DO i = 1, ni
        x(i)  = REAL(i,KIND=8)*dx
        j0(i) = BESSELJ0(x(i))
        j1(i) = BESSELJ1(x(i))
        j2(i) = BESSELJN(2,x(i))

        y0(i) = BESSELY0(x(i))
        y1(i) = BESSELY1(x(i))
        y2(i) = BESSELYN(2,x(i))

        i0(i) = BESSELI0(x(i))
        i1(i) = BESSELI1(x(i))
        i2(i) = BESSELIN(2,x(i))

        k0(i) = BESSELK0(x(i))
        k1(i) = BESSELK1(x(i))
        k2(i) = BESSELKN(2,x(i))
    END DO

    OPEN(1,FILE='BESSEL.X',FORM='UNFORMATTED',ACTION='WRITE',STATUS='REPLACE',ACCESS='STREAM')
    WRITE(1) ni
    WRITE(1) x
    WRITE(1) j0
    WRITE(1) j1
    WRITE(1) j2
    WRITE(1) y0
    WRITE(1) y1
    WRITE(1) y2
    WRITE(1) i0
    WRITE(1) i1
    WRITE(1) i2
    WRITE(1) k0
    WRITE(1) k1
    WRITE(1) k2
    CLOSE(1)
END PROGRAM MAIN
