MODULE mod_special_function_interface
    IMPLICIT NONE

    INTERFACE BESSELJ0
        FUNCTION bessel_j0_wrapper(x) BIND(C)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_DOUBLE
            IMPLICIT NONE
            REAL(C_DOUBLE), INTENT(IN) :: x
            REAL(C_DOUBLE) :: bessel_j0_wrapper
        END 
    END INTERFACE

    INTERFACE BESSELJ1
        FUNCTION bessel_j1_wrapper(x) BIND(C)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_DOUBLE
            IMPLICIT NONE
            REAL(C_DOUBLE), INTENT(IN) :: x
            REAL(C_DOUBLE) :: bessel_j1_wrapper
        END 
    END INTERFACE
    INTERFACE BESSELJN
        FUNCTION bessel_jn_wrapper(n, x) BIND(C)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_DOUBLE
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: n
            REAL(C_DOUBLE), INTENT(IN) :: x
            REAL(C_DOUBLE)             :: bessel_jn_wrapper
        END 
    END INTERFACE
    INTERFACE BESSELY0
        FUNCTION bessel_y0_wrapper(x) BIND(C)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_DOUBLE
            IMPLICIT NONE
            REAL(C_DOUBLE), INTENT(IN) :: x
            REAL(C_DOUBLE) :: bessel_y0_wrapper
        END 
    END INTERFACE

    INTERFACE BESSELY1
        FUNCTION bessel_y1_wrapper(x) BIND(C)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_DOUBLE
            IMPLICIT NONE
            REAL(C_DOUBLE), INTENT(IN) :: x
            REAL(C_DOUBLE) :: bessel_y1_wrapper
        END 
    END INTERFACE
    INTERFACE BESSELYN
        FUNCTION bessel_yn_wrapper(n, x) BIND(C)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_DOUBLE
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: n
            REAL(C_DOUBLE), INTENT(IN) :: x
            REAL(C_DOUBLE)             :: bessel_yn_wrapper
        END 
    END INTERFACE
    INTERFACE BESSELI0
        FUNCTION bessel_i0_wrapper(x) BIND(C)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_DOUBLE
            IMPLICIT NONE
            REAL(C_DOUBLE), INTENT(IN) :: x
            REAL(C_DOUBLE) :: bessel_i0_wrapper
        END 
    END INTERFACE

    INTERFACE BESSELI1
        FUNCTION bessel_i1_wrapper(x) BIND(C)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_DOUBLE
            IMPLICIT NONE
            REAL(C_DOUBLE), INTENT(IN) :: x
            REAL(C_DOUBLE) :: bessel_i1_wrapper
        END 
    END INTERFACE
    INTERFACE BESSELIN
        FUNCTION bessel_in_wrapper(n, x) BIND(C)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_DOUBLE
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: n
            REAL(C_DOUBLE), INTENT(IN) :: x
            REAL(C_DOUBLE)             :: bessel_in_wrapper
        END 
    END INTERFACE
    INTERFACE BESSELK0
        FUNCTION bessel_k0_wrapper(x) BIND(C)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_DOUBLE
            IMPLICIT NONE
            REAL(C_DOUBLE), INTENT(IN) :: x
            REAL(C_DOUBLE) :: bessel_k0_wrapper
        END 
    END INTERFACE
    INTERFACE BESSELK1
        FUNCTION bessel_k1_wrapper(x) BIND(C)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_DOUBLE
            IMPLICIT NONE
            REAL(C_DOUBLE), INTENT(IN) :: x
            REAL(C_DOUBLE) :: bessel_k1_wrapper
        END 
    END INTERFACE
    INTERFACE BESSELKN
        FUNCTION bessel_kn_wrapper(n, x) BIND(C)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_DOUBLE
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: n
            REAL(C_DOUBLE), INTENT(IN) :: x
            REAL(C_DOUBLE) :: bessel_kn_wrapper
        END 
    END INTERFACE

END MODULE mod_special_function_interface


