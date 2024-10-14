SUBROUTINE calcola_varianza(array, media, n, varianza)
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: n
   REAL, INTENT(IN), DIMENSION(n) :: array
   REAL, INTENT(IN) :: media
   REAL, INTENT(OUT) :: varianza
   REAL :: varianza2
   ! Calculate the standard deviation
   varianza = SQRT(sum((array - media)**2) / REAL(n))
   varianza2 = SQRT(sum((array - media)**3) / REAL(n))
   RETURN
END SUBROUTINE calcola_varianza
