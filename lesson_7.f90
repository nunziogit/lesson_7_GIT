! Exercise, Lesson 7
! Array rank 2

PROGRAM l7_c1
   !
   ! This program calculates the mean and the standard deviation
   ! of 2*n (unknown) numbers stored in the file dati.dat
   ! over two columns, and writes the results to the file output.dat.
   ! The mean is calculated using a function, and the standard deviation
   ! is calculated using a subroutine. The data from dati.dat is stored
   ! in a rank-2 array using static arrays. The data is passed through
   ! the argument list.
   !----------------------------------------------------------------------
   ! Variable declarations

   INTEGER :: ierror                          ! I/O status
   INTEGER :: icont                           ! Counter for data in the file
   INTEGER :: n                               ! Maximum number of data points
   INTEGER, PARAMETER :: max_size = 100       ! Maximum size of the array
   REAL, DIMENSION(2, max_size) :: x          ! Rank-2 array for data

   REAL :: media1, media2, media3                     ! Mean values for the two columns
   REAL :: dev_std_1, dev_std_2               ! Standard deviation for the two columns
   REAL :: average                      ! Function to calculate the mean

   ! Open the file dati.dat (the file already exists and is read-only)
   OPEN(UNIT=3, FILE='dati.dat', STATUS='old', ACTION='read', IOSTAT=ierror)

   openif: IF (ierror == 0) THEN
      ! The file was opened successfully: read the values

      ! Read the file dati.dat (number of data points is unknown)
      ! The sole purpose of this routine is to identify n
      icont = 1  ! Initialize counter
      dati_in: DO
         READ(3,*,IOSTAT=ierror) x(1,icont), x(2,icont)  ! Read the data from row icont
         IF (ierror /= 0) EXIT
         icont = icont + 1
      END DO dati_in

      ! The DO loop has terminated: was it due to a read error or the end of the file?
      readif: IF (ierror > 0) THEN
         WRITE(*,*) 'Error at line', icont
      ELSE readif  ! The end of the data was reached correctly
         WRITE(*,1010) icont - 1
      END IF readif
1010  FORMAT(' ', "End of data reached. There are:", 1x, I6, 1x, "data points in the file")

      ! Assign the number of data points read to n
      n = icont - 1

      ! Check that the number of data points does not exceed the maximum array size
      IF (n > max_size) THEN
         WRITE(*,*) 'WARNING!!! Number of data points > maximum array size'
         WRITE(*,*) 'WARNING!!! Increase max_size!!!!'
         STOP
      END IF

      ! Calculate the means for both columns by calling the calcola_media function
      media1 = average(x(1,:), n)
      media2 = average(x(2,:), n)
      media3 = average(x(2,:)+ x(1,:), n)

      ! Calculate the standard deviations for both columns by calling the calcola_varianza subroutine
      CALL calcola_varianza(x(1,:), media1, n, dev_std_1)
      CALL calcola_varianza(x(2,:), media2, n, dev_std_2)

      ! Write the mean and standard deviation results to a file in a specific format
      OPEN(UNIT=4, FILE='output.dat', STATUS='replace', ACTION='write')
      WRITE(4,1030) media1, media2
      WRITE(4,1040) dev_std_1, dev_std_2
      CLOSE(4)
      WRITE (*,*) '-----------------------------------'
      WRITE (*,*) 'The code has successfully completed. The results can be found in the output.dat file'
1030  FORMAT("The means of the data are:", 1x, E10.3, 1x, E10.3)
1040  FORMAT("The standard deviations of the data are:", 1x, ES10.3, 1x, ES10.3)

   ELSE openif
      ! Close the file in case of an error
      CLOSE(3)
      WRITE(*,*) "Error opening the file: please check the data"
   END IF openif

   STOP
END PROGRAM l7_c1
!--------------------------------------------------------------------------
REAL FUNCTION average(array, n)
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: n
   REAL, INTENT(IN), DIMENSION(n) :: array
   ! Calculate the mean
   average = sum(array) / REAL(n)
   
   RETURN
END FUNCTION average
!--------------------------------------------------------------------------

