!234567
      SUBROUTINE f_rdgas(larr, darr, dx, x, y, z, mass, val, map, xr, yr)

      USE omp_lib
      IMPLICIT NONE
      INTEGER(KIND=4) larr(20)
      REAL(KIND=8) darr(20)

      REAL(KIND=8) dx(larr(1)), x(larr(1)), y(larr(1)), z(larr(1))
      REAL(KIND=8) mass(larr(1)), val(larr(1), 3)      ! DEN, TEMP, METAL
      REAL(KIND=8) map(larr(2),larr(2))
      REAL(KIND=8) xr(2), yr(2)

!!!!!
!! LOCAL VARIABLES
!!!!!
      INTEGER(KIND=4) i, j, k, nn, nx, ny, n_pix
      REAL(KIND=8) mindx, mass_map(larr(2),larr(2))

      mindx = darr(1)
      n_pix = larr(2)
      map = 0.
      mass_map = 0.

        ! nn, nx, ny, j
        ! map, mass_map
      DO i=1, larr(1)
        IF(x(i)**2 + y(i)**2 + z(i)**2 .LT. xr(2)**2) THEN
          nn = ((dx(i) + mindx*0.1) / mindx)
          nx = ((x(i) - xr(1)) / mindx) + 1
          ny = ((y(i) - yr(1)) / mindx) + 1

          DO j=MAX(nx - nn,1), MIN(nx + nn, n_pix)
          DO k=MAX(ny - nn,1), MIN(ny + nn, n_pix)
            IF(larr(20) .EQ. 1) THEN !DENSITY
              map(j,k) = map(j,k) + val(i,1) * &
                      val(i,1) * mindx**3
              mass_map(j,k) = mass_map(j,k) + val(i,1) * mindx**3
            ENDIF

            IF(larr(20) .EQ. 2) THEN !TEMPERATURE
              map(j,k) = map(j,k) + val(i,2) * &
                      val(i,1) * mindx**3
              mass_map(j,k) = mass_map(j,k) + val(i,1) * mindx**3
            ENDIF
          ENDDO
          ENDDO
        ENDIF
      ENDDO

      !IF(larr(20) .NE. 1) THEN
        DO i=1, n_pix
          DO j=1, n_pix
            IF(mass_map(i,j) .GT. 0) map(i,j) = map(i,j) / mass_map(i,j)
          ENDDO
        ENDDO
      !ENDIF

      RETURN
      END
