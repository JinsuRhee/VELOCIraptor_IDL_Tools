!234567 
      Subroutine prop_sfr(xc, yc, zc, rr, bin, uin, pos, mass, gyr, &
                      sfr, &
                      larr, darr)

      use omp_lib
      implicit none

      integer(kind=4) larr(20)
      real(kind=8) darr(20)
        
      real(kind=8) xc(larr(1)), yc(larr(1)), zc(larr(1)), rr(larr(1))
      real(kind=8) pos(larr(2),3), mass(larr(2)), gyr(larr(2))
      real(kind=8) sfr(larr(1))
      integer(kind=4) bin(larr(1),2), uin(larr(1),2)

      !!!!!!
      integer(kind=4) i, j, k, l
      integer(kind=4) n_gal, n_part, n_thread
      real(kind=8) dum

      n_gal     = larr(1)
      n_part    = larr(2)
      n_thread  = larr(3)
      call omp_set_num_threads(n_thread)

      !$OMP PARALLEL DO default(shared) private(dum, j) schedule(static)
      Do i=1, n_gal
        Do j=bin(i,1)+1, bin(i,2)+1
          IF(mass(j) .GT. 0) THEN
            If(gyr(j) .lt. darr(11) .and. gyr(j) .ge. 0) then
              IF(darr(1) .LT. 0) THEN
                sfr(i) = sfr(i) + mass(j)
              ELSE
                dum = (pos(j,1) - xc(i))**2 + &
                        (pos(j,2) - yc(i))**2 + &
                        (pos(j,3) - zc(i))**2
                dum = sqrt(dum)
                IF(dum .LT. rr(i)*darr(1)) &
                        sfr(i) = sfr(i) + mass(j)
              ENDIF
            Endif
          ENDIF
        Enddo

        Do j=uin(i,1)+1, uin(i,2)+1
          IF(mass(j) .GT. 0) THEN
            If(gyr(j) .lt. darr(11) .and. gyr(j) .ge. 0) then
              IF(darr(1) .LT. 0) THEN
                sfr(i) = sfr(i) + mass(j)
              ELSE
                dum = (pos(j,1) - xc(i))**2 + &
                        (pos(j,2) - yc(i))**2 + &
                        (pos(j,3) - zc(i))**2
                dum = sqrt(dum)
                IF(dum .LT. rr(i)*darr(1)) &
                        sfr(i) = sfr(i) + mass(j)
              ENDIF
            Endif
          ENDIF
        Enddo

        sfr(i) = sfr(i) / darr(11) / 1.0d9
      Enddo
      !$OMP END PARALLEL DO

      Return
      End subroutine
