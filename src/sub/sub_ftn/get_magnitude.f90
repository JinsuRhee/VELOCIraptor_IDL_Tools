!234567
      Subroutine get_magnitude(rr, xx, yy, zz, bin, uin, pos, flux, mag, larr, darr)

      use omp_lib
      implicit none

      integer(kind=4) larr(20)
      real(kind=8) darr(20)

      real(kind=8) rr(larr(1)), mag(larr(1))
      real(kind=8) xx(larr(1)), yy(larr(1)), zz(larr(1))
      real(kind=8) pos(larr(2),3), flux(larr(2))

      integer(kind=4) bin(larr(1),2), uin(larr(1),2)

      !!!!!!
      integer(kind=4) i, j, k, l
      integer(kind=4) n_thread, n_gal, n_part
      real(kind=8) fl_dum, dist_dum

      n_gal     = larr(1)
      n_part    = larr(2)
      n_thread  = larr(3)

      call omp_set_num_threads(n_thread)

      !$OMP PARALLEL DO default(shared) private(fl_dum, dist_dum, j) schedule(static)
      Do i=1, n_gal
        fl_dum = 0.
        mag(i) = -1d8

        DO j=bin(i,1)+1, bin(i,2)+1
          IF(flux(j) .GT. -1.0d7) THEN
            IF(darr(1) .LT. 0.) THEN
              fl_dum = fl_dum + flux(j)
            ELSE
              dist_dum = (pos(j,1) - xx(i))**2 + &
                      (pos(j,2) - yy(i))**2 + &
                      (pos(j,3) - zz(i))**2
              dist_dum = sqrt(dist_dum)
              IF(dist_dum .lt. rr(i)*darr(1)) fl_dum = fl_dum + flux(j)
            ENDIF
          ENDIF
        ENDDO

        DO j=uin(i,1)+1, uin(i,2)+1
          IF(flux(j) .GT. -1.0d7) THEN
            IF(darr(1) .LT. 0.) THEN
              fl_dum = fl_dum + flux(j)
            ELSE
              dist_dum = (pos(j,1) - xx(i))**2 + &
                      (pos(j,2) - yy(i))**2 + &
                      (pos(j,3) - zz(i))**2
              dist_dum = sqrt(dist_dum)
              IF(dist_dum .lt. rr(i)*darr(1)) fl_dum = fl_dum + flux(j)
            ENDIF
          ENDIF
        ENDDO

        if(fl_dum .gt. 0) mag(i) = -2.5 * dlog10(fl_dum)
      ENDDO
      !$OMP END PARALLEL DO

      Return
      End subroutine

