!1234567        
       SUBROUTINE get_contam(larr, darr, dir_raw, xc, yc, zc, rr, &
                       xp, mp, conf_n, conf_m, conf_r)

       USE omp_lib

!!-----
!! GLOBAL VARIABLES
!-----
       IMPLICIT NONE

       REAL(KIND=8) darr(20)
       INTEGER(KIND=4) larr(20)

       REAL(KIND=8) xc(larr(1)), yc(larr(1)), zc(larr(1))
       REAL(KIND=8) rr(larr(1))

       REAL(KIND=8) xp(larr(2), 3), mp(larr(2))
       REAL(KIND=8) conf_n(larr(1), larr(3))
       REAL(KIND=8) conf_m(larr(1), larr(3))
       REAL(KIND=8) conf_r(larr(3))

       CHARACTER(LEN=larr(12)) dir_raw
!!-----
!! LOCAL VARIABLES
!!-----

       INTEGER(KIND=4) i, j, k, l, m
       INTEGER(KIND=4) n_gal, n_dm, n_aper, n_thread, n_snap
       REAL(KIND=8) dmp_mass, D2
       REAL(KIND=8) c_nall(larr(3)), c_n(larr(3))
       REAL(KIND=8) c_mall(larr(3)), c_m(larr(3))
       REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: tmp_dbl
       CHARACTER(LEN=100) domnum, fdum, snum, fname
       INTEGER(KIND=4) rd_dlist(larr(2))

       n_gal    = larr(1)
       n_dm    = larr(2)
       n_aper   = larr(3)
       n_thread = larr(4)
       n_snap   = larr(11)
       
       dmp_mass = darr(1)

       CALL OMP_SET_NUM_THREADS(n_thread)

       !!-----
       !! MAIN LOOP
       !!-----
       DO i=1, n_gal

             c_nall = 0.
             c_mall = 0.
             c_n = 0.
             c_m = 0.
             !! LOOP FOR PARTICLE
             !$OMP PARALLEL DO default(shared) &
             !$OMP & schedule(static) private(D2, l) &
             !$OMP & reduction(+:c_nall, c_mall) &
             !$OMP & reduction(+:c_n, c_m)
             DO k=1, n_dm
               D2 = (xp(k,1) - xc(i))**2 + &
                       (xp(k,2) - yc(i))**2 + &
                       (xp(k,3) - zc(i))**2
               D2 = SQRT(D2) / rr(i)
               DO l=1, n_aper
                 IF(D2 .GT. conf_r(l)) CYCLE
                 c_nall(l) = c_nall(l) + 1
                 c_mall(l) = c_mall(l) + mp(k)

                 IF(mp(k) .GT. 1.1*dmp_mass) THEN
                   c_n(l) = c_n(l) + 1
                   c_m(l) = c_m(l) + mp(k)
                 ENDIF
               ENDDO
             ENDDO
             !$OMP END PARALLEL DO

             DO l=1, n_aper
               IF(c_nall(l) .EQ. 0) THEN
                 conf_n(i,l) = 0.
               ELSE
                 conf_n(i,l) = c_n(l) / c_nall(l)
               ENDIF

               IF(c_mall(l) .EQ. 0) THEN
                 conf_m(i,l) = 0.
               ELSE
                 conf_m(i,l) = c_m(l) / c_mall(l)
               ENDIF
             ENDDO
       ENDDO
       RETURN
       END SUBROUTINE

