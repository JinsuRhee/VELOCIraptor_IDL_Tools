!1234567        
       SUBROUTINE get_contam(larr, darr, dir_raw, xc, yc, zc, rr, &
                       dom_list, confrac, conf_r)

       USE omp_lib

!!-----
!! GLOBAL VARIABLES
!!-----
       IMPLICIT NONE

       REAL(KIND=8) darr(20)
       INTEGER(KIND=4) larr(20)

       REAL(KIND=8) xc(larr(1)), yc(larr(1)), zc(larr(1))
       REAL(KIND=8) rr(larr(1))

       INTEGER(KIND=4) dom_list(larr(1), larr(2))
       REAL(KIND=8) confrac(larr(1), larr(3))
       REAL(KIND=8) conf_r(larr(3))

       CHARACTER(LEN=larr(12)) dir_raw
!!-----
!! LOCAL VARIABLES
!!-----

       INTEGER(KIND=4) i, j, k, l, m
       INTEGER(KIND=4) n_gal, n_mpi, n_aper, n_thread, n_snap
       INTEGER(KIND=4) uout, npart
       REAL(KIND=8) dmp_mass, D2
       REAL(KIND=4) dmct_all(larr(3)), dmct_aper(larr(3))
       REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: tmp_dbl
       CHARACTER(LEN=100) domnum, fdum, snum, fname

       n_gal    = larr(1)
       n_mpi    = larr(2)
       n_aper   = larr(3)
       n_thread = larr(4)
       n_snap   = larr(11)
       
       dmp_mass = darr(1)

       CALL OMP_SET_NUM_THREADS(n_thread)

       !!-----
       !! WRITE FILE NAME
       !!-----
       WRITE(snum, '(I5.5)') n_snap
       fname    = TRIM(dir_raw)//'output_'//TRIM(snum)//'/part_'// &
               TRIM(snum)//'.out'
       !!-----
       !! MAIN LOOP
       !!-----
       DO i=1, n_gal

         uout   = OMP_GET_THREAD_NUM() + 10
         !! LOOP FOR DOMAINS
         dmct_all = 0
         dmct_aper= 0
         DO j=1, n_mpi
           IF(dom_list(i,j) .LT. 0) CYCLE

             !! OPEN FILE
             WRITE(domnum, '(I5.5)') j
             fdum       = TRIM(fname)//TRIM(domnum)
             OPEN(UNIT=uout, FILE=fdum, FORM='unformatted', STATUS='old')
             READ(uout); READ(uout); READ(uout) npart; READ(uout);
             READ(uout); READ(uout); READ(uout); READ(uout);

             IF(npart .EQ. 0) THEN
               CLOSE(uout)
               CYCLE
             ENDIF

             !! MEMORY ALLOCATE
             ALLOCATE(tmp_dbl(1:npart, 1:9))

             !! READ
             !!!! X, Y, Z
             READ(uout) tmp_dbl(1:npart, 1:1)
             READ(uout) tmp_dbl(1:npart, 2:2)
             READ(uout) tmp_dbl(1:npart, 3:3)

             !!!! VX, VY, VZ
             READ(uout); READ(uout); READ(uout)

             !!!! MASS
             READ(uout) tmp_dbl(1:npart, 4:4)

             !!!! ID, LEVEL
             READ(uout); READ(uout)

             !!!! FAM, TAG
             IF(larr(19) .GE. 10) THEN
               READ(uout); READ(uout)
             ENDIF

             !!!! AGE
             READ(uout) tmp_dbl(1:npart, 5:5)
             CLOSE(10)

             !! LOOP FOR PARTICLE
             !$OMP PARALLEL DO default(shared) &
             !$OMP & schedule(static,10) private(D2, l) &
             !$OMP & reduction(+:dmct_aper, dmct_all)
             DO k=1, npart
               IF(tmp_dbl(k,5) .NE. 0) CYCLE
               IF(tmp_dbl(k,4) .LT. 0.9*dmp_mass) CYCLE

               D2 = (tmp_dbl(k,1) - xc(i))**2 + &
                       (tmp_dbl(k,2) - yc(i))**2 + &
                       (tmp_dbl(k,3) - zc(i))**2
               D2 = SQRT(D2) / rr(i)

               DO l=1, n_aper
                 IF(D2 .GT. conf_r(l)) CYCLE
                 dmct_all(l)    = dmct_all(l) + 1
                 IF(tmp_dbl(k,4) .GT. 1.1*dmp_mass) &
                        dmct_aper(l) = dmct_aper(l) + 1
               ENDDO
             ENDDO
             !$OMP END PARALLEL DO
           DEALLOCATE(tmp_dbl)
         ENDDO

         DO l=1, n_aper
           IF(dmct_all(l) .EQ. 0) THEN
             confrac(i,l) = 0.
           ELSE
             confrac(i,l) = dmct_aper(l) / dmct_all(l)
           ENDIF
         ENDDO
       ENDDO
       RETURN
       END SUBROUTINE

