PROGRAM main
  USE HTYPE
  USE subs
  USE parameters


  IMPLICIT NONE


  !-----------------------------------------------------------!
  !                 LOOK-UP TABLE VARIABLES                   !
  !-----------------------------------------------------------!
  REAL(DP), DIMENSION(0:N-1, 0:N-1):: T1, T2, U1, U2
  REAL(DP), DIMENSION(0:Nb, 0:Nb):: T1n, T2n, U1n, U2n
  REAL(DP), DIMENSION(0:2*Nb+1, 0:2*Nb+1) :: Mn



  !-----------------------------------------------------------!
  !                    FLUID VELOCITIES                       !
  !-----------------------------------------------------------!
  REAL(DP), DIMENSION(0:N-1, 0:N-1):: u0, v0


  !-----------------------------------------------------------!
  !                     BOUNDARY VARIABLES                    !
  !-----------------------------------------------------------!
  REAL(DP), DIMENSION(0:2*Nb+1) :: bdry0, bdry

  !-----------------------------------------------------------!
  !                 BOUNDARY UPDATE QUANTITY                  !
  !-----------------------------------------------------------!
  REAL(DP), DIMENSION(0:2*Nb+1):: Bn

  !-----------------------------------------------------------!
  !                       VARIOUS INDECES                     !
  !-----------------------------------------------------------!
  INTEGER(DP) :: i,j, s
  INTEGER(DP) :: time

  character(6) :: filename
  integer :: fcount = 1

  REAL(DP), PARAMETER :: tf = 2





  CALL PG_COEFF_MATRIX(p_coeff, g_coeff)

  CALL T_MATRIX(T1, T2)
  CALL U_MATRIX(U1, U2)


  ! DEFINE INITAL VELOCITY
  u0 = 0.0
  v0 = 0.0

  ! DEFINE INTIAL BOUNDARY CONFIG.
  DO s= 0, Nb
     IF( s == 0)THEN
        bdry0(s) = 0.0 + .1 
        bdry0(s+Nb+1) = 1.  
     ELSE
        bdry0(s) = bdry0(s-1) + x0_quad(s*hb)
        bdry0(s+Nb+1) = bdry0(s-1 + Nb+1) + y0_quad(s*hb)
     ENDIF
  ENDDO


  ! FOR EACH TIME STEP
  DO time = 1,int(tf/dt)
     print*, time*dt

     CALL Mn_MATRIX(T1n, T2n, U1n, U2n, T1,T2, U1,U2, bdry0)
     Mn(0:Nb, 0:Nb) = T1n
     Mn(Nb+1:2*Nb+1, 0:Nb) = T2n
     Mn(0:Nb, Nb+1:2*Nb+1) = U1n
     Mn(Nb+1:2*Nb+1, Nb+1:2*Nb+1) = U2n


     !-------------------------------------------------!
     !                  UPDATE BOUNDARY                !
     !                  UPDATE FLUID                   !
     !-------------------------------------------------!
     CALL Bn_VECTOR(Bn, u0, v0, bdry0)
     CALL NEWTON(bdry0, bdry, time*dt, Mn, Bn)
     bdry0=bdry
     CALL FLUID_SOLVE(u0, v0, bdry, time*dt)

     


!!$     ! - writes swimmer location to local directory './data'
!!$     ! - for use in animation 'swimmer.gif'
!!$     IF(mod(time,int(tf/dt)/100) == 0)THEN
!!$        if(fcount < 10)then
!!$           WRITE(filename, "(I1,A4)") fcount,'.txt'
!!$        else
!!$           WRITE(filename, "(I2,A4)") fcount,'.txt'
!!$        endif
!!$
!!$        OPEN(44, file = "./data/"//trim(filename), status = 'replace')
!!$        DO s= 0, Nb
!!$           write(44,"(2(1x,f15.9))") bdry(s), bdry(s+Nb+1)
!!$        ENDDO
!!$        CLOSE(44)
!!$        fcount = fcount + 1
!!$     ENDIF


  ENDDO !TIME

ENDPROGRAM main
