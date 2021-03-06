MODULE subs
  USE HTYPE
  USE parameters


  IMPLICIT NONE

CONTAINS

  FUNCTION delta(x)
    REAL(DP):: x, delta
    IF(ABS(x) < 2*h) THEN
       delta = ( 1 + cos( PI_D*x/(2*h) ) )/(4*h)
    ELSE
       delta = 0.0
    ENDIF
  ENDFUNCTION delta


  SUBROUTINE PG_COEFF_MATRIX(p_coeff_mat, g_coeff_mat)
    COMPLEX(DP), DIMENSION(0:N-1, 0:N-1):: p_coeff_mat
    COMPLEX(DP), DIMENSION(0:N-1, 0:N-1):: g_coeff_mat
    INTEGER(DP) :: i,j

    DO i = 0, N-1
       DO j = 0, N-1
          p_coeff_mat(i,j) = ( 1.0/(h**2) )*(exp( CMPLX(0.0, TWOPI_D*i/REAL(N),DP)) &
               + exp( CMPLX(0.0, -TWOPI_D*i/REAL(N),DP)) &
               + exp( CMPLX(0.0, TWOPI_D*j/REAL(N),DP)) &
               + exp( CMPLX(0.0, -TWOPI_D*j/REAL(N),DP)) - 4)

          g_coeff_mat(i,j) = ( 1.0/(h**2) )*(exp( CMPLX(0.0, TWOPI_D*i/REAL(N),DP)) &
               + exp( CMPLX(0.0, -TWOPI_D*i/REAL(N),DP)) &
               + exp( CMPLX(0.0, TWOPI_D*j/REAL(N),DP)) &
               + exp( CMPLX(0.0, -TWOPI_D*j/REAL(N),DP)) - 4)
       ENDDO
    ENDDO
  ENDSUBROUTINE PG_COEFF_MATRIX


  SUBROUTINE U_MATRIX(U1, U2)
    REAL(DP), DIMENSION(0:N-1, 0:N-1), INTENT(INOUT) :: U1, U2

    REAL(DP), DIMENSION(0:N-1, 0:N-1):: A1, g1, g2
    COMPLEX(DP), DIMENSION(0:Nc-1, 0:N-1):: A1_hat, P_hat, g1_hat, g2_hat

    !COEFFICIENT FOR COMP. OF P_hat (fourier symbols)
    COMPLEX(DP) :: lap_coeff, p_rhs
    COMPLEX(DP) :: g1_rhs, g2_rhs

    INTEGER(DP) :: plan_forward_A1
    INTEGER(DP) :: plan_backward_g1
    INTEGER(DP) :: plan_backward_g2


    INTEGER(DP) :: i,j, index_i, index_j !handle periodicity
    INTEGER(DP) :: l,m

    REAL(DP) :: x,y
    REAL(DP) :: sum_in, sum_out

    !STEP 1 - BUILD THE A1 MATRIX.  THIS IS WHAT IS BEING FED
    !         TO THE FLUID SOLVER IN EQ (42)
    A1 = 0.0
    DO i = -3, 3
       x = x0 +i*h
       index_i = modulo(i,N)

       DO j = -3, 3
          y = y0 + j*h
          index_j = modulo(j,N)

          A1(index_i,index_j) = delta(x)*delta(y)

       ENDDO
    ENDDO

    !STEP 2 - BUILD A1_hat
    CALL dfftw_plan_dft_r2c_2d_ (plan_forward_A1, N, N, A1, A1_hat, fftw_estimate)
    CALL dfftw_execute(plan_forward_A1)


    !STEP 3 - COMPUTE P_hat
    P_hat(0,0) = CMPLX(0.0,0.0,DP)
    DO i = 0, Nc-1
       DO j = 0,N-1

          IF(i .NE. 0 .OR. j .NE. 0)THEN

             p_rhs = -(exp( CMPLX(0.0, TWOPI_D*j/REAL(N),DP)) - &
                  exp( CMPLX(0.0, -TWOPI_D*j/REAL(N),DP)) )*A1_hat(i,j)/(2*h)

             P_hat(i,j) = p_rhs/p_coeff(i,j)

          ENDIF
       ENDDO
    ENDDO


    !STEP 4 - COMPUTE g1_hat AND g2_hat
    g1_hat(0,0) = CMPLX(0.0,0.0,DP)
    g2_hat(0,0) = CMPLX(0.0,0.0,DP)
    DO i = 0, Nc-1
       DO j = 0,N-1
          IF(i .NE. 0 .OR. j .NE. 0)THEN
             g1_rhs = (1.0/(2*h) )*(exp( CMPLX(0.0, TWOPI_D*i/REAL(N),DP)) &
                  -exp( CMPLX(0.0,-TWOPI_D*i/REAL(N),DP)) )*p_hat(i,j) 
             g1_hat(i,j) = g1_rhs/g_coeff(i,j)

             g2_rhs = (1.0/(2*h) )*(exp( CMPLX(0.0, TWOPI_D*j/REAL(N),DP)) &
                  -exp( CMPLX(0.0,-TWOPI_D*j/REAL(N),DP)) )*p_hat(i,j) + A1_hat(i,j)
             g2_hat(i,j) = g2_rhs/g_coeff(i,j)
          ENDIF
       ENDDO
    ENDDO

    !STEP 5 - COMPUTE g1 and g2 via IFT
    CALL dfftw_plan_dft_c2r_2d_ (plan_backward_g1, N, N, g1_hat, g1, fftw_estimate)
    CALL dfftw_execute(plan_backward_g1)

    CALL dfftw_plan_dft_c2r_2d_ (plan_backward_g2, N, N, g2_hat, g2, fftw_estimate)
    CALL dfftw_execute(plan_backward_g2)

    !SCALE THE INVERSE TRANSFORM
    g1 = g1/REAL(N**2)
    g2 = g2/REAL(N**2)


    !REMARK:  THIS SUM IS BEING DONE ASSUMING PERIODICITY IN EQ (42)
    DO i = 0, N-1
       DO j = 0, N-1

          sum_out = 0.0
          DO l = i-3, i+3
             !set index_l
             index_i = MODULO(l,N)

             sum_in = 0.0
             DO m = j -3, j+3
                !set index_m
                index_j = MODULO(m,N)
                sum_in = sum_in + g1(index_i, index_j)*delta((l-i)*h)*delta((m-j)*h)
             ENDDO
             sum_out = sum_out + sum_in

          ENDDO

          U1(i,j) = alpha*hb*h**2*sum_out
       ENDDO
    ENDDO


    ! BUILD THE "Y" COMPONENT OF THE T-MATRIX

    DO i = 0, N-1
       DO j = 0, N-1
          sum_out = 0.0
          DO l = i-3, i+3
             !set index_l
             index_i = MODULO(l,N)

             sum_in = 0.0
             DO m = j -3, j+3
                !set index_m
                index_j = MODULO(m,N)


                sum_in = sum_in + g2(index_i, index_j)*delta((l-i)*h)*delta((m-j)*h)
             ENDDO
             sum_out = sum_out + sum_in
          ENDDO
          U2(i,j) = alpha*hb*h**2*sum_out
       ENDDO
    ENDDO


    CALL dfftw_destroy_plan(plan_forward_A1)
    CALL dfftw_destroy_plan(plan_backward_g1)
    CALL dfftw_destroy_plan(plan_backward_g2)

  ENDSUBROUTINE U_MATRIX




  SUBROUTINE T_MATRIX(T1, T2)
    REAL(DP), DIMENSION(0:N-1, 0:N-1), INTENT(INOUT) :: T1, T2

    REAL(DP), DIMENSION(0:N-1, 0:N-1):: A1, g1, g2
    COMPLEX(DP), DIMENSION(0:Nc-1, 0:N-1):: A1_hat, P_hat, g1_hat, g2_hat

    !COEFFICIENT FOR COMP. OF P_hat (fourier symbols)

    COMPLEX(DP) :: lap_coeff, p_rhs
    COMPLEX(DP) :: g1_rhs, g2_rhs

    INTEGER(DP) :: plan_forward_A1
    INTEGER(DP) :: plan_backward_g1
    INTEGER(DP) :: plan_backward_g2


    INTEGER(DP) :: i,j, index_i, index_j !handle periodicity
    INTEGER(DP) :: l,m

    REAL(DP) :: x,y
    REAL(DP) :: sum_in, sum_out

    !STEP 1 - BUILD THE A1 MATRIX.  THIS IS WHAT IS BEING FED
    !         TO THE FLUID SOLVER IN EQ (42)
    A1 = 0.0
    DO i = -3, 3
       x = x0 +i*h
       index_i = modulo(i,N)

       DO j = -3, 3
          y = y0 + j*h
          index_j = modulo(j,N)

          A1(index_i,index_j) = delta(x)*delta(y)

       ENDDO
    ENDDO

    !STEP 2 - BUILD A1_hat
    CALL dfftw_plan_dft_r2c_2d_ (plan_forward_A1, N, N, A1, A1_hat, fftw_estimate)
    CALL dfftw_execute(plan_forward_A1)


    !STEP 3 - COMPUTE P_hat
    P_hat(0,0) = CMPLX(0.0,0.0,DP)
    DO i = 0, Nc-1
       DO j = 0,N-1

          IF(i .NE. 0 .OR. j .NE. 0)THEN

             p_rhs = -(exp( CMPLX(0.0, TWOPI_D*i/REAL(N),DP)) - &
                  exp( CMPLX(0.0, -TWOPI_D*i/REAL(N),DP)) )*A1_hat(i,j)/(2*h)

             P_hat(i,j) = p_rhs/p_coeff(i,j)

          ENDIF
       ENDDO
    ENDDO


    !STEP 4 - COMPUTE g1_hat AND g2_hat
    !  note that g1 and g2 are the components of the fluid solver
    !  applied to a unit force at the origin

    g1_hat(0,0) = CMPLX(0.0,0.0,DP)
    g2_hat(0,0) = CMPLX(0.0,0.0,DP)
    DO i = 0, Nc-1
       DO j = 0,N-1
          IF(i .NE. 0 .OR. j .NE. 0)THEN
             g1_rhs = (1.0/(2*h) )*(exp( CMPLX(0.0, TWOPI_D*i/REAL(N),DP)) &
                  -exp( CMPLX(0.0,-TWOPI_D*i/REAL(N),DP)) )*p_hat(i,j) + A1_hat(i,j)
             g1_hat(i,j) = g1_rhs/g_coeff(i,j)


             g2_rhs = (1.0/(2*h) )*(exp( CMPLX(0.0, TWOPI_D*j/REAL(N),DP)) &
                  -exp( CMPLX(0.0,-TWOPI_D*j/REAL(N),DP)) )*p_hat(i,j)
             g2_hat(i,j) = g2_rhs/g_coeff(i,j)
          ENDIF
       ENDDO
    ENDDO

    !STEP 5 - COMPUTE g1 and g2 via IFT
    CALL dfftw_plan_dft_c2r_2d_ (plan_backward_g1, N, N, g1_hat, g1, fftw_estimate)
    CALL dfftw_execute(plan_backward_g1)

    CALL dfftw_plan_dft_c2r_2d_ (plan_backward_g2, N, N, g2_hat, g2, fftw_estimate)
    CALL dfftw_execute(plan_backward_g2)

    !SCALE THE INVERSE TRANSFORM
    g1 = g1/REAL(N**2)
    g2 = g2/REAL(N**2)


    !REMARK:  THIS SUM IS BEING DONE ASSUMING PERIODICITY IN EQ (42)
    DO i = 0, N-1
       DO j = 0, N-1

          sum_out = 0.0
          DO l = i-3, i+3
             !set index_l
             index_i = MODULO(l,N)

             sum_in = 0.0
             DO m = j -3, j+3
                !set index_m
                index_j = MODULO(m,N)


                sum_in = sum_in + g1(index_i, index_j)*delta((l-i)*h)*delta((m-j)*h)

             ENDDO
             sum_out = sum_out + sum_in

          ENDDO

          T1(i,j) = alpha*hb*h**2*sum_out
       ENDDO
    ENDDO


    ! BUILD THE "Y" COMPONENT OF THE T-MATRIX
    DO i = 0, N-1
       DO j = 0, N-1
          sum_out = 0.0
          DO l = i-3, i+3
             !set index_l
             index_i = MODULO(l,N)

             sum_in = 0.0
             DO m = j -3, j+3
                !set index_m
                index_j = MODULO(m,N)
                sum_in = sum_in + g2(index_i, index_j)*delta((l-i)*h)*delta((m-j)*h)
             ENDDO
             sum_out = sum_out + sum_in
          ENDDO
          T2(i,j) = alpha*hb*h**2*sum_out
       ENDDO
    ENDDO


    CALL dfftw_destroy_plan(plan_forward_A1)
    CALL dfftw_destroy_plan(plan_backward_g1)
    CALL dfftw_destroy_plan(plan_backward_g2)

  ENDSUBROUTINE T_MATRIX


  SUBROUTINE Mn_MATRIX(T1n,T2n, U1n,U2n, T1, T2, U1, U2, bdry0)
    REAL(DP), DIMENSION(0:Nb, 0:Nb), INTENT(INOUT) :: T1n,T2n
    REAL(DP), DIMENSION(0:Nb, 0:Nb), INTENT(INOUT) :: U1n,U2n

    REAL(DP), DIMENSION(0:N-1, 0:N-1), INTENT(INOUT) :: T1,T2
    REAL(DP), DIMENSION(0:N-1, 0:N-1), INTENT(INOUT) :: U1,U2

    REAL(DP), DIMENSION(0:2*Nb+1),INTENT(IN) :: bdry0

    REAL(DP) :: p1, p2
    INTEGER(DP) :: ip, jp
    INTEGER(DP) :: i,j

    !FOR EACH i and j from 0:Nb-1
    DO i = 0,Nb
       DO j = 0,Nb

          p1 = bdry0(i) - bdry0(j)
          p2 = bdry0(i+Nb+1) - bdry0(j+Nb+1)

          ip = FLOOR( (p1-x0)/h )  ;  ip = MODULO(ip, N)
          jp = FLOOR( (p2-y0)/h )  ;  jp = MODULO(jp, N)

          IF(p1 < 0) THEN
             p1 = p1 + (xf-x0)
          ENDIF
          IF(p2 < 0) THEN
             p2 = p2 + (yf-y0)
          ENDIF

          T1n(i,j) = bi_int(ip,jp,p1,p2,T1)
          T2n(i,j) = bi_int(ip,jp,p1,p2,T2)

          U1n(i,j) = bi_int(ip,jp,p1,p2,U1)
          U2n(i,j) = bi_int(ip,jp,p1,p2,U2)
       ENDDO
    ENDDO
  ENDSUBROUTINE Mn_MATRIX





  SUBROUTINE FLUID_SOLVE(uold, vold, bdry, time)
    REAL(DP), DIMENSION(0:N-1,0:N-1), INTENT(INOUT) :: uold, vold
    REAL(DP), DIMENSION(0:2*Nb+1):: bdry

    REAL(DP), DIMENSION(0:2*Nb+1):: AX
    REAL(DP), DIMENSION(0:N-1, 0:N-1):: SnAX1,SnAX2

    !FOR THE CONVECTIVE AND NONLINEAR PART
    REAL(DP), DIMENSION(0:N-1, 0:N-1):: A1, A2 
    REAL(DP), DIMENSION(0:N-1, 0:N-1):: an_u, an_v
    COMPLEX(DP), DIMENSION(0:Nc-1, 0:N-1):: an_u_hat, an_v_hat

    REAL(DP), DIMENSION(0:N-1, 0:N-1):: g1, g2
    COMPLEX(DP), DIMENSION(0:Nc-1, 0:N-1):: g1_hat, g2_hat

    COMPLEX(DP), DIMENSION(0:Nc-1, 0:N-1):: P_hat
    COMPLEX(DP) :: p_rhs  
    COMPLEX(DP) :: g1_rhs, g2_rhs

    INTEGER(DP) :: plan_forward_an_u
    INTEGER(DP) :: plan_forward_an_v

    INTEGER(DP) :: plan_backward_g1
    INTEGER(DP) :: plan_backward_g2


    INTEGER(DP) :: i,j,s
    INTEGER(DP) :: index_i, index_j
    INTEGER(DP) :: i_close, j_close, left, right, up, down
    REAL(DP) :: x,y, time



    CALL FORCE(AX, bdry, time)


    SnAX1 = 0.0
    SnAX2 = 0.0

    DO s = 0, Nb

       !IDENTIFY NEAREST EULARIAN POINTS
       i_close = INT( (bdry(s) - x0)/h    )
       j_close = INT( (bdry(s+Nb+1) - y0)/h )


       !SPREAD THE FORCE TO THIS AREA
       DO i = i_close-3, i_close + 3
          index_i = MODULO(i,N)
          x = x0 +i*h
          DO j = j_close -3, j_close +3
             index_j = MODULO(j,N)
             y = y0 + j*h

             SnAX1(index_i,index_j) = SnAX1(index_i,index_j) + &
                  hb*AX(s)*delta(x - bdry(s))*delta(y-bdry(s+Nb+1))
             SnAX2(index_i,index_j) = SnAX2(index_i,index_j) + &
                  hb*AX(s+Nb+1)*delta(x - bdry(s))*delta(y-bdry(s+Nb+1))


          ENDDO
       ENDDO
    ENDDO

    an_u = SnAX1
    an_v = SnAX2



    ! COMPUTE THE ASSOCIATED PRESSURE
    !STEP 2 - BUILD A2_hat
    CALL dfftw_plan_dft_r2c_2d_ (plan_forward_an_u, N, N, an_u, an_u_hat, fftw_estimate)
    CALL dfftw_execute(plan_forward_an_u)
    CALL dfftw_plan_dft_r2c_2d_ (plan_forward_an_v, N, N, an_v, an_v_hat, fftw_estimate)
    CALL dfftw_execute(plan_forward_an_v)


    !STEP 3 - COMPUTE P_hat
    P_hat(0,0) = CMPLX(0.0,0.0,DP)
    DO i = 0, Nc-1
       DO j = 0,N-1

          IF(i .NE. 0 .OR. j .NE. 0)THEN

             p_rhs = (exp( CMPLX(0.0, TWOPI_D*i/REAL(N),DP)) - &
                  exp( CMPLX(0.0, -TWOPI_D*i/REAL(N),DP)) )*an_u_hat(i,j)/(2*h)  + &
                  (exp( CMPLX(0.0, TWOPI_D*j/REAL(N),DP)) - &
                  exp( CMPLX(0.0, -TWOPI_D*j/REAL(N),DP)) )*an_v_hat(i,j)/(2*h)

             P_hat(i,j) = p_rhs/p_coeff(i,j)
          ENDIF
       ENDDO
    ENDDO

    g1_hat(0,0) = CMPLX(0.0,0.0,DP)
    g2_hat(0,0) = CMPLX(0.0,0.0,DP)

    DO i = 0, Nc-1
       DO j = 0,N-1

          IF(i .NE. 0 .OR. j .NE. 0)THEN
             g1_rhs = (1.0/(2*h) )*(exp( CMPLX(0.0, TWOPI_D*i/REAL(N),DP)) &
                  -exp( CMPLX(0.0,-TWOPI_D*i/REAL(N),DP)) )*p_hat(i,j) - an_u_hat(i,j)
             g1_hat(i,j) = g1_rhs/g_coeff(i,j)

             g2_rhs = (1.0/(2*h) )*(exp( CMPLX(0.0, TWOPI_D*j/REAL(N),DP)) &
                  -exp( CMPLX(0.0,-TWOPI_D*j/REAL(N),DP)) )*p_hat(i,j) - an_v_hat(i,j)
             g2_hat(i,j) = g2_rhs/g_coeff(i,j)

          ENDIF
       ENDDO
    ENDDO


    CALL dfftw_plan_dft_c2r_2d_ (plan_backward_g1, N, N, g1_hat, g1, fftw_estimate)
    CALL dfftw_execute(plan_backward_g1)
    CALL dfftw_plan_dft_c2r_2d_ (plan_backward_g2, N, N, g2_hat, g2, fftw_estimate)
    CALL dfftw_execute(plan_backward_g2)    


    uold = g1/(REAL(N**2))
    vold = g2/(REAL(N**2))

    CALL dfftw_destroy_plan(plan_forward_an_u)
    CALL dfftw_destroy_plan(plan_forward_an_v)
    CALL dfftw_destroy_plan(plan_backward_g1)
    CALL dfftw_destroy_plan(plan_backward_g2)    
  ENDSUBROUTINE FLUID_SOLVE


  SUBROUTINE Bn_VECTOR(B,uold, vold, bdry0)
    REAL(DP), DIMENSION(0:2*Nb+1) :: B, Sn_star_g, bdry0
    REAL(DP), DIMENSION(0:N-1, 0:N-1) :: uold, vold, A1, A2, g1, g2
    COMPLEX(DP), DIMENSION(0:Nc-1, 0:N-1) :: A1_hat, A2_hat, g1_hat, g2_hat

    COMPLEX(DP), DIMENSION(0:Nc-1, 0:N-1) :: p_hat
    INTEGER(DP) :: i,j, left, right, up, down, s
    INTEGER(DP) :: i_close, j_close, index_i, index_j
    REAL(DP) :: x,y

    COMPLEX(DP) :: p_rhs, g1_rhs, g2_rhs
    REAL(DP) :: sumout_u, sumout_v, sumin_u, sumin_v


    INTEGER(DP) :: plan_forward_A1
    INTEGER(DP) :: plan_forward_A2
    INTEGER(DP) :: plan_backward_g1
    INTEGER(DP) :: plan_backward_g2


    ! STEP 1 - COMPUTE THE FLUID SOLVER APPLIED TO 
    !          u^n - dt*u^n\cdot\nabla u^n
    DO i = 0, N-1
       DO j = 0, N-1

          A1(i,j) = 0.
          A2(i,j) = 0.

       ENDDO
    ENDDO

    ! COMPUTE THE ASSOCIATED PRESSURE
    !STEP 2 - BUILD A2_hat

    CALL dfftw_plan_dft_r2c_2d_ (plan_forward_A1, N, N, A1, A1_hat, fftw_estimate)
    CALL dfftw_execute(plan_forward_A1)

    CALL dfftw_plan_dft_r2c_2d_ (plan_forward_A2, N, N, A2, A2_hat, fftw_estimate)
    CALL dfftw_execute(plan_forward_A2)


    !STEP 3 - COMPUTE P_hat
    P_hat(0,0) = CMPLX(0.0,0.0,DP)
    DO i = 0, Nc-1
       DO j = 0,N-1

          IF(i .NE. 0 .OR. j .NE. 0)THEN
             p_rhs = -((exp( CMPLX(0.0, TWOPI_D*i/REAL(N),DP)) - &
                  exp( CMPLX(0.0, -TWOPI_D*i/REAL(N),DP)) )*A1_hat(i,j)/(2*h)  + &
                  (exp( CMPLX(0.0, TWOPI_D*j/REAL(N),DP)) - &
                  exp( CMPLX(0.0, -TWOPI_D*j/REAL(N),DP)) )*A2_hat(i,j)/(2*h))

             P_hat(i,j) = p_rhs/p_coeff(i,j)
          ENDIF
       ENDDO
    ENDDO


    !STEP 4 - COMPUTE g1_hat AND g2_hat
    g1_hat(0,0) = CMPLX(0.0,0.0,DP)
    g2_hat(0,0) = CMPLX(0.0,0.0,DP)

    DO i = 0, Nc-1
       DO j = 0,N-1
          IF(i .NE. 0 .OR. j .NE. 0) THEN
             g1_rhs = (1.0/(2*h) )*(exp( CMPLX(0.0, TWOPI_D*i/REAL(N),DP)) &
                  -exp( CMPLX(0.0,-TWOPI_D*i/REAL(N),DP)) )*p_hat(i,j) + A1_hat(i,j)
             g1_hat(i,j) = g1_rhs/g_coeff(i,j)

             g2_rhs = (1.0/(2*h) )*(exp( CMPLX(0.0, TWOPI_D*j/REAL(N),DP)) &
                  -exp( CMPLX(0.0,-TWOPI_D*j/REAL(N),DP)) )*p_hat(i,j) + A2_hat(i,j)
             g2_hat(i,j) = g2_rhs/g_coeff(i,j)
          ENDIF
       ENDDO
    ENDDO

    CALL dfftw_plan_dft_c2r_2d_ (plan_backward_g1, N, N, g1_hat, g1, fftw_estimate)
    CALL dfftw_execute(plan_backward_g1)

    CALL dfftw_plan_dft_c2r_2d_ (plan_backward_g2, N, N, g2_hat, g2, fftw_estimate)
    CALL dfftw_execute(plan_backward_g2)

    g1 = g1/REAL(N**2)
    g2 = g2/REAL(N**2)


    ! INTERPOLATE THE RESULT FROM THE FLUID SOLVE
    DO s = 0, Nb
       ! 1 - FOR EACH S, COMPUTE THE EULERIAN INDEX CLOSEST TO BDRY
       i_close = INT( (bdry0(s) - x0)/h    )
       j_close = INT( (bdry0(s+Nb+1) - y0)/h )

       sumout_u = 0.0
       sumout_v = 0.0

       DO i = i_close -3, i_close +3

          index_i = MODULO(i, N)
          x = x0 + i*h

          sumin_u = 0.0
          sumin_v = 0.0

          DO j = j_close -3, j_close +3

             index_j = MODULO(j, N)
             y = y0 + j*h

             sumin_u = sumin_u + g1(index_i,index_j)*delta(x - bdry0(s))*delta(y-bdry0(s+Nb+1))
             sumin_v = sumin_v + g2(index_i,index_j)*delta(x - bdry0(s))*delta(y-bdry0(s+Nb+1))

          ENDDO

          sumout_u = sumout_u + sumin_u
          sumout_v = sumout_v + sumin_v

       ENDDO
       Sn_star_g(s) = sumout_u*h**2
       Sn_star_g(s+Nb+1) = sumout_v*h**2

       B(s) = bdry0(s) - alpha*Sn_star_g(s)
       B(s+Nb+1) = bdry0(s+Nb+1) - alpha*Sn_star_g(s+Nb+1)

    ENDDO

    CALL dfftw_destroy_plan(plan_forward_A1)
    CALL dfftw_destroy_plan(plan_forward_A2)
    CALL dfftw_destroy_plan(plan_backward_g1)
    CALL dfftw_destroy_plan(plan_backward_g2)    
  ENDSUBROUTINE Bn_VECTOR


  SUBROUTINE NEWTON(old_position, approx, time, Mn, B)
    REAL(DP), DIMENSION(0:2*Nb+1) :: old_position
    REAL(DP), DIMENSION(0:2*Nb+1) :: initial, approx
    REAL(DP), DIMENSION(0:2*Nb+1) :: B
    REAL(DP), DIMENSION(0:2*Nb+1,0:2*Nb+1) :: Mn, Id
    REAL(DP), DIMENSION(0:2*Nb+1) :: F, rhs, aux_force

    ! JACOBIAN VARIABLES
    REAL(DP), DIMENSION(0:2*Nb+1,0:2*Nb+1) :: jacobian

    !LAPACK VARIABLES
    INTEGER, DIMENSION(0:2*Nb+1) :: IPIV
    INTEGER :: info

    INTEGER(DP) :: i, count, max_count, j,k
    REAL(DP) :: R, time
    
    
    REAL(DP) :: anorm, rcond
    REAL(DP)   , DIMENSION(8*2*Nb) :: work
    INTEGER(DP), DIMENSION(2*Nb)   :: iwork

    max_count = 5
    count = 0

    approx = 0.0

    !ESTABLISH AN IDENTITY MATRIX
    Id = 0.0
    DO i = 0,Nb
       Id(i,i) = 1.0
       Id(i+Nb+1,i+Nb+1) = 1.0
    ENDDO

    initial = old_position


    DO
       
       count = count + 1
       
       R = 0
       
       ! GET THE JACOBIAN OF force AND FUNCTION VALUE F = M*force + BN - X
       
       CALL compute_jacobian(initial, jacobian,time)
       jacobian = matmul(Mn,jacobian) + Id


       ! STEP 2: COMPUTE FUNC = Mn*aux + Bn - vector
       CALL FORCE(aux_force, initial, time)
       F = MATMUL(Mn, aux_force) + initial - B
       
       
       ! STEP 3: SOLVE Jac*aux_bdry = -F
       rhs = -F
       CALL DGESV(2*Nb+2, 1, jacobian, 2*Nb+2, ipiv, rhs, 2*Nb+2, info)
       IF( info /= 0) print*, "INFO  = ", INFO
       
       ! STEP 4: SET UPDATED BDRY
       approx = initial + rhs
       
       DO i = 0, 2*Nb+1
          IF(abs(  (initial(i) - approx(i))/initial(i)   )>R)THEN
             R = abs((initial(i) - approx(i))/initial(i))
          ENDIF
       ENDDO



       initial = approx
       IF(R < 10.0**(-10)) EXIT

       IF(count > max_count)THEN
          print*, "count exceeded max:  r = ", r
          EXIT
       ENDIF

    ENDDO

  ENDSUBROUTINE NEWTON






  FUNCTION C(k,t) !CURVATURE FUNCTION
    REAL(DP) :: C
    REAL(DP) :: t,x
    INTEGER(DP) :: k
    
    C = -b*(k*hb - 1.)*kpa**2*sin(kpa*k*hb + w*t)
  ENDFUNCTION C



  SUBROUTINE FORCE(AX, bdry, t)
    !-----------------------------------------
    ! COMPUTES THE FORCE DERIVED BY
    !
    ! 1. DISCRETIZE ENERGY
    ! 2. TAKE GRADIENT
    !-----------------------------------------

    REAL(DP), DIMENSION(0:2*Nb+1), INTENT(INOUT):: AX
    REAL(DP), DIMENSION(0:2*Nb+1), INTENT(IN):: bdry
    REAL(DP) :: t

    !-- C/P FROM JACOBIAN CODE --!
    REAL(DP) :: xl1, xl2, xr1, xr2

    INTEGER(DP) :: k, i
    INTEGER(DP) :: l1,l2, r1, r2

    ! VARIOUS RECURRING QUANTITIES IN THE JACOBIAN
    REAL(DP) :: dX_k  , dY_k      ! RMK:  dX_k = X_{k+1} - X_{k-1} i.e. the difference centered at k
    REAL(DP) :: dX_kp1, dX_km1      
    REAL(DP) :: dY_kp1, dY_km1

    REAL(DP) :: sum_k, sum_kp1, sum_km1 ! sum_k = dX_k**2 + dY_k**2
    REAL(DP) :: mix_sum_k, mix_sum_kp1, mix_sum_km1 ! dXLY - LXdY

    !SECOND DERIVATIVE SUM/DIFF
    REAL(DP) :: LX_k  , LY_k
    REAL(DP) :: LX_kp1, LX_km1      
    REAL(DP) :: LY_kp1, LY_km1
    !---------------------------!

    REAL(DP) :: temp1,temp2,temp3
    REAL(DP) :: temp_E1, temp_E2
    REAL(DP) :: temp_curv_km1, temp_curv_k, temp_curv_kp1

    REAL(DP) :: sum_0, sum_1, sum_2, sum_N
    REAL(DP) :: dX_1, LX_1, dY_1, LY_1, mix_sum_1
    REAL(DP) :: dX_N, LX_N, dY_N, LY_N, mix_sum_N


    AX = 0.0
    
    ! sum_0 = 1st derivative centered at X0
    ! sum_1 = 1st derivative centered at X1
    ! sum_2 = 1st derivative centered at X2
    
    sum_0 = (bdry(1) - bdry(0))**2 + (bdry(Nb+2) - bdry(Nb+1))**2

    ! ---------------- K = 0  AND K = Nb CASE ----------------------!
    sum_1 = (bdry(2) - bdry(0))**2 + (bdry(Nb+3) - bdry(Nb+1))**2    

    TEMP_E1 = (sqrt(sum_0)/(hb) -1)*(bdry(1)-bdry(0))*(-1)/sqrt(sum_0) &
         +  (sqrt(sum_1)/(2*hb) -1)*(bdry(2)-bdry(0))*(-1)/sqrt(sum_1)   
    AX(0) = -S1*TEMP_E1


    TEMP_E1 = (sqrt(sum_0)/(hb) -1)*(bdry(2+Nb)-bdry(1+Nb))*(-1)/sqrt(sum_0) &
         +  (sqrt(sum_1)/(2*hb) -1)*(bdry(3+Nb)-bdry(1+Nb))*(-1)/sqrt(sum_1)
    AX(Nb+1) = -S1*TEMP_E1


    ! ---------------- K = 1  AND K = Nb+1 CASE --------------------!
    !    sum_0 = (bdry(1) - bdry(0))**2 + (bdry(Nb+2) - bdry(Nb+1))**2
    !     Y(0)         Y(1)        Y(2)         Y(3)
    ! bdry(Nb+1) , bdry(Nb+2) , bdry(Nb+3) , bdry(Nb+4)

    !     X(0)         X(1)        X(2)         X(3)
    ! bdry(0)   ,    bdry(1)   , bdry(2)   ,   bdry(3)


    sum_2 = (bdry(3) - bdry(1))**2 + (bdry(Nb+4) - bdry(Nb+2))**2
    TEMP_E1 = (sqrt(sum_0)/(hb) -1)*(bdry(1)-bdry(0))*(1)/sqrt(sum_0) &
         +  (sqrt(sum_2)/(2*hb) -1)*(bdry(3)-bdry(1))*(-1)/sqrt(sum_2)
    AX(1) = -S1*TEMP_E1

    TEMP_E1 = (sqrt(sum_0)/(hb) -1)*(bdry(2+Nb)-bdry(1+Nb))*(1)/sqrt(sum_0) &
         +  (sqrt(sum_2)/(2*hb) -1)*(bdry(4+Nb)-bdry(2+Nb))*(-1)/sqrt(sum_2)
    AX(Nb+2) = -S1*TEMP_E1




    ! FOR THE FOLLOWING LET
    ! sum_0 = sum_{Nb}
    sum_0 = (bdry(Nb) - bdry(Nb-1))**2 + (bdry(2*Nb+1) - bdry(2*Nb))**2

    ! ---------------- K = Nb-1  AND K = 2*Nb  CASE --------------------!
    ! sum_1 = sum_{Nb-2}
    sum_1 = (bdry(Nb-1) - bdry(Nb-3))**2 + (bdry(2*Nb) - bdry(2*Nb-2))**2    


    TEMP_E1 = (sqrt(sum_0)/(hb) -1)*(bdry(Nb)-bdry(Nb-1))*(-1)/sqrt(sum_0) &
         +  (sqrt(sum_1)/(2*hb) -1)*(bdry(Nb-1)-bdry(Nb-3))*(1)/sqrt(sum_1)
    AX(Nb-1) = -S1*TEMP_E1

    TEMP_E1 = (sqrt(sum_0)/(hb) -1)*(bdry(2*Nb+1)-bdry(2*Nb))*(-1)/sqrt(sum_0) &
         +  (sqrt(sum_1)/(2*hb) -1)*(bdry(2*Nb)-bdry(2*Nb-2))*(1)/sqrt(sum_1)
    AX(2*Nb) = -S1*TEMP_E1



    ! ---------------- K = Nb   AND K = 2*Nb+1 CASE --------------------!
    ! sum_1 = sum_{Nb-1}
    sum_1 = (bdry(Nb) - bdry(Nb-2))**2 + (bdry(2*Nb+1) - bdry(2*Nb-1))**2    


    TEMP_E1 = (sqrt(sum_0)/(hb) -1)*(bdry(Nb)-bdry(Nb-1))*(1)/sqrt(sum_0) &
         +  (sqrt(sum_1)/(2*hb) -1)*(bdry(Nb)-bdry(Nb-2))*(1)/sqrt(sum_1)
    AX(Nb) = -S1*TEMP_E1

    TEMP_E1 = (sqrt(sum_0)/(hb) -1)*(bdry(2*Nb+1)-bdry(2*Nb))*(1)/sqrt(sum_0) &
         +  (sqrt(sum_1)/(2*hb) -1)*(bdry(2*Nb+1)-bdry(2*Nb-1))*(1)/sqrt(sum_1)
    AX(2*Nb+1) = -S1*TEMP_E1






    DO k = 2, Nb-2

       l1  = k-1
       l2  = k-2
       r1  = k+1
       r2  = k+2

       xl1 = bdry(l1) ;  xl2 = bdry(l2)
       xr1 = bdry(r1) ;  xr2 = bdry(r2)


       ! DIFFERENCES IN X       ;   DIFFERENCES IN Y                            ;    SUM SQUARED
       dX_kp1 = xr2 - bdry(k)   ;   dY_kp1 = bdry(r2 + Nb+1) - bdry(k + Nb+1)   ;    sum_kp1 = dX_kp1**2 + dY_kp1**2
       dX_km1 = bdry(k) - xl2   ;   dY_km1 = bdry(k  + Nb+1) - bdry(l2 + Nb+1)  ;    sum_km1 = dX_km1**2 + dY_km1**2


       ! FIRST Nb COMPONENTS OF THE FORCE       
       TEMP_E1 = (sqrt(sum_km1)/(2*hb) -1)*dX_km1/sqrt(sum_km1) - (sqrt(sum_kp1)/(2*hb) -1)*dX_kp1/sqrt(sum_kp1)
       AX(k) = -S1*TEMP_E1 

       TEMP_E1 = (sqrt(sum_km1)/(2*hb) -1)*dY_km1/sqrt(sum_km1) - (sqrt(sum_kp1)/(2*hb) -1)*dY_kp1/sqrt(sum_kp1)
       AX(k+Nb+1) = -S1*TEMP_E1 
    ENDDO



    !---------------------------------------------------------------------------!
    !                             CURVATURE CASE                                !
    !---------------------------------------------------------------------------!

    dX_1 = bdry(2) - bdry(0)
    dY_1 = bdry(3+Nb) - bdry(1+Nb)
    LX_1 = bdry(2) -2*bdry(1) + bdry(0)
    LY_1 = bdry(3+Nb) -2*bdry(2+Nb) + bdry(1+Nb)


    sum_1 = (dX_1)**2 + (dY_1)**2    
    mix_sum_1 = LY_1*dX_1 - LX_1*dY_1

    ! DERVIATIVE WITH RESPECT TO X_0
    TEMP_E2 = 8*(4*mix_sum_1/sqrt(sum_1)**3 - C(1_DP,t))*&
         ( (-LY_1-dY_1)*sqrt(sum_1)**3 + 3*mix_sum_1*sqrt(sum_1)*dX_1)/sum_1**3

    AX(0) = AX(0) - S2*hb*TEMP_E2

    ! DERVIATIVE WITH RESPECT TO Y_0
    TEMP_E2 = 8*(4*mix_sum_1/sqrt(sum_1)**3 - C(1_DP,t))*&
         ( (dX_1+LX_1)*sqrt(sum_1)**3 + 3*mix_sum_1*sqrt(sum_1)*dY_1)/sum_1**3

    AX(Nb+1) = AX(Nb+1) - S2*hb*TEMP_E2


    
    ! RIGHT ENDPOINT CASE
    dX_N  = bdry(Nb) - bdry(Nb-2) !CENTERED AT Nb-1
    dY_N  = bdry(2*Nb+1) - bdry(2*Nb-1) !CENTERED AT Nb-1 FOR Y, THOUGH
    LX_N  = bdry(Nb) -2*bdry(Nb-1) +  bdry(Nb-2)
    LY_N  = bdry(2*Nb+1) -2*bdry(2*Nb)+ bdry(2*Nb-1)

    sum_N = dX_N**2 + dY_N**2
    mix_sum_N = LY_N*dX_N - LX_N*dY_N


    ! DERVIATIVE WITH RESPECT TO X_Nb-1
    TEMP_E2 = 8*(4*mix_sum_N/sqrt(sum_N)**3 - C(Nb-1,t))*&
         ( (LY_N-dY_N)*sqrt(sum_N)**3 - 3*mix_sum_N*sqrt(sum_N)*dX_N)/sum_N**3
    AX(Nb) = AX(Nb) - S2*hb*TEMP_E2

    ! DERVIATIVE WITH RESPECT TO Y_Nb-1
    TEMP_E2 = 8*(4*mix_sum_N/sqrt(sum_N)**3 - C(Nb-1,t))*&
         ( (dX_N-LX_N)*sqrt(sum_N)**3 - 3*mix_sum_N*sqrt(sum_N)*dY_N)/sum_N**3
    AX(2*Nb+1) = AX(2*Nb+1) - S2*hb*TEMP_E2


    DO k = 1, Nb-1
       
       l1  = k-1
       l2  = k-2
       r1  = k+1
       r2  = k+2


       IF(k==1)THEN
          xl1 = bdry(l1)
          xr1 = bdry(r1)
          xr2 = bdry(r2)


          ! DIFFERENCES IN X       ;   DIFFERENCES IN Y                            ;    SUM SQUARED
          dX_k   = xr1 - xl1       ;   dY_k   = bdry(r1 + Nb+1) - bdry(l1 + Nb+1)  ;    sum_k   = dX_k**2 + dY_k**2
          dX_kp1 = xr2 - bdry(k)   ;   dY_kp1 = bdry(r2 + Nb+1) - bdry(k + Nb+1)   ;    sum_kp1 = dX_kp1**2 + dY_kp1**2

          LX_k   = xr1  -2*bdry(k) +  xl1       ;   LY_k   = bdry(r1 + Nb+1) -2*bdry(k  + Nb+1) + bdry(l1 + Nb+1)
          LX_kp1 = xr2  -2*xr1     + bdry(k)    ;   LY_kp1 = bdry(r2 + Nb+1) -2*bdry(r1 + Nb+1) + bdry(k + Nb+1) 



          ! MIXED SUM
          mix_sum_k   = dX_k*LY_k - LX_k*dY_k
          mix_sum_kp1 = dX_kp1*LY_kp1 - LX_kp1*dY_kp1

          ! AUXILARY QUANTITIES
          temp_curv_k   = 4*mix_sum_k/sqrt(sum_k)**3 - C(k,t)
          temp_curv_kp1 = 4*mix_sum_kp1/sqrt(sum_kp1)**3 - C(k+1,t)



          !------------------------------------------------------------------------------------------------!
          ! FIRST Nb COMPONENTS OF THE FORCE       
          temp2 = dY_k*sqrt(sum_k)**3 /sum_k**3
          temp3 = ( (-LY_kp1 - dY_kp1)*sqrt(sum_kp1)**3 + 3*mix_sum_kp1*sqrt(sum_kp1)*dX_kp1 )/sum_kp1**3


          TEMP_E2 = 16*temp_curv_k*temp2 + 8*temp_curv_kp1*temp3

       ELSEIF(k==Nb-1)THEN
          xl1 = bdry(l1)
          xr1 = bdry(r1)
          xl2 = bdry(l2)



          ! DIFFERENCES IN X       ;   DIFFERENCES IN Y                            ;    SUM SQUARED
          dX_k   = xr1 - xl1       ;   dY_k   = bdry(r1 + Nb+1) - bdry(l1 + Nb+1)  ;    sum_k   = dX_k**2 + dY_k**2
          dX_km1 = bdry(k) - xl2   ;   dY_km1 = bdry(k  + Nb+1) - bdry(l2 + Nb+1)  ;    sum_km1 = dX_km1**2 + dY_km1**2

          LX_k   = xr1     -2*bdry(k) +  xl1       ;   LY_k   = bdry(r1 + Nb+1) -2*bdry(k  + Nb+1) + bdry(l1 + Nb+1)
          LX_km1 = bdry(k) -2*xl1     + xl2        ;   LY_km1 = bdry(k  + Nb+1) -2*bdry(l1 + Nb+1) + bdry(l2 + Nb+1)


          ! MIXED SUM
          mix_sum_k   = dX_k*LY_k - LX_k*dY_k
          mix_sum_km1 = dX_km1*LY_km1 - LX_km1*dY_km1


          ! AUXILARY QUANTITIES
          temp_curv_km1 = 4*mix_sum_km1/sqrt(sum_km1)**3 - C(k-1,t)
          temp_curv_k   = 4*mix_sum_k/sqrt(sum_k)**3 - C(k,t)


          !------------------------------------------------------------------------------------------------!
          ! FIRST Nb COMPONENTS OF THE FORCE       
          temp1 = ( (LY_km1 - dY_km1)*sqrt(sum_km1)**3 - 3*mix_sum_km1*sqrt(sum_km1)*dX_km1 )/sum_km1**3
          temp2 = dY_k*sqrt(sum_k)**3 /sum_k**3

          TEMP_E2 = 8*temp_curv_km1*temp1 + 16*temp_curv_k*temp2
       ELSE
          xl1 = bdry(l1)
          xr1 = bdry(r1)
          xl2 = bdry(l2)
          xr2 = bdry(r2)


          ! DIFFERENCES IN X       ;   DIFFERENCES IN Y                            ;    SUM SQUARED
          dX_k   = xr1 - xl1       ;   dY_k   = bdry(r1 + Nb+1) - bdry(l1 + Nb+1)  ;    sum_k   = dX_k**2 + dY_k**2
          dX_kp1 = xr2 - bdry(k)   ;   dY_kp1 = bdry(r2 + Nb+1) - bdry(k + Nb+1)   ;    sum_kp1 = dX_kp1**2 + dY_kp1**2
          dX_km1 = bdry(k) - xl2   ;   dY_km1 = bdry(k  + Nb+1) - bdry(l2 + Nb+1)  ;    sum_km1 = dX_km1**2 + dY_km1**2

          LX_k   = xr1     -2*bdry(k) +  xl1       ;   LY_k   = bdry(r1 + Nb+1) -2*bdry(k  + Nb+1) + bdry(l1 + Nb+1)
          LX_kp1 = xr2     -2*xr1     + bdry(k)    ;   LY_kp1 = bdry(r2 + Nb+1) -2*bdry(r1 + Nb+1) + bdry(k  + Nb+1) 
          LX_km1 = bdry(k) -2*xl1     + xl2        ;   LY_km1 = bdry(k  + Nb+1) -2*bdry(l1 + Nb+1) + bdry(l2 + Nb+1)


          ! MIXED SUM
          mix_sum_k   = dX_k*LY_k - LX_k*dY_k
          mix_sum_kp1 = dX_kp1*LY_kp1 - LX_kp1*dY_kp1
          mix_sum_km1 = dX_km1*LY_km1 - LX_km1*dY_km1


          ! AUXILARY QUANTITIES
          temp_curv_km1 = 4*mix_sum_km1/sqrt(sum_km1)**3 - C(k-1,t)
          temp_curv_k   = 4*mix_sum_k/sqrt(sum_k)**3 - C(k,t)
          temp_curv_kp1 = 4*mix_sum_kp1/sqrt(sum_kp1)**3 - C(k+1,t)



          !------------------------------------------------------------------------------------------------!
          ! FIRST Nb COMPONENTS OF THE FORCE
          temp1 = ( (LY_km1 - dY_km1)*sqrt(sum_km1)**3 - 3*mix_sum_km1*sqrt(sum_km1)*dX_km1 )/sum_km1**3
          temp2 = dY_k*sqrt(sum_k)**3 /sum_k**3
          temp3 = ( (-LY_kp1 - dY_kp1)*sqrt(sum_kp1)**3 + 3*mix_sum_kp1*sqrt(sum_kp1)*dX_kp1 )/sum_kp1**3

          TEMP_E2 = 8*temp_curv_km1*temp1 + 16*temp_curv_k*temp2 + 8*temp_curv_kp1*temp3

       ENDIF


       AX(k) = AX(k) - S2*hb*TEMP_E2


       IF(k==1)THEN
          xl1 = bdry(l1)
          xr1 = bdry(r1)
          xr2 = bdry(r2)


          ! DIFFERENCES IN X       ;   DIFFERENCES IN Y                            ;    SUM SQUARED
          dX_k   = xr1 - xl1       ;   dY_k   = bdry(r1 + Nb+1) - bdry(l1 + Nb+1)    ;    sum_k   = dX_k**2 + dY_k**2
          dX_kp1 = xr2 - bdry(k)   ;   dY_kp1 = bdry(r2 + Nb+1) - bdry(k + Nb+1)     ;    sum_kp1 = dX_kp1**2 + dY_kp1**2


          LX_k   = xr1  -2*bdry(k) +  xl1       ;   LY_k   = bdry(r1 + Nb+1) -2*bdry(k  + Nb+1) + bdry(l1 + Nb+1)
          LX_kp1 = xr2  -2*xr1     + bdry(k)    ;   LY_kp1 = bdry(r2 + Nb+1) -2*bdry(r1 + Nb+1) + bdry(k + Nb+1) 


          ! MIXED SUM
          mix_sum_k   = dX_k*LY_k - LX_k*dY_k
          mix_sum_kp1 = dX_kp1*LY_kp1 - LX_kp1*dY_kp1


          ! AUXILARY QUANTITIES
          temp_curv_k   = 4*mix_sum_k/sqrt(sum_k)**3 - C(k,t)
          temp_curv_kp1 = 4*mix_sum_kp1/sqrt(sum_kp1)**3 - C(k+1,t)



          !------------------------------------------------------------------------------------------------!
          ! SECOND Nb COMPONENTS OF THE FORCE       
          temp2 = -dX_k*sqrt(sum_k)**3/sum_k**3
          temp3 = ( (dX_kp1+LX_kp1)*sqrt(sum_kp1)**3 + 3*mix_sum_kp1*sqrt(sum_kp1)*dY_kp1 )/sum_kp1**3



          TEMP_E2 = 16*temp_curv_k*temp2 + 8*temp_curv_kp1*temp3
       ELSEIF(k==Nb-1)THEN
          xl1 = bdry(l1)
          xr1 = bdry(r1)
          xl2 = bdry(l2)

          ! DIFFERENCES IN X       ;   DIFFERENCES IN Y                            ;    SUM SQUARED
          dX_k   = xr1 - xl1       ;   dY_k   = bdry(r1 + Nb+1) - bdry(l1 + Nb+1)  ;    sum_k   = dX_k**2 + dY_k**2
          dX_km1 = bdry(k) - xl2   ;   dY_km1 = bdry(k  + Nb+1) - bdry(l2 + Nb+1)  ;    sum_km1 = dX_km1**2 + dY_km1**2

          LX_k   = xr1     -2*bdry(k) +  xl1       ;   LY_k   = bdry(r1 + Nb+1) -2*bdry(k  + Nb+1) + bdry(l1 + Nb+1)
          LX_km1 = bdry(k) -2*xl1     + xl2        ;   LY_km1 = bdry(k  + Nb+1) -2*bdry(l1 + Nb+1) + bdry(l2 + Nb+1)

          ! MIXED SUM
          mix_sum_k   = dX_k*LY_k - LX_k*dY_k
          mix_sum_km1 = dX_km1*LY_km1 - LX_km1*dY_km1


          ! AUXILARY QUANTITIES
          temp_curv_km1 = 4*mix_sum_km1/sqrt(sum_km1)**3 - C(k-1,t)
          temp_curv_k   = 4*mix_sum_k/sqrt(sum_k)**3 - C(k,t)


          !------------------------------------------------------------------------------------------------!
          ! SECOND Nb COMPONENTS OF THE FORCE       

          temp1 = ( (dX_km1-LX_km1)*sqrt(sum_km1)**3 - 3*mix_sum_km1*sqrt(sum_km1)*dY_km1 )/sum_km1**3
          temp2 = -dX_k*sqrt(sum_k)**3/sum_k**3


          TEMP_E2 = 8*temp_curv_km1*temp1 + 16*temp_curv_k*temp2
       ELSE
          xl1 = bdry(l1)
          xr1 = bdry(r1)
          xl2 = bdry(l2)
          xr2 = bdry(r2)


          ! DIFFERENCES IN X       ;   DIFFERENCES IN Y                            ;    SUM SQUARED
          dX_k   = xr1 - xl1       ;   dY_k   = bdry(r1 + Nb+1) - bdry(l1 + Nb+1)  ;    sum_k   = dX_k**2 + dY_k**2
          dX_kp1 = xr2 - bdry(k)   ;   dY_kp1 = bdry(r2 + Nb+1) - bdry(k  + Nb+1)  ;    sum_kp1 = dX_kp1**2 + dY_kp1**2
          dX_km1 = bdry(k) - xl2   ;   dY_km1 = bdry(k  + Nb+1) - bdry(l2 + Nb+1)  ;    sum_km1 = dX_km1**2 + dY_km1**2

          LX_k   = xr1     -2*bdry(k) +  xl1       ;   LY_k   = bdry(r1 + Nb+1) -2*bdry(k  + Nb+1) + bdry(l1 + Nb+1)
          LX_kp1 = xr2     -2*xr1     + bdry(k)    ;   LY_kp1 = bdry(r2 + Nb+1) -2*bdry(r1 + Nb+1) + bdry(k  + Nb+1) 
          LX_km1 = bdry(k) -2*xl1     + xl2        ;   LY_km1 = bdry(k  + Nb+1) -2*bdry(l1 + Nb+1) + bdry(l2 + Nb+1)


          ! MIXED SUM
          mix_sum_k   = dX_k*LY_k - LX_k*dY_k
          mix_sum_kp1 = dX_kp1*LY_kp1 - LX_kp1*dY_kp1
          mix_sum_km1 = dX_km1*LY_km1 - LX_km1*dY_km1


          ! AUXILARY QUANTITIES
          temp_curv_km1 = 4*mix_sum_km1/sqrt(sum_km1)**3 - C(k-1,t)
          temp_curv_k   = 4*mix_sum_k/sqrt(sum_k)**3 - C(k,t)
          temp_curv_kp1 = 4*mix_sum_kp1/sqrt(sum_kp1)**3 - C(k+1,t)



          !------------------------------------------------------------------------------------------------!
          ! SECOND Nb COMPONENTS OF THE FORCE       

          temp1 = ( (dX_km1-LX_km1)*sqrt(sum_km1)**3 - 3*mix_sum_km1*sqrt(sum_km1)*dY_km1 )/sum_km1**3
          temp2 = -dX_k*sqrt(sum_k)**3/sum_k**3
          temp3 = ( (dX_kp1+LX_kp1)*sqrt(sum_kp1)**3 + 3*mix_sum_kp1*sqrt(sum_kp1)*dY_kp1 )/sum_kp1**3



          TEMP_E2 = 8*temp_curv_km1*temp1 + 16*temp_curv_k*temp2 + 8*temp_curv_kp1*temp3


       ENDIF

       AX(k+Nb+1) =  AX(k+Nb+1)  - S2*hb*TEMP_E2

    ENDDO

  ENDSUBROUTINE FORCE



  SUBROUTINE compute_jacobian(bdry, jacobian, time)
    REAL(DP), DIMENSION(0:2*Nb+1) :: bdry
    REAL(DP), DIMENSION(0:2*Nb+1, 0:2*Nb+1) :: jacobian
    REAL(DP) :: time

    REAL(DP) :: xl1, xl2, xr1, xr2

    INTEGER(DP) :: k, i,j
    INTEGER(DP) :: l1,l2, r1, r2



    ! VARIOUS RECURRING QUANTITIES IN THE JACOBIAN
    REAL(DP) :: dX_k  , dY_k      ! RMK:  dX_k = X_{k+1} - X_{k-1} i.e. the difference centered at k
    REAL(DP) :: dX_kp1, dX_km1      
    REAL(DP) :: dY_kp1, dY_km1

    REAL(DP) :: sum_k, sum_kp1, sum_km1 ! sum_k = dX_k**2 + dY_k**2
    REAL(DP) :: mix_sum_k, mix_sum_kp1, mix_sum_km1 ! dXLY - LXdY

    !SECOND DERIVATIVE SUM/DIFF
    REAL(DP) :: LX_k  , LY_k
    REAL(DP) :: LX_kp1, LX_km1      
    REAL(DP) :: LY_kp1, LY_km1


    ! DERIVATIVES OF THE VARIOUS RECURRING QUANTITES ABOVE - ALL APPENDED WITH _DX OR _DY
    REAL(DP), DIMENSION(-2:2) ::  dX_k_DX   = (/  0.0, -1.0,  0.0, 1.0, 0.0 /)
    REAL(DP), DIMENSION(-2:2) ::  dX_kp1_DX = (/  0.0,  0.0, -1.0, 0.0, 1.0 /)
    REAL(DP), DIMENSION(-2:2) ::  dX_km1_DX = (/ -1.0,  0.0,  1.0, 0.0, 0.0 /)

    REAL(DP), DIMENSION(-2:2) ::  dX_k_DY   = 0.0
    REAL(DP), DIMENSION(-2:2) ::  dX_kp1_DY = 0.0
    REAL(DP), DIMENSION(-2:2) ::  dX_km1_DY = 0.0


    REAL(DP), DIMENSION(-2:2) ::  dY_k_DY   = (/  0.0, -1.0,  0.0, 1.0, 0.0 /)
    REAL(DP), DIMENSION(-2:2) ::  dY_kp1_DY = (/  0.0,  0.0, -1.0, 0.0, 1.0 /)
    REAL(DP), DIMENSION(-2:2) ::  dY_km1_DY = (/ -1.0,  0.0,  1.0, 0.0, 0.0 /)

    REAL(DP), DIMENSION(-2:2) ::  dY_k_DX   = 0.0
    REAL(DP), DIMENSION(-2:2) ::  dY_kp1_DX = 0.0
    REAL(DP), DIMENSION(-2:2) ::  dY_km1_DX = 0.0


    REAL(DP), DIMENSION(-2:2) ::  LX_k_DX   = (/  0.0,  1.0, -2.0,  1.0,  0.0 /)
    REAL(DP), DIMENSION(-2:2) ::  LX_kp1_DX = (/  0.0,  0.0,  1.0, -2.0,  1.0 /)
    REAL(DP), DIMENSION(-2:2) ::  LX_km1_DX = (/  1.0, -2.0,  1.0,  0.0,  0.0 /)

    REAL(DP), DIMENSION(-2:2) ::  LX_k_DY   = 0.0
    REAL(DP), DIMENSION(-2:2) ::  LX_kp1_DY = 0.0
    REAL(DP), DIMENSION(-2:2) ::  LX_km1_DY = 0.0

    REAL(DP), DIMENSION(-2:2) ::  LY_k_DY   = (/  0.0,  1.0, -2.0,  1.0,  0.0 /)
    REAL(DP), DIMENSION(-2:2) ::  LY_kp1_DY = (/  0.0,  0.0,  1.0, -2.0,  1.0 /)
    REAL(DP), DIMENSION(-2:2) ::  LY_km1_DY = (/  1.0, -2.0,  1.0,  0.0,  0.0 /)

    REAL(DP), DIMENSION(-2:2) ::  LY_k_DX   = 0.0
    REAL(DP), DIMENSION(-2:2) ::  LY_kp1_DX = 0.0
    REAL(DP), DIMENSION(-2:2) ::  LY_km1_DX = 0.0
    !--------------------------------------------------------------------------

    REAL(DP), DIMENSION(-2:2) ::  sum_k_DX, sum_kp1_DX, sum_km1_DX
    REAL(DP), DIMENSION(-2:2) ::  sum_k_DY, sum_kp1_DY, sum_km1_DY

    REAL(DP), DIMENSION(-2:2) ::  mix_sum_k_DX, mix_sum_kp1_DX, mix_sum_km1_DX
    REAL(DP), DIMENSION(-2:2) ::  mix_sum_k_DY, mix_sum_kp1_DY, mix_sum_km1_DY


    REAL(DP), DIMENSION(-2:2) ::  P_DX, Q_DX, R_DX
    REAL(DP), DIMENSION(-2:2) ::  P_DY, Q_DY, R_DY
    REAL(DP), DIMENSION(-2:2) ::  P_kp1, P_km1
    REAL(DP), DIMENSION(-2:2) ::  Q_k, Q_kp1, Q_km1
    REAL(DP), DIMENSION(-2:2) ::  R_kp1, R_km1

    REAL(DP), DIMENSION(-2:2) ::  norm_km1_DX, norm_k_DX, norm_kp1_DX
    REAL(DP), DIMENSION(-2:2) ::  norm_km1_DY, norm_k_DY, norm_kp1_DY

    REAL(DP), DIMENSION(-2:2) ::  temp1, temp2, temp3


    !--------------------------------------------------------------------------!
    !                       QUANTITIES FOR THE ENDPOINTS                       !
    !--------------------------------------------------------------------------!
    REAL(DP) :: dX_0, dX_1, dX_2, dY_0, dY_1, dY_2
    REAL(DP) :: LX_1, LX_2, LY_1, LY_2
    REAL(DP) :: mix_sum_1, mix_sum_2
    REAL(DP) :: sum_0, sum_1, sum_2


    ! ASSOCIATED DERIVATIVES
    REAL(DP), DIMENSION(0:4) :: dX_0_DX, dX_1_DX, dX_2_DX, dY_0_DX, dY_1_DX, dY_2_DX
    REAL(DP), DIMENSION(0:4) :: dX_0_DY, dX_1_DY, dX_2_DY, dY_0_DY, dY_1_DY, dY_2_DY
    REAL(DP), DIMENSION(0:4) :: LX_1_DX, LX_2_DX, LY_1_DX, LY_2_DX    
    REAL(DP), DIMENSION(0:4) :: LX_1_DY, LX_2_DY, LY_1_DY, LY_2_DY
    REAL(DP), DIMENSION(0:4) :: mix_sum_1_DX, mix_sum_2_DX, mix_sum_1_DY, mix_sum_2_DY
    REAL(DP), DIMENSION(0:4) :: sum_0_DX, sum_1_DX, sum_2_DX, sum_0_DY, sum_1_DY, sum_2_DY
    REAL(DP), DIMENSION(0:4) :: norm_0_DX, norm_1_DX, norm_2_DX, norm_0_DY, norm_1_DY, norm_2_DY

    !LOCAL STORAGE
    REAL(DP), DIMENSION(0:4) :: P_0, P_1, Q_0, Q_1
    REAL(DP), DIMENSION(0:4) :: Q_1a_DX, Q_1b_DX, Q_2a_DX, Q_1a_DY, Q_1b_DY, Q_2a_DY
    REAL(DP), DIMENSION(0:4) :: P_0_DX, P_1_DX, P_2_DX, P_0_DY, P_1_DY, P_2_DY  
    REAL(DP), DIMENSION(0:4) :: Q_0_DX, Q_1_DX, Q_0_DY, Q_1_DY


    jacobian = 0.0

    DO k = 2, Nb-2

       l1  = k-1
       l2  = k-2
       r1  = k+1
       r2  = k+2

       IF(ABS( bdry(l1) - bdry(k) ) > .5*xf)THEN
          xl1 = bdry(l1) - xf
       ELSE
          xl1 = bdry(l1)
       ENDIF

       IF(ABS( bdry(r1) - bdry(k) ) > .5*xf)THEN
          xr1 = bdry(r1) + xf
       ELSE
          xr1 = bdry(r1)
       ENDIF

       IF(ABS( bdry(l2) - bdry(k) ) > .5*xf)THEN
          xl2 = bdry(l2) - xf
       ELSE
          xl2 = bdry(l2)
       ENDIF

       IF(ABS( bdry(r2) - bdry(k) ) > .5*xf)THEN
          xr2 = bdry(r2) + xf
       ELSE
          xr2 = bdry(r2)
       ENDIF


       ! DIFFERENCES IN X       ;   DIFFERENCES IN Y                            ;    SUM SQUARED
       dX_k   = xr1 - xl1       ;   dY_k   = bdry(r1 + Nb+1) - bdry(l1 + Nb+1)  ;    sum_k   = dX_k**2 + dY_k**2
       dX_kp1 = xr2 - bdry(k)   ;   dY_kp1 = bdry(r2 + Nb+1) - bdry(k + Nb+1)   ;    sum_kp1 = dX_kp1**2 + dY_kp1**2
       dX_km1 = bdry(k) - xl2   ;   dY_km1 = bdry(k  + Nb+1) - bdry(l2 + Nb+1)  ;    sum_km1 = dX_km1**2 + dY_km1**2

       LX_k   = xr1     -2*bdry(k) +  xl1       ;   LY_k   = bdry(r1 + Nb+1) -2*bdry(k  + Nb+1) + bdry(l1 + Nb+1)
       LX_kp1 = xr2     -2*xr1     + bdry(k)    ;   LY_kp1 = bdry(r2 + Nb+1) -2*bdry(r1 + Nb+1) + bdry(k  + Nb+1) 
       LX_km1 = bdry(k) -2*xl1     + xl2        ;   LY_km1 = bdry(k  + Nb+1) -2*bdry(l1 + Nb+1) + bdry(l2 + Nb+1)

       ! DERIVATIVES OF THE SUM SQUARED
       sum_k_DX   = 2*dX_k*dX_k_DX       ;       sum_k_DY   = 2*dY_k*dY_k_DY
       sum_kp1_DX = 2*dX_kp1*dX_kp1_DX   ;       sum_kp1_DY = 2*dY_kp1*dY_kp1_DY
       sum_km1_DX = 2*dX_km1*dX_km1_DX   ;       sum_km1_DY = 2*dY_km1*dY_km1_DY




       ! MIXED SUM
       mix_sum_k   = dX_k*LY_k - LX_k*dY_k
       mix_sum_kp1 = dX_kp1*LY_kp1 - LX_kp1*dY_kp1
       mix_sum_km1 = dX_km1*LY_km1 - LX_km1*dY_km1

       ! MIXED SUM DERIVATIVES
       mix_sum_k_DX   = dX_k_DX*LY_k     - LX_k_DX*dY_k          ;   mix_sum_k_DY   = dX_k*LY_k_DY     - LX_k*dY_k_DY
       mix_sum_kp1_DX = dX_kp1_DX*LY_kp1 - LX_kp1_DX*dY_kp1      ;   mix_sum_kp1_DY = dX_kp1*LY_kp1_DY - LX_kp1*dY_kp1_DY
       mix_sum_km1_DX = dX_km1_DX*LY_km1 - LX_km1_DX*dY_km1      ;   mix_sum_km1_DY = dX_km1*LY_km1_DY - LX_km1*dY_km1_DY

       ! DERIVATIVES OF NORM
       norm_km1_DX = sum_km1**(-.5)*dX_km1*dX_km1_DX     ;    norm_km1_DY = sum_km1**(-.5)*dY_km1*dY_km1_DY
       norm_k_DX   = sum_k**(-.5)*dX_k*dX_k_DX           ;    norm_k_DY   = sum_k**(-.5)*dY_k*dY_k_DY
       norm_kp1_DX = sum_kp1**(-.5)*dX_kp1*dX_kp1_DX     ;    norm_kp1_DY = sum_kp1**(-.5)*dY_kp1*dY_kp1_DY




       !------------------------------------------------------------------------------------------------!
       !                    COMPUTATIONS FOR THE X DERIVATIVE OF EQUATIONS 0 - Nb-1                     !
       !------------------------------------------------------------------------------------------------!
       P_km1 = norm_km1_DX*dX_km1/(sqrt(sum_km1)*2*hb) &
            + (sqrt(sum_km1)/(2*hb)-1)*(dX_km1_DX*sqrt(sum_km1)-dX_km1*norm_km1_DX)/sum_km1
       P_kp1 = norm_kp1_DX*dX_kp1/(sqrt(sum_kp1)*2*hb) &
            + (sqrt(sum_kp1)/(2*hb)-1)*(dX_kp1_DX*sqrt(sum_kp1)-dX_kp1*norm_kp1_DX)/sum_kp1

       P_DX =  P_km1 - P_kp1        


       !--------------------!

       temp1 = 4*( mix_sum_km1_DX*sqrt(sum_km1)**3 - 1.5*(mix_sum_km1)*sqrt(sum_km1)*sum_km1_DX )/sum_km1**3
       temp1 = temp1*( (LY_km1 - dY_km1)*sqrt(sum_km1)**(-3) - 3*mix_sum_km1*dX_km1*sqrt(sum_km1)**(-5) )
       temp2 = 4*mix_sum_km1/sqrt(sum_km1)**3 - C(k-1,time)
       temp3 = (LY_km1_DX - dY_km1_DX)*sqrt(sum_km1)**(-3) - 3*(LY_km1-dY_km1)*sqrt(sum_km1)**(-4)*norm_km1_DX &
            - 3*(mix_sum_km1_DX*sqrt(sum_km1)**(-5)*dX_km1 + mix_sum_km1*(dX_km1_DX*sqrt(sum_km1)**(-5) &
            - 5*dX_km1*sqrt(sum_km1)**(-6)*norm_km1_DX ))

       Q_km1 = temp1 + temp2*temp3




       temp1 = 4*( mix_sum_k_DX*sqrt(sum_k)**3 - 1.5*(mix_sum_k)*sqrt(sum_k)*sum_k_DX )/sum_k**3
       temp1 = temp1*(2*dY_k*sqrt(sum_k)**(-3) )
       temp2 = 4*mix_sum_k/sqrt(sum_k)**3 - C(k,time)
       temp3 = 2*( (dY_k_DX)*sqrt(sum_k)**(-3) - 3*(dY_k)*sqrt(sum_k)**(-4)*norm_k_DX )
       Q_k = temp1 + temp2*temp3



       temp1 = 4*( mix_sum_kp1_DX*sqrt(sum_kp1)**3 - 1.5*(mix_sum_kp1)*sqrt(sum_kp1)*sum_kp1_DX )/sum_kp1**3
       temp1 = temp1*( (-LY_kp1 - dY_kp1)*sqrt(sum_kp1)**(-3) + 3*mix_sum_kp1*dX_kp1*sqrt(sum_kp1)**(-5) )
       temp2 = 4*mix_sum_kp1/sqrt(sum_kp1)**3 - C(k+1,time)
       temp3 = (-LY_kp1_DX - dY_kp1_DX)*sqrt(sum_kp1)**(-3) - 3*(-LY_kp1-dY_kp1)*sqrt(sum_kp1)**(-4)*norm_kp1_DX &
            + 3*(mix_sum_kp1_DX*sqrt(sum_kp1)**(-5)*dX_kp1 + mix_sum_kp1*(dX_kp1_DX*sqrt(sum_kp1)**(-5) &
            - 5*dX_kp1*sqrt(sum_kp1)**(-6)*norm_kp1_DX ))
       !-------------!
       Q_kp1 = temp1 + temp2*temp3



       Q_DX  = 8*(Q_kp1 + Q_k + Q_km1)


       !ORIGINAL
       DO i = -2,2
          jacobian(k, k+i ) = -S1*P_DX(i) - S2*hb*Q_DX(i)
       ENDDO


       !------------------------------------------------------------------------------------------------!
       !                   COMPUTATIONS FOR THE X DERIVATIVE OF EQUATIONS Nb - 2Nb-1                    !
       !------------------------------------------------------------------------------------------------!


       P_km1 = norm_km1_DX*dY_km1/(sqrt(sum_km1)*2*hb) &
            + (sqrt(sum_km1)/(2*hb)-1)*(dY_km1_DX*sqrt(sum_km1)-dY_km1*norm_km1_DX)/sum_km1
       P_kp1 = norm_kp1_DX*dY_kp1/(sqrt(sum_kp1)*2*hb) &
            + (sqrt(sum_kp1)/(2*hb)-1)*(dY_kp1_DX*sqrt(sum_kp1)-dY_kp1*norm_kp1_DX)/sum_kp1

       P_DX =  P_km1 - P_kp1        


       !--------------------!

       temp1 = 4*( mix_sum_km1_DX*sqrt(sum_km1)**3 - 1.5*(mix_sum_km1)*sqrt(sum_km1)*sum_km1_DX )/sum_km1**3
       temp1 = temp1*( (dX_km1 - LX_km1)*sqrt(sum_km1)**(-3) - 3*mix_sum_km1*dY_km1*sqrt(sum_km1)**(-5) )
       temp2 = 4*mix_sum_km1/sqrt(sum_km1)**3 - C(k-1,time)
       temp3 = (dX_km1_DX - LX_km1_DX)*sqrt(sum_km1)**(-3) - 3*(dX_km1 - LX_km1)*sqrt(sum_km1)**(-4)*norm_km1_DX &
            - 3*(mix_sum_km1_DX*sqrt(sum_km1)**(-5)*dY_km1 + mix_sum_km1*(dY_km1_DX*sqrt(sum_km1)**(-5) &
            - 5*dY_km1*sqrt(sum_km1)**(-6)*norm_km1_DX ))

       Q_km1 = temp1 + temp2*temp3




       temp1 = 4*( mix_sum_k_DX*sqrt(sum_k)**3 - 1.5*(mix_sum_k)*sqrt(sum_k)*sum_k_DX )/sum_k**3
       temp1 = temp1*(-2*dX_k*sqrt(sum_k)**(-3) )
       temp2 = 4*mix_sum_k/sqrt(sum_k)**3 - C(k,time)
       temp3 = -2*( (dX_k_DX)*sqrt(sum_k)**(-3) - 3*(dX_k)*sqrt(sum_k)**(-4)*norm_k_DX )
       Q_k = temp1 + temp2*temp3



       temp1 = 4*( mix_sum_kp1_DX*sqrt(sum_kp1)**3 - 1.5*(mix_sum_kp1)*sqrt(sum_kp1)*sum_kp1_DX )/sum_kp1**3
       temp1 = temp1*( (dX_kp1 + LX_kp1)*sqrt(sum_kp1)**(-3) + 3*mix_sum_kp1*dY_kp1*sqrt(sum_kp1)**(-5) )
       temp2 = 4*mix_sum_kp1/sqrt(sum_kp1)**3 - C(k+1,time)
       temp3 = (dX_kp1_DX + LX_kp1_DX)*sqrt(sum_kp1)**(-3) - 3*(dX_kp1 + LX_kp1)*sqrt(sum_kp1)**(-4)*norm_kp1_DX &
            + 3*(mix_sum_kp1_DX*sqrt(sum_kp1)**(-5)*dY_kp1 + mix_sum_kp1*(dY_kp1_DX*sqrt(sum_kp1)**(-5) &
            - 5*dY_kp1*sqrt(sum_kp1)**(-6)*norm_kp1_DX ))
       !-------------!
       Q_kp1 = temp1 + temp2*temp3



       Q_DX  = 8*(Q_kp1 + Q_k + Q_km1)

       !ORIGINAL
       DO i = -2,2
          jacobian(k+Nb+1, k+i ) = -S1*P_DX(i) - S2*hb*Q_DX(i)
       ENDDO





       !------------------------------------------------------------------------------------------------!
       !                    COMPUTATIONS FOR THE Y DERIVATIVE OF EQUATIONS 0 - Nb-1                     !
       !------------------------------------------------------------------------------------------------!
       P_km1 = norm_km1_DY*dX_km1/(sqrt(sum_km1)*2*hb) &
            + (sqrt(sum_km1)/(2*hb)-1)*(dX_km1_DY*sqrt(sum_km1)-dX_km1*norm_km1_DY)/sum_km1
       P_kp1 = norm_kp1_DY*dX_kp1/(sqrt(sum_kp1)*2*hb) &
            + (sqrt(sum_kp1)/(2*hb)-1)*(dX_kp1_DY*sqrt(sum_kp1)-dX_kp1*norm_kp1_DY)/sum_kp1

       P_DY =  P_km1 - P_kp1        


       !--------------------!

       temp1 = 4*( mix_sum_km1_DY*sqrt(sum_km1)**3 - 1.5*(mix_sum_km1)*sqrt(sum_km1)*sum_km1_DY )/sum_km1**3
       temp1 = temp1*( (LY_km1 - dY_km1)*sqrt(sum_km1)**(-3) - 3*mix_sum_km1*dX_km1*sqrt(sum_km1)**(-5) )
       temp2 = 4*mix_sum_km1/sqrt(sum_km1)**3 - C(k-1,time)
       temp3 = (LY_km1_DY - dY_km1_DY)*sqrt(sum_km1)**(-3) - 3*(LY_km1-dY_km1)*sqrt(sum_km1)**(-4)*norm_km1_DY &
            - 3*(mix_sum_km1_DY*sqrt(sum_km1)**(-5)*dX_km1 + mix_sum_km1*(dX_km1_DY*sqrt(sum_km1)**(-5) &
            - 5*dX_km1*sqrt(sum_km1)**(-6)*norm_km1_DY ))

       Q_km1 = temp1 + temp2*temp3



       temp1 = 4*( mix_sum_k_DY*sqrt(sum_k)**3 - 1.5*(mix_sum_k)*sqrt(sum_k)*sum_k_DY )/sum_k**3
       temp1 = temp1*(2*dY_k*sqrt(sum_k)**(-3) )
       temp2 = 4*mix_sum_k/sqrt(sum_k)**3 - C(k,time)
       temp3 = 2*( (dY_k_DY)*sqrt(sum_k)**(-3) - 3*(dY_k)*sqrt(sum_k)**(-4)*norm_k_DY )
       Q_k = temp1 + temp2*temp3



       temp1 = 4*( mix_sum_kp1_DY*sqrt(sum_kp1)**3 - 1.5*(mix_sum_kp1)*sqrt(sum_kp1)*sum_kp1_DY )/sum_kp1**3
       temp1 = temp1*( (-LY_kp1 - dY_kp1)*sqrt(sum_kp1)**(-3) + 3*mix_sum_kp1*dX_kp1*sqrt(sum_kp1)**(-5) )
       temp2 = 4*mix_sum_kp1/sqrt(sum_kp1)**3 - C(k+1,time)
       temp3 = (-LY_kp1_DY - dY_kp1_DY)*sqrt(sum_kp1)**(-3) - 3*(-LY_kp1-dY_kp1)*sqrt(sum_kp1)**(-4)*norm_kp1_DY &
            + 3*(mix_sum_kp1_DY*sqrt(sum_kp1)**(-5)*dX_kp1 + mix_sum_kp1*(dX_kp1_DY*sqrt(sum_kp1)**(-5) &
            - 5*dX_kp1*sqrt(sum_kp1)**(-6)*norm_kp1_DY ))
       !-------------!
       Q_kp1 = temp1 + temp2*temp3



       Q_DY  = 8*(Q_kp1 + Q_k + Q_km1)

       DO i = -2,2
          jacobian(k , k+i + Nb+1) = -S1*P_DY(i) - S2*hb*Q_DY(i)
       ENDDO


       !------------------------------------------------------------------------------------------------!
       !                   COMPUTATIONS FOR THE Y DERIVATIVE OF EQUATIONS Nb - 2Nb-1                    !
       !------------------------------------------------------------------------------------------------!

       P_km1 = norm_km1_DY*dY_km1/(sqrt(sum_km1)*2*hb) &
            + (sqrt(sum_km1)/(2*hb)-1)*(dY_km1_DY*sqrt(sum_km1)-dY_km1*norm_km1_DY)/sum_km1
       P_kp1 = norm_kp1_DY*dY_kp1/(sqrt(sum_kp1)*2*hb) &
            + (sqrt(sum_kp1)/(2*hb)-1)*(dY_kp1_DY*sqrt(sum_kp1)-dY_kp1*norm_kp1_DY)/sum_kp1

       P_DY =  P_km1 - P_kp1        


       !--------------------!

       temp1 = 4*( mix_sum_km1_DY*sqrt(sum_km1)**3 - 1.5*(mix_sum_km1)*sqrt(sum_km1)*sum_km1_DY )/sum_km1**3
       temp1 = temp1*( (dX_km1 - LX_km1)*sqrt(sum_km1)**(-3) - 3*mix_sum_km1*dY_km1*sqrt(sum_km1)**(-5) )
       temp2 = 4*mix_sum_km1/sqrt(sum_km1)**3 - C(k-1,time)
       temp3 = (dX_km1_DY - LX_km1_DY)*sqrt(sum_km1)**(-3) - 3*(dX_km1 - LX_km1)*sqrt(sum_km1)**(-4)*norm_km1_DY &
            - 3*(mix_sum_km1_DY*sqrt(sum_km1)**(-5)*dY_km1 + mix_sum_km1*(dY_km1_DY*sqrt(sum_km1)**(-5) &
            - 5*dY_km1*sqrt(sum_km1)**(-6)*norm_km1_DY ))

       Q_km1 = temp1 + temp2*temp3




       temp1 = 4*( mix_sum_k_DY*sqrt(sum_k)**3 - 1.5*(mix_sum_k)*sqrt(sum_k)*sum_k_DY )/sum_k**3
       temp1 = temp1*(-2*dX_k*sqrt(sum_k)**(-3) )
       temp2 = 4*mix_sum_k/sqrt(sum_k)**3 - C(k,time)
       temp3 = -2*( (dX_k_DY)*sqrt(sum_k)**(-3) - 3*(dX_k)*sqrt(sum_k)**(-4)*norm_k_DY )
       Q_k = temp1 + temp2*temp3



       temp1 = 4*( mix_sum_kp1_DY*sqrt(sum_kp1)**3 - 1.5*(mix_sum_kp1)*sqrt(sum_kp1)*sum_kp1_DY )/sum_kp1**3
       temp1 = temp1*( (dX_kp1 + LX_kp1)*sqrt(sum_kp1)**(-3) + 3*mix_sum_kp1*dY_kp1*sqrt(sum_kp1)**(-5) )
       temp2 = 4*mix_sum_kp1/sqrt(sum_kp1)**3 - C(k+1,time)
       temp3 = (dX_kp1_DY + LX_kp1_DY)*sqrt(sum_kp1)**(-3) - 3*(dX_kp1 + LX_kp1)*sqrt(sum_kp1)**(-4)*norm_kp1_DY &
            + 3*(mix_sum_kp1_DY*sqrt(sum_kp1)**(-5)*dY_kp1 + mix_sum_kp1*(dY_kp1_DY*sqrt(sum_kp1)**(-5) &
            - 5*dY_kp1*sqrt(sum_kp1)**(-6)*norm_kp1_DY ))
       !-------------!
       Q_kp1 = temp1 + temp2*temp3



       Q_DY  = 8*(Q_kp1 + Q_k + Q_km1)



       DO i = -2,2
          jacobian(k+Nb+1, k+i + Nb+1) = -S1*P_DY(i) - S2*hb*Q_DY(i)
       ENDDO


    ENDDO

    !--------------------------------------- ENDPOINT CASES ----------------------------------------------!
    dX_0_DX = (/ -1., 1., 0., 0., 0. /) ; dX_1_DX = (/ -1., 0., 1., 0., 0. /) ; dX_2_DX = (/ 0., -1., 0., 1., 0. /)
    dY_0_DY = (/ -1., 1., 0., 0., 0. /) ; dY_1_DY = (/ -1., 0., 1., 0., 0. /) ; dY_2_DY = (/ 0., -1., 0., 1., 0. /)
    dX_0_DY = 0. ; dX_1_DY = 0. ; dX_2_DY = 0.
    dY_0_DX = 0. ; dY_1_DX = 0. ; dY_2_DX = 0.

    LX_1_DX = (/ 1., -2., 1., 0., 0. /) ; LX_2_DX = (/ 0., 1., -2., 1., 0. /)
    LY_1_DY = (/ 1., -2., 1., 0., 0. /) ; LY_2_DY = (/ 0., 1., -2., 1., 0. /)
    LX_1_DY = 0. ; LX_2_DY = 0.
    LY_1_DX = 0. ; LY_2_DX = 0.


    dX_0 = bdry(1) - bdry(0)    ;    dY_0 = bdry(Nb+2) - bdry(Nb+1)
    dX_1 = bdry(2) - bdry(0)    ;    dY_1 = bdry(Nb+3) - bdry(Nb+1)
    dX_2 = bdry(3) - bdry(1)    ;    dY_2 = bdry(Nb+4) - bdry(Nb+2)

    LX_1 = bdry(2) -2*bdry(1) + bdry(0)    ;    LY_1 = bdry(Nb+3) -2*bdry(Nb+2) + bdry(Nb+1)
    LX_2 = bdry(3) -2*bdry(2) + bdry(1)    ;    LY_2 = bdry(Nb+4) -2*bdry(Nb+3) + bdry(Nb+2)

    sum_0 = dX_0**2 + dY_0**2   ;   sum_1 = dX_1**2 + dY_1**2   ;    sum_2 = dX_2**2 + dY_2**2
    mix_sum_1 = dX_1*LY_1 - LX_1*dY_1    ;     mix_sum_2 = dX_2*LY_2 - LX_2*dY_2

    ! DERIVATIVES OF THE SUM SQUARED
    sum_0_DX = 2*dX_0*dX_0_DX   ;       sum_0_DY = 2*dY_0*dY_0_DY
    sum_1_DX = 2*dX_1*dX_1_DX   ;       sum_1_DY = 2*dY_1*dY_1_DY
    sum_2_DX = 2*dX_2*dX_2_DX   ;       sum_2_DY = 2*dY_2*dY_2_DY

    ! MIXED SUM DERIVATIVES
    mix_sum_1_DX   = dX_1_DX*LY_1 - LX_1_DX*dY_1    ;   mix_sum_1_DY   = dX_1*LY_1_DY  - LX_1*dY_1_DY
    mix_sum_2_DX   = dX_2_DX*LY_2 - LX_2_DX*dY_2    ;   mix_sum_2_DY = dX_2*LY_2_DY - LX_2*dY_2_DY


    ! DERIVATIVES OF NORM
    norm_0_DX = sum_0**(-.5)*dX_0*dX_0_DX     ;    norm_0_DY = sum_0**(-.5)*dY_0*dY_0_DY
    norm_1_DX = sum_1**(-.5)*dX_1*dX_1_DX     ;    norm_1_DY = sum_1**(-.5)*dY_1*dY_1_DY
    norm_2_DX = sum_2**(-.5)*dX_2*dX_2_DX     ;    norm_2_DY = sum_2**(-.5)*dY_2*dY_2_DY


    !-------------------------------------------------------------------------------------------!
    !              NECESSARY QUANTITES FOR JACOBIAN CONTRIBUTIONS FROM F_0, F_1                 !
    !-------------------------------------------------------------------------------------------!

    !  X - DERIVATIVES
    P_0_DX = norm_0_DX*dX_0/(sqrt(sum_0)*hb) &
         + (sqrt(sum_0)/(hb)-1)*(dX_0_DX*sqrt(sum_0)-dX_0*norm_0_DX)/sum_0
    P_1_DX = norm_1_DX*dX_1/(sqrt(sum_1)*2*hb) &
         + (sqrt(sum_1)/(2*hb)-1)*(dX_1_DX*sqrt(sum_1)-dX_1*norm_1_DX)/sum_1
    P_2_DX = norm_2_DX*dX_2/(sqrt(sum_2)*2*hb) &
         + (sqrt(sum_2)/(2*hb)-1)*(dX_2_DX*sqrt(sum_2)-dX_2*norm_2_DX)/sum_2

    temp1 = 4*( mix_sum_1_DX*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DX )/sum_1**3
    temp1 = temp1*( (-LY_1 - dY_1)*sqrt(sum_1)**(-3) + 3*mix_sum_1*dX_1*sqrt(sum_1)**(-5) )
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(1_DP,time)
    temp3 = (-LY_1_DX - dY_1_DX)*sqrt(sum_1)**(-3) - 3*(-LY_1-dY_1)*sqrt(sum_1)**(-4)*norm_1_DX &
         + 3*(mix_sum_1_DX*sqrt(sum_1)**(-5)*dX_1 + mix_sum_1*(dX_1_DX*sqrt(sum_1)**(-5) &
         - 5*dX_1*sqrt(sum_1)**(-6)*norm_1_DX ))

    Q_1a_DX = temp1 + temp2*temp3


    temp1 = 4*( mix_sum_1_DX*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DX )/sum_1**3
    temp1 = temp1*(2*dY_1)*sqrt(sum_1)**(-3)
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(1_DP,time)
    temp3 = 2*(dY_1_DX*sqrt(sum_1)**(-3) - 3*dY_1*sqrt(sum_1)**(-4)*norm_1_DX)

    Q_1b_DX = temp1 + temp2*temp3



    temp1 = 4*( mix_sum_2_DX*sqrt(sum_2)**3 - 1.5*(mix_sum_2)*sqrt(sum_2)*sum_2_DX )/sum_2**3
    temp1 = temp1*( (-LY_2 - dY_2)*sqrt(sum_2)**(-3) + 3*mix_sum_2*dX_2*sqrt(sum_2)**(-5) )
    temp2 = 4*mix_sum_2/sqrt(sum_2)**3 - C(2_DP,time)
    temp3 = (-LY_2_DX - dY_2_DX)*sqrt(sum_2)**(-3) - 3*(-LY_2-dY_2)*sqrt(sum_2)**(-4)*norm_2_DX &
         + 3*(mix_sum_2_DX*sqrt(sum_2)**(-5)*dX_2 + mix_sum_2*(dX_2_DX*sqrt(sum_2)**(-5) &
         - 5*dX_2*sqrt(sum_2)**(-6)*norm_2_DX ))

    Q_2a_DX = temp1 + temp2*temp3


    ! Y DERIVATIVES

    P_0_DY = norm_0_DY*dX_0/(sqrt(sum_0)*hb) &
         + (sqrt(sum_0)/(hb)-1)*(dX_0_DY*sqrt(sum_0)-dX_0*norm_0_DY)/sum_0
    P_1_DY = norm_1_DY*dX_1/(sqrt(sum_1)*2*hb) &
         + (sqrt(sum_1)/(2*hb)-1)*(dX_1_DY*sqrt(sum_1)-dX_1*norm_1_DY)/sum_1
    P_2_DY = norm_2_DY*dX_2/(sqrt(sum_2)*2*hb) &
         + (sqrt(sum_2)/(2*hb)-1)*(dX_2_DY*sqrt(sum_2)-dX_2*norm_2_DY)/sum_2

    temp1 = 4*( mix_sum_1_DY*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DY )/sum_1**3
    temp1 = temp1*( (-LY_1 - dY_1)*sqrt(sum_1)**(-3) + 3*mix_sum_1*dX_1*sqrt(sum_1)**(-5) )
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(1_DP,time)
    temp3 = (-LY_1_DY - dY_1_DY)*sqrt(sum_1)**(-3) - 3*(-LY_1-dY_1)*sqrt(sum_1)**(-4)*norm_1_DY &
         + 3*(mix_sum_1_DY*sqrt(sum_1)**(-5)*dX_1 + mix_sum_1*(dX_1_DY*sqrt(sum_1)**(-5) &
         - 5*dX_1*sqrt(sum_1)**(-6)*norm_1_DY ))

    Q_1a_DY = temp1 + temp2*temp3


    temp1 = 4*( mix_sum_1_DY*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DY )/sum_1**3
    temp1 = temp1*(2*dY_1)*sqrt(sum_1)**(-3)
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(1_DP,time)
    temp3 = 2*(dY_1_DY*sqrt(sum_1)**(-3) - 3*dY_1*sqrt(sum_1)**(-4)*norm_1_DY)

    Q_1b_DY = temp1 + temp2*temp3



    temp1 = 4*( mix_sum_2_DY*sqrt(sum_2)**3 - 1.5*(mix_sum_2)*sqrt(sum_2)*sum_2_DY )/sum_2**3
    temp1 = temp1*( (-LY_2 - dY_2)*sqrt(sum_2)**(-3) + 3*mix_sum_2*dX_2*sqrt(sum_2)**(-5) )
    temp2 = 4*mix_sum_2/sqrt(sum_2)**3 - C(2_DP,time)
    temp3 = (-LY_2_DY - dY_2_DY)*sqrt(sum_2)**(-3) - 3*(-LY_2-dY_2)*sqrt(sum_2)**(-4)*norm_2_DY &
         + 3*(mix_sum_2_DY*sqrt(sum_2)**(-5)*dX_2 + mix_sum_2*(dX_2_DY*sqrt(sum_2)**(-5) &
         - 5*dX_2*sqrt(sum_2)**(-6)*norm_2_DY ))

    Q_2a_DY = temp1 + temp2*temp3

    ! JACOBIAN ENTRIES CORRESPONDING TO F_0, F_1 WRT X DERIVATIVES
    P_0 = -(P_0_DX + P_1_DX)
    Q_0 = 8*Q_1a_DX

    DO i = 0,3
       jacobian(0, i) = -S1*P_0(i) - S2*hb*Q_0(i)
    ENDDO


    P_1 = -P_2_DX + P_0_DX
    Q_1 = 8*(Q_1b_DX + Q_2a_DX)

    DO i = 0,3
       jacobian(1, i) = -S1*P_1(i) - S2*hb*Q_1(i)
    ENDDO

    ! JACOBIAN ENTRIES CORRESPONDING TO F_0, F_1 WRT Y DERIVATIVES
    P_0 = -(P_0_DY + P_1_DY)
    Q_0 = 8*Q_1a_DY

    DO i = 0,3
       jacobian(0, i+Nb+1) = -S1*P_0(i) - S2*hb*Q_0(i)
    ENDDO


    P_1 = -P_2_DY + P_0_DY
    Q_1 = 8*(Q_1b_DY + Q_2a_DY)

    DO i = 0,4
       jacobian(1, i+Nb+1) = -S1*P_1(i) - S2*hb*Q_1(i)
    ENDDO


    !-------------------------------------------------------------------------------------------!
    !              NECESSARY QUANTITES FOR JACOBIAN CONTRIBUTIONS FROM F_Nb, F_Nbp1             !
    !-------------------------------------------------------------------------------------------!

    !  X - DERIVATIVES
    P_0_DX = norm_0_DX*dY_0/(sqrt(sum_0)*hb) &
         + (sqrt(sum_0)/(hb)-1)*(dY_0_DX*sqrt(sum_0)-dY_0*norm_0_DX)/sum_0
    P_1_DX = norm_1_DX*dY_1/(sqrt(sum_1)*2*hb) &
         + (sqrt(sum_1)/(2*hb)-1)*(dY_1_DX*sqrt(sum_1)-dY_1*norm_1_DX)/sum_1
    P_2_DX = norm_2_DX*dY_2/(sqrt(sum_2)*2*hb) &
         + (sqrt(sum_2)/(2*hb)-1)*(dY_2_DX*sqrt(sum_2)-dY_2*norm_2_DX)/sum_2

    temp1 = 4*( mix_sum_1_DX*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DX )/sum_1**3
    temp1 = temp1*( (dX_1 + LX_1)*sqrt(sum_1)**(-3) + 3*mix_sum_1*dY_1*sqrt(sum_1)**(-5) )
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(1_DP,time)
    temp3 = (dX_1_DX + LX_1_DX)*sqrt(sum_1)**(-3) - 3*(dX_1 + LX_1)*sqrt(sum_1)**(-4)*norm_1_DX &
         + 3*(mix_sum_1_DX*sqrt(sum_1)**(-5)*dY_1 + mix_sum_1*(dY_1_DX*sqrt(sum_1)**(-5) &
         - 5*dY_1*sqrt(sum_1)**(-6)*norm_1_DX ))

    Q_1a_DX = temp1 + temp2*temp3


    temp1 = 4*( mix_sum_1_DX*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DX )/sum_1**3
    temp1 = temp1*(-2*dX_1)*sqrt(sum_1)**(-3)
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(1_DP,time)
    temp3 = -2*(dX_1_DX*sqrt(sum_1)**(-3) - 3*dX_1*sqrt(sum_1)**(-4)*norm_1_DX)

    Q_1b_DX = temp1 + temp2*temp3


    temp1 = 4*( mix_sum_2_DX*sqrt(sum_2)**3 - 1.5*(mix_sum_2)*sqrt(sum_2)*sum_2_DX )/sum_2**3
    temp1 = temp1*( (dX_2 + LX_2)*sqrt(sum_2)**(-3) + 3*mix_sum_2*dY_2*sqrt(sum_2)**(-5) )
    temp2 = 4*mix_sum_2/sqrt(sum_2)**3 - C(2_DP,time)
    temp3 = (dX_2_DX + LX_2_DX)*sqrt(sum_2)**(-3) - 3*(dX_2 + LX_2)*sqrt(sum_2)**(-4)*norm_2_DX &
         + 3*(mix_sum_2_DX*sqrt(sum_2)**(-5)*dY_2 + mix_sum_2*(dY_2_DX*sqrt(sum_2)**(-5) &
         - 5*dY_2*sqrt(sum_2)**(-6)*norm_2_DX ))

    Q_2a_DX = temp1 + temp2*temp3


    ! Y DERIVATIVES
    P_0_DY = norm_0_DY*dY_0/(sqrt(sum_0)*hb) &
         + (sqrt(sum_0)/(hb)-1)*(dY_0_DY*sqrt(sum_0)-dY_0*norm_0_DY)/sum_0
    P_1_DY = norm_1_DY*dY_1/(sqrt(sum_1)*2*hb) &
         + (sqrt(sum_1)/(2*hb)-1)*(dY_1_DY*sqrt(sum_1)-dY_1*norm_1_DY)/sum_1
    P_2_DY = norm_2_DY*dY_2/(sqrt(sum_2)*2*hb) &
         + (sqrt(sum_2)/(2*hb)-1)*(dY_2_DY*sqrt(sum_2)-dY_2*norm_2_DY)/sum_2

    temp1 = 4*( mix_sum_1_DY*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DY )/sum_1**3
    temp1 = temp1*( (dX_1 + LX_1)*sqrt(sum_1)**(-3) + 3*mix_sum_1*dY_1*sqrt(sum_1)**(-5) )
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(1_DP,time)
    temp3 = (dX_1_DY + LX_1_DY)*sqrt(sum_1)**(-3) - 3*(dX_1 + LX_1)*sqrt(sum_1)**(-4)*norm_1_DY &
         + 3*(mix_sum_1_DY*sqrt(sum_1)**(-5)*dY_1 + mix_sum_1*(dY_1_DY*sqrt(sum_1)**(-5) &
         - 5*dY_1*sqrt(sum_1)**(-6)*norm_1_DY ))

    Q_1a_DY = temp1 + temp2*temp3


    temp1 = 4*( mix_sum_1_DY*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DY )/sum_1**3
    temp1 = temp1*(-2*dX_1)*sqrt(sum_1)**(-3)
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(1_DP,time)
    temp3 = -2*(dX_1_DY*sqrt(sum_1)**(-3) - 3*dX_1*sqrt(sum_1)**(-4)*norm_1_DY)

    Q_1b_DY = temp1 + temp2*temp3


    temp1 = 4*( mix_sum_2_DY*sqrt(sum_2)**3 - 1.5*(mix_sum_2)*sqrt(sum_2)*sum_2_DY )/sum_2**3
    temp1 = temp1*( (dX_2 + LX_2)*sqrt(sum_2)**(-3) + 3*mix_sum_2*dY_2*sqrt(sum_2)**(-5) )
    temp2 = 4*mix_sum_2/sqrt(sum_2)**3 - C(2_DP,time)
    temp3 = (dX_2_DY + LX_2_DY)*sqrt(sum_2)**(-3) - 3*(dX_2 + LX_2)*sqrt(sum_2)**(-4)*norm_2_DY &
         + 3*(mix_sum_2_DY*sqrt(sum_2)**(-5)*dY_2 + mix_sum_2*(dY_2_DY*sqrt(sum_2)**(-5) &
         - 5*dY_2*sqrt(sum_2)**(-6)*norm_2_DY ))

    Q_2a_DY = temp1 + temp2*temp3

    ! JACOBIAN ENTRIES CORRESPONDING TO F_Nb, F_{Nb+1} WRT X DERIVATIVES
    P_0 = -(P_0_DX + P_1_DX)
    Q_0 = 8*Q_1a_DX

    DO i = 0,3
       jacobian(Nb+1, i) = -S1*P_0(i) - S2*hb*Q_0(i)
    ENDDO


    P_1 = -P_2_DX + P_0_DX
    Q_1 = 8*(Q_1b_DX + Q_2a_DX)
    DO i = 0,3
       jacobian(Nb+2, i) = -S1*P_1(i) - S2*hb*Q_1(i)
    ENDDO

    ! JACOBIAN ENTRIES CORRESPONDING TO F_Nb, F_Nbp1 WRT Y DERIVATIVES
    P_0 = -(P_0_DY + P_1_DY)
    Q_0 = 8*Q_1a_DY

    DO i = 0,3
       jacobian(Nb+1, i+Nb+1) = -S1*P_0(i) - S2*hb*Q_0(i)
    ENDDO


    P_1 = -P_2_DY + P_0_DY
    Q_1 = 8*(Q_1b_DY + Q_2a_DY)

    DO i = 0,3
       jacobian(Nb+2, i+Nb+1) = -S1*P_1(i) - S2*hb*Q_1(i)
    ENDDO




    !-------------------------------------------------------------------------------------------!
    !                                     RIGHT END POINT CASES                                 !
    !-------------------------------------------------------------------------------------------!
    ! 0 index corresponds to Nb   term
    ! 1 index corresponds to Nb-1 term
    ! 2 index corresponds to Nb-2 term

    dX_0_DX = (/ 0., 0., -1., 1., 0. /) ; dX_1_DX = (/ 0., -1., 0., 1., 0. /) ; dX_2_DX = (/ -1., 0., 1., 0., 0. /)
    dY_0_DY = (/ 0., 0., -1., 1., 0. /) ; dY_1_DY = (/ 0., -1., 0., 1., 0. /) ; dY_2_DY = (/ -1., 0., 1., 0., 0. /)
    dX_0_DY = 0. ; dX_1_DY = 0. ; dX_2_DY = 0.
    dY_0_DX = 0. ; dY_1_DX = 0. ; dY_2_DX = 0.


    LX_1_DX = (/ 0., 1., -2., 1., 0. /) ; LX_2_DX = (/ 1., -2., 1., 0., 0. /)
    LY_1_DY = (/ 0., 1., -2., 1., 0. /) ; LY_2_DY = (/ 1., -2., 1., 0., 0. /)
    LX_1_DY = 0. ; LX_2_DY = 0.
    LY_1_DX = 0. ; LY_2_DX = 0.


    dX_0 = bdry(Nb) - bdry(Nb-1)     ;    dY_0 = bdry(2*Nb+1) - bdry(2*Nb)
    dX_1 = bdry(Nb) - bdry(Nb-2)     ;    dY_1 = bdry(2*Nb+1) - bdry(2*Nb-1)
    dX_2 = bdry(Nb-1) - bdry(Nb-3)   ;    dY_2 = bdry(2*Nb)   - bdry(2*Nb-2)

    LX_1 = bdry(Nb) -2*bdry(Nb-1) + bdry(Nb-2)    ;    LY_1 = bdry(2*Nb+1) -2*bdry(2*Nb)   + bdry(2*Nb-1)
    LX_2 = bdry(Nb-1) -2*bdry(Nb-2) + bdry(Nb-3)  ;    LY_2 = bdry(2*Nb  ) -2*bdry(2*Nb-1) + bdry(2*Nb-2)

    sum_0 = dX_0**2 + dY_0**2   ;   sum_1 = dX_1**2 + dY_1**2   ;    sum_2 = dX_2**2 + dY_2**2
    mix_sum_1 = dX_1*LY_1 - LX_1*dY_1    ;     mix_sum_2 = dX_2*LY_2 - LX_2*dY_2

    ! DERIVATIVES OF THE SUM SQUARED
    sum_0_DX = 2*dX_0*dX_0_DX   ;       sum_0_DY = 2*dY_0*dY_0_DY
    sum_1_DX = 2*dX_1*dX_1_DX   ;       sum_1_DY = 2*dY_1*dY_1_DY
    sum_2_DX = 2*dX_2*dX_2_DX   ;       sum_2_DY = 2*dY_2*dY_2_DY

    ! MIXED SUM DERIVATIVES
    mix_sum_1_DX   = dX_1_DX*LY_1 - LX_1_DX*dY_1    ;   mix_sum_1_DY   = dX_1*LY_1_DY  - LX_1*dY_1_DY
    mix_sum_2_DX   = dX_2_DX*LY_2 - LX_2_DX*dY_2    ;   mix_sum_2_DY = dX_2*LY_2_DY - LX_2*dY_2_DY


    ! DERIVATIVES OF NORM
    norm_0_DX = sum_0**(-.5)*dX_0*dX_0_DX     ;    norm_0_DY = sum_0**(-.5)*dY_0*dY_0_DY
    norm_1_DX = sum_1**(-.5)*dX_1*dX_1_DX     ;    norm_1_DY = sum_1**(-.5)*dY_1*dY_1_DY
    norm_2_DX = sum_2**(-.5)*dX_2*dX_2_DX     ;    norm_2_DY = sum_2**(-.5)*dY_2*dY_2_DY

    !-------------------------------------------------------------------------------------------!
    !              NECESSARY QUANTITES FOR JACOBIAN CONTRIBUTIONS FROM F_{Nb-1}, F_{Nb-2}       !
    !-------------------------------------------------------------------------------------------!
    !  X - DERIVATIVES    
    P_0_DX = norm_0_DX*dX_0/(sqrt(sum_0)*hb) &
         + (sqrt(sum_0)/(hb)-1)*(dX_0_DX*sqrt(sum_0)-dX_0*norm_0_DX)/sum_0
    P_1_DX = norm_1_DX*dX_1/(sqrt(sum_1)*2*hb) &
         + (sqrt(sum_1)/(2*hb)-1)*(dX_1_DX*sqrt(sum_1)-dX_1*norm_1_DX)/sum_1
    P_2_DX = norm_2_DX*dX_2/(sqrt(sum_2)*2*hb) &
         + (sqrt(sum_2)/(2*hb)-1)*(dX_2_DX*sqrt(sum_2)-dX_2*norm_2_DX)/sum_2


    !Q_1a corresponds with Nb-2 term
    temp1 = 4*( mix_sum_1_DX*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DX )/sum_1**3
    temp1 = temp1*( (LY_1 - dY_1)*sqrt(sum_1)**(-3) - 3*mix_sum_1*dX_1*sqrt(sum_1)**(-5) )
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(Nb-1,time)
    temp3 = (LY_1_DX - dY_1_DX)*sqrt(sum_1)**(-3) - 3*(LY_1-dY_1)*sqrt(sum_1)**(-4)*norm_1_DX &
         - 3*(mix_sum_1_DX*sqrt(sum_1)**(-5)*dX_1 + mix_sum_1*(dX_1_DX*sqrt(sum_1)**(-5) &
         - 5*dX_1*sqrt(sum_1)**(-6)*norm_1_DX ))

    Q_1a_DX = temp1 + temp2*temp3

    !Q_1b corresponds with the centered version of Nb-2 term
    temp1 = 4*( mix_sum_1_DX*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DX )/sum_1**3
    temp1 = temp1*(2*dY_1)*sqrt(sum_1)**(-3)
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(Nb-1,time)
    temp3 = 2*(dY_1_DX*sqrt(sum_1)**(-3) - 3*dY_1*sqrt(sum_1)**(-4)*norm_1_DX)

    Q_1b_DX = temp1 + temp2*temp3


    !Q_2a corresponds with Nb-3 term
    temp1 = 4*( mix_sum_2_DX*sqrt(sum_2)**3 - 1.5*(mix_sum_2)*sqrt(sum_2)*sum_2_DX )/sum_2**3
    temp1 = temp1*( (LY_2 - dY_2)*sqrt(sum_2)**(-3) - 3*mix_sum_2*dX_2*sqrt(sum_2)**(-5) )
    temp2 = 4*mix_sum_2/sqrt(sum_2)**3 - C(Nb-2,time)
    temp3 = (-LY_2_DX - dY_2_DX)*sqrt(sum_2)**(-3) - 3*(LY_2-dY_2)*sqrt(sum_2)**(-4)*norm_2_DX &
         - 3*(mix_sum_2_DX*sqrt(sum_2)**(-5)*dX_2 + mix_sum_2*(dX_2_DX*sqrt(sum_2)**(-5) &
         - 5*dX_2*sqrt(sum_2)**(-6)*norm_2_DX ))

    Q_2a_DX = temp1 + temp2*temp3


    ! Y DERIVATIVES
    P_0_DY = norm_0_DY*dX_0/(sqrt(sum_0)*hb) &
         + (sqrt(sum_0)/(hb)-1)*(dX_0_DY*sqrt(sum_0)-dX_0*norm_0_DY)/sum_0
    P_1_DY = norm_1_DY*dX_1/(sqrt(sum_1)*2*hb) &
         + (sqrt(sum_1)/(2*hb)-1)*(dX_1_DY*sqrt(sum_1)-dX_1*norm_1_DY)/sum_1
    P_2_DY = norm_2_DY*dX_2/(sqrt(sum_2)*2*hb) &
         + (sqrt(sum_2)/(2*hb)-1)*(dX_2_DY*sqrt(sum_2)-dX_2*norm_2_DY)/sum_2


    !Q_1a corresponds with Nb-2 term
    temp1 = 4*( mix_sum_1_DY*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DY )/sum_1**3
    temp1 = temp1*( (LY_1 - dY_1)*sqrt(sum_1)**(-3) - 3*mix_sum_1*dX_1*sqrt(sum_1)**(-5) )
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(Nb-1,time)
    temp3 = (LY_1_DY - dY_1_DY)*sqrt(sum_1)**(-3) - 3*(LY_1-dY_1)*sqrt(sum_1)**(-4)*norm_1_DY &
         - 3*(mix_sum_1_DY*sqrt(sum_1)**(-5)*dX_1 + mix_sum_1*(dX_1_DY*sqrt(sum_1)**(-5) &
         - 5*dX_1*sqrt(sum_1)**(-6)*norm_1_DY ))

    Q_1a_DY = temp1 + temp2*temp3

    !Q_1b corresponds with the centered version of Nb-2 term
    temp1 = 4*( mix_sum_1_DY*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DY )/sum_1**3
    temp1 = temp1*(2*dY_1)*sqrt(sum_1)**(-3)
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(Nb-1,time)
    temp3 = 2*(dY_1_DY*sqrt(sum_1)**(-3) - 3*dY_1*sqrt(sum_1)**(-4)*norm_1_DY)

    Q_1b_DY = temp1 + temp2*temp3


    !Q_2a corresponds with Nb-3 term
    temp1 = 4*( mix_sum_2_DY*sqrt(sum_2)**3 - 1.5*(mix_sum_2)*sqrt(sum_2)*sum_2_DY )/sum_2**3
    temp1 = temp1*( (LY_2 - dY_2)*sqrt(sum_2)**(-3) - 3*mix_sum_2*dX_2*sqrt(sum_2)**(-5) )
    temp2 = 4*mix_sum_2/sqrt(sum_2)**3 - C(Nb-2,time)
    temp3 = (-LY_2_DY - dY_2_DY)*sqrt(sum_2)**(-3) - 3*(LY_2-dY_2)*sqrt(sum_2)**(-4)*norm_2_DY &
         - 3*(mix_sum_2_DY*sqrt(sum_2)**(-5)*dX_2 + mix_sum_2*(dX_2_DY*sqrt(sum_2)**(-5) &
         - 5*dX_2*sqrt(sum_2)**(-6)*norm_2_DY ))

    Q_2a_DY = temp1 + temp2*temp3

    ! JACOBIAN ENTRIES CORRESPONDING TO F_{Nb-1}, F_{Nb-2} WRT X DERIVATIVES
    P_0 = P_0_DX + P_1_DX
    Q_0 = 8*Q_1a_DX

    DO i = 0,3
       jacobian(Nb, Nb-3 + i) = -S1*P_0(i) - S2*hb*Q_0(i)
    ENDDO


    P_1 = P_2_DX - P_0_DX
    Q_1 = 8*(Q_1b_DX + Q_2a_DX)

    DO i = 0,3
       jacobian(Nb-1, Nb-3 + i) = -S1*P_1(i) - S2*hb*Q_1(i)
    ENDDO


    ! JACOBIAN ENTRIES CORRESPONDING TO F_{Nb-1}, F_{Nb-2} WRT Y DERIVATIVES
    P_0 = P_0_DY + P_1_DY
    Q_0 = 8*Q_1a_DY

    DO i = 0,3
       jacobian(Nb, 2*Nb-2 + i) = -S1*P_0(i) - S2*hb*Q_0(i)
    ENDDO


    P_1 = P_2_DY - P_0_DY
    Q_1 = 8*(Q_1b_DY + Q_2a_DY)

    DO i = 0,3
       jacobian(Nb-1, 2*Nb-2 + i) = -S1*P_1(i) - S2*hb*Q_1(i)
    ENDDO


    !-------------------------------------------------------------------------------------------!
    !            NECESSARY QUANTITES FOR JACOBIAN CONTRIBUTIONS FROM F_{2Nb-1}, F_{2Nb-2}       !
    !-------------------------------------------------------------------------------------------!
    ! 0 index corresponds to Nb-1 term
    ! 1 index corresponds to Nb-2 term
    ! 2 index corresponds to Nb-3 term


    !  X - DERIVATIVES    
    P_0_DX = norm_0_DX*dY_0/(sqrt(sum_0)*hb) &
         + (sqrt(sum_0)/(hb)-1)*(dY_0_DX*sqrt(sum_0)-dY_0*norm_0_DX)/sum_0
    P_1_DX = norm_1_DX*dY_1/(sqrt(sum_1)*2*hb) &
         + (sqrt(sum_1)/(2*hb)-1)*(dY_1_DX*sqrt(sum_1)-dY_1*norm_1_DX)/sum_1
    P_2_DX = norm_2_DX*dY_2/(sqrt(sum_2)*2*hb) &
         + (sqrt(sum_2)/(2*hb)-1)*(dY_2_DX*sqrt(sum_2)-dY_2*norm_2_DX)/sum_2


    !Q_1a corresponds with Nb-2 term
    temp1 = 4*( mix_sum_1_DX*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DX )/sum_1**3
    temp1 = temp1*( (dX_1 - LX_1)*sqrt(sum_1)**(-3) - 3*mix_sum_1*dY_1*sqrt(sum_1)**(-5) )
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(Nb-1,time)
    temp3 = (dX_1_DX - LX_1_DX)*sqrt(sum_1)**(-3) - 3*(dX_1-LX_1)*sqrt(sum_1)**(-4)*norm_1_DX &
         - 3*(mix_sum_1_DX*sqrt(sum_1)**(-5)*dY_1 + mix_sum_1*(dY_1_DX*sqrt(sum_1)**(-5) &
         - 5*dY_1*sqrt(sum_1)**(-6)*norm_1_DX ))

    Q_1a_DX = temp1 + temp2*temp3

    !Q_1b corresponds with the centered version of Nb-2 term
    temp1 = 4*( mix_sum_1_DX*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DX )/sum_1**3
    temp1 = temp1*(-2*dX_1)*sqrt(sum_1)**(-3)
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(Nb-1,time)
    temp3 = -2*(dX_1_DX*sqrt(sum_1)**(-3) - 3*dX_1*sqrt(sum_1)**(-4)*norm_1_DX)

    Q_1b_DX = temp1 + temp2*temp3


    !Q_2a corresponds with Nb-3 term
    temp1 = 4*( mix_sum_2_DX*sqrt(sum_2)**3 - 1.5*(mix_sum_2)*sqrt(sum_2)*sum_2_DX )/sum_2**3
    temp1 = temp1*( (dX_2 - LX_2)*sqrt(sum_2)**(-3) - 3*mix_sum_2*dY_2*sqrt(sum_2)**(-5) )
    temp2 = 4*mix_sum_2/sqrt(sum_2)**3 - C(Nb-2,time)
    temp3 = (dX_2_DX - LX_2_DX)*sqrt(sum_2)**(-3) - 3*(dX_2-LX_2)*sqrt(sum_2)**(-4)*norm_2_DX &
         - 3*(mix_sum_2_DX*sqrt(sum_2)**(-5)*dY_2 + mix_sum_2*(dY_2_DX*sqrt(sum_2)**(-5) &
         - 5*dY_2*sqrt(sum_2)**(-6)*norm_2_DX ))

    Q_2a_DX = temp1 + temp2*temp3


    ! Y DERIVATIVES
    P_0_DY = norm_0_DY*dY_0/(sqrt(sum_0)*hb) &
         + (sqrt(sum_0)/(hb)-1)*(dY_0_DY*sqrt(sum_0)-dY_0*norm_0_DY)/sum_0
    P_1_DY = norm_1_DY*dY_1/(sqrt(sum_1)*2*hb) &
         + (sqrt(sum_1)/(2*hb)-1)*(dY_1_DY*sqrt(sum_1)-dY_1*norm_1_DY)/sum_1
    P_2_DY = norm_2_DY*dY_2/(sqrt(sum_2)*2*hb) &
         + (sqrt(sum_2)/(2*hb)-1)*(dY_2_DY*sqrt(sum_2)-dY_2*norm_2_DY)/sum_2


    !Q_1a corresponds with Nb-2 term
    temp1 = 4*( mix_sum_1_DY*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DY )/sum_1**3
    temp1 = temp1*( (dX_1 - LX_1)*sqrt(sum_1)**(-3) - 3*mix_sum_1*dY_1*sqrt(sum_1)**(-5) )
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(Nb-1,time)
    temp3 = (dX_1_DY - LX_1_DY)*sqrt(sum_1)**(-3) - 3*(dX_1-LX_1)*sqrt(sum_1)**(-4)*norm_1_DY &
         - 3*(mix_sum_1_DY*sqrt(sum_1)**(-5)*dY_1 + mix_sum_1*(dY_1_DY*sqrt(sum_1)**(-5) &
         - 5*dY_1*sqrt(sum_1)**(-6)*norm_1_DY ))

    Q_1a_DY = temp1 + temp2*temp3

    !Q_1b corresponds with the centered version of Nb-2 term
    temp1 = 4*( mix_sum_1_DY*sqrt(sum_1)**3 - 1.5*(mix_sum_1)*sqrt(sum_1)*sum_1_DY )/sum_1**3
    temp1 = temp1*(-2*dX_1)*sqrt(sum_1)**(-3)
    temp2 = 4*mix_sum_1/sqrt(sum_1)**3 - C(Nb-1,time)
    temp3 = -2*(dX_1_DY*sqrt(sum_1)**(-3) - 3*dX_1*sqrt(sum_1)**(-4)*norm_1_DY)

    Q_1b_DY = temp1 + temp2*temp3


    !Q_2a corresponds with Nb-3 term
    temp1 = 4*( mix_sum_2_DY*sqrt(sum_2)**3 - 1.5*(mix_sum_2)*sqrt(sum_2)*sum_2_DY )/sum_2**3
    temp1 = temp1*( (dX_2 - LX_2)*sqrt(sum_2)**(-3) - 3*mix_sum_2*dY_2*sqrt(sum_2)**(-5) )
    temp2 = 4*mix_sum_2/sqrt(sum_2)**3 - C(Nb-2,time)
    temp3 = (dX_2_DY - LX_2_DY)*sqrt(sum_2)**(-3) - 3*(dX_2-LX_2)*sqrt(sum_2)**(-4)*norm_2_DY &
         - 3*(mix_sum_2_DY*sqrt(sum_2)**(-5)*dY_2 + mix_sum_2*(dY_2_DY*sqrt(sum_2)**(-5) &
         - 5*dY_2*sqrt(sum_2)**(-6)*norm_2_DY ))

    Q_2a_DY = temp1 + temp2*temp3


    ! JACOBIAN ENTRIES CORRESPONDING TO F_{Nb-1}, F_{Nb-2} WRT X DERIVATIVES
    P_0 = P_0_DX + P_1_DX
    Q_0 = 8*Q_1a_DX

    DO i = 0,3
       jacobian(2*Nb+1, Nb-3 + i) = -S1*P_0(i) - S2*hb*Q_0(i)
    ENDDO


    P_1 = P_2_DX - P_0_DX
    Q_1 = 8*(Q_1b_DX + Q_2a_DX)

    DO i = 0,3
       jacobian(2*Nb, Nb-3 + i) = -S1*P_1(i) - S2*hb*Q_1(i)
    ENDDO


    ! JACOBIAN ENTRIES CORRESPONDING TO F_{Nb-1}, F_{Nb-2} WRT Y DERIVATIVES
    P_0 = P_0_DY + P_1_DY
    Q_0 = 8*Q_1a_DY

    DO i = 0,3
       jacobian(2*Nb+1, 2*Nb-2 + i) = -S1*P_0(i) - S2*hb*Q_0(i)
    ENDDO


    P_1 = P_2_DY - P_0_DY
    Q_1 = 8*(Q_1b_DY + Q_2a_DY)

    DO i = 0,3
       jacobian(2*Nb, 2*Nb-2 + i) = -S1*P_1(i) - S2*hb*Q_1(i)
    ENDDO


  ENDSUBROUTINE compute_jacobian


  !----------------- INITIAL CONDITION ROUTINES -----------------!
  FUNCTION x0_quad(end_pt)
    REAL(DP) :: end_pt
    REAL(DP) :: x0_quad


    REAL(DP) :: x
    INTEGER(DP), PARAMETER :: N1 = 2**3
    REAL(DP) :: delta_x
    REAL(DP) :: XI0, XI1, XI2


    INTEGER(DP) :: index

    x0_quad = hb*.5*(x0_initial(end_pt) + x0_initial(end_pt - hb))
  ENDFUNCTION X0_quad
  


  FUNCTION y0_quad(end_pt)
    REAL(DP) :: end_pt
    REAL(DP) :: y0_quad


    REAL(DP) :: x
    INTEGER(DP), PARAMETER :: N1 = 2**3
    REAL(DP) :: delta_x
    REAL(DP) :: XI0, XI1, XI2


    INTEGER(DP) :: index

    y0_quad = hb*.5*(y0_initial(end_pt) + y0_initial(end_pt - hb))
  ENDFUNCTION Y0_quad
  
  
  
  FUNCTION x0_initial(s_value)
    REAL(DP) :: s_value
    REAL(DP) :: x0_initial
    
    REAL(DP) :: temp_theta
    
    temp_theta = b*(s_value - 1.)*kpa*cos(kpa*s_value) &
         - b*sin(kpa*s_value)
    x0_initial = cos(temp_theta)
  ENDFUNCTION x0_initial


  FUNCTION y0_initial(s_value)
    REAL(DP) :: s_value
    REAL(DP) :: y0_initial
    
    REAL(DP) :: temp_theta
    
    temp_theta = b*(s_value - 1.)*kpa*cos(kpa*s_value) &
         - b*sin(kpa*s_value)
    y0_initial = sin(temp_theta)
  ENDFUNCTION y0_initial

  FUNCTION bi_int(i,j,p1,p2,A) 
    !bilinear interpolation
    REAL(DP) :: bi_int,p1,p2
    INTEGER(DP) :: i,j
    REAL(DP), DIMENSION(0:N-1, 0:N-1) :: A

    REAL(DP) :: x1,x2, y1,y2


    x1 = x0 + i*h
    x2 = x0 + (i+1)*h
    y1 = y0 + j*h
    y2 = y0 + (j+1)*h

    bi_int = ( A(i,j)*(x2-p1)*(y2-p2)  &
         + A(mod(i+1,N),j)*(p1-x1)*(y2-p2) &
         + A(i,mod(j+1,N))*(x2-p1)*(p2-y1) &
         + A(mod(i+1,N),mod(j+1,N))*(p1-x1)*(p2-y1))/h**2
  ENDFUNCTION bi_int

ENDMODULE subs
