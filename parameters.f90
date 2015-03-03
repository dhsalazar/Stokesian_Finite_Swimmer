MODULE parameters
  USE HTYPE
  
  IMPLICIT NONE
  
  
  REAL(DP), PARAMETER :: s0 = 0.0
  REAL(DP), PARAMETER :: sf = 0.6
  
  REAL(DP), PARAMETER :: x0 = 0.0
  REAL(DP), PARAMETER :: xf = 4.0

  REAL(DP), PARAMETER :: y0 = 0.0
  REAL(DP), PARAMETER :: yf = xf!TWOPI_D/kpa


  INTEGER(DP), PARAMETER :: N = 2**9
  INTEGER(DP), PARAMETER :: Nb = N*(sf-s0)/(.5*(xf-x0))


  REAL(DP), PARAMETER :: h = (xf - x0)/REAL(N)
  REAL(DP), PARAMETER :: hb = (sf - s0)/REAL(Nb)
  REAL(DP), PARAMETER :: dt  = .3*hb

  !FFTW SPECIFIC
  INTEGER(DP), PARAMETER :: Nc = N/2 + 1
  INTEGER, PARAMETER :: fftw_estimate = 64
  
  !FLUID PARAMETERS
  REAL(DP), PARAMETER :: De = .1
  REAL(DP), PARAMETER :: alpha = dt
  REAL(DP), PARAMETER :: beta = 1.
    
  !FENE PARAMETERS
  REAL(DP), PARAMETER :: b = .1
  REAL(DP), PARAMETER :: w = TWOPI_D
  REAL(DP), PARAMETER :: kpa = 2*PI_D/sf


  !STIFFNESS CONSTANTS
  REAL(DP), PARAMETER :: S1 = 10.**7
  REAL(DP), PARAMETER :: S2 = 10.**4
  REAL(DP), PARAMETER :: diff_coeff = 0*h
  !ROUTINELY USED MATRICES.
  COMPLEX(DP), DIMENSION(0:N-1, 0:N-1):: p_coeff
  COMPLEX(DP), DIMENSION(0:N-1, 0:N-1):: g_coeff
  
  
ENDMODULE parameters
