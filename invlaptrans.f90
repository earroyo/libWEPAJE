! Great resource fo Inverse Laplance Transformations at
! http://www.cs.hs-rm.de/~weber/
! Web Page of Prof. Dr. Helmut Weber
! Source: http://www.cs.hs-rm.de/~weber/lapinv/dehoog/f90/
! this module implements the F.R. de Hoog, J.H. Knight, and A.N. Stokes
! numerical inverse Laplace transform algorithm.
! see "An improved method for numerical inversion of Laplace
!     transforms", SIAM J. Sci. Stat. Comp., 3, 357-366, 1982.

!%  IF YOU PUBLISH WORK BENEFITING FROM THIS M-FILE, PLEASE CITE IT AS:
!%    Hollenbeck, K. J. (1998) INVLAP.M: A matlab function for numerical 
!%    inversion of Laplace transforms by the de Hoog algorithm, 
!%    http://www.isva.dtu.dk/staff/karl/invlap.htm 

module invlaptrans

IMPLICIT NONE
! same selected_real_kind as in calling program
INTEGER, PARAMETER, private  :: dp = SELECTED_REAL_KIND(15, 307)
INTEGER, PARAMETER, public   ::  M__ = 20
!=======================================================================
! Read the Stehfest Parameter n (6 < n < 20; only even number)
! Performance of Horizontal Well, by Erdal Ozkan
! PhD Thesis University of Tulsa Page 42
! From Thesis: "In our experience, as a starting guess, N can be
! choosen as 8 for inversion of infinite acting system
! solution and 16 for inversion of bounded system solution"
INTEGER, PARAMETER, public   :: GAVER_STEHFEST_N = 16

  PRIVATE
  PUBLIC  ::  invlap, m_invlap, m_invlapGS, m_invlapGSMultOut, lsep, lsepGS

CONTAINS

! Let's find the Laplance Space Evaluation Points
!-------------------------------------------------------------------------------------------
    function lsep(tmax, M, alpha, tol) result(s)
!-------------------------------------------------------------------------------------------
! Laplance Space Evaluation Points
        real(dp), intent(in) :: tmax
        integer,  intent(in)          :: M
        real(dp), intent(in),optional :: alpha
        real(dp), intent(in),optional :: tol
        complex(dp), dimension(0:2*M) :: s
        
        real(dp), dimension(0:2*M) :: run
        real(dp) :: gamma, pi, bigT
        integer  :: i
        real(dp) :: alpha_,tol_
        
        ! Let's make sure default values are captured 
        if(present(alpha))then
           alpha_=alpha
        else
           alpha_=0
        end if
        
        if(present(tol))then
           tol_=tol
        else
           tol_=1.d-12
        end if
        
        ! write(*,*) 'Optional Arguments alpha, tol :', alpha_, tol_
        ! Allocate Memory for run
        ! allocate(run(0:2*M))
        do i=0,2*M
           run(i) = float(i)
        end do

        pi = 3.1415926535897931d0
        bigT = 2.d0*tmax
        gamma = alpha_-log(tol_)/(2.d0*bigT)
        do i=0,2*M
          s(i) = gamma + cmplx(0.d0,1.d0) * pi * run(i) / bigT
        end do
        ! write (*,*) s
        ! deallocate(run)
        return
    end function lsep

! Let use similar sintaxis as Matlab
!-------------------------------------------------------------------------------------------
    subroutine m_invlap(F, ft, t, Nt, alpha, tol, M_opt) 
!   function m_invlap(F, t, Nt, alpha, tol, M_opt) result (ft)    
!-------------------------------------------------------------------------------------------
! % INVLAP  numerical inverse Laplace transform
! %
! % f = invlap(F, t, alpha, tol, P1,P2,P3,P4,P5,P6,P7,P8,P9);
! %         
! % F       laplace-space function (string refering to an m-file), 
! %           must have form F(s, P1,..,P9), where s is the Laplace parameter,
! %           and return column vector as result
! % t       column vector of times for which real-space function values are
! %           sought
! % alpha   largest pole of F (default zero)
! % tol     numerical tolerance of approaching pole (default 1e-9)
! % P1-P9   optional parameters to be passed on to F
! % f       vector of real-space values f(t)
!
! Added by Elkin Arroyo-Negrete
!
        complex(dp),intent(in),dimension(:) :: F   ! Evaluate your function before passing it
        real(dp), intent(out) ,dimension(Nt) :: ft ! Laplance Transformed values of F at t        
        real(dp), intent(in) , dimension(Nt) :: t  ! vector of times
        integer , intent(in) :: Nt
        real(dp), intent(in) , optional :: alpha
        real(dp), intent(in) , optional :: tol
        integer,  intent(in) , optional:: M_opt        
        

!       real(dp), optional, intent(in) :: P1,P2,P3,P4,P5,P6,P7,P8,P9
        real(dp) :: tmin, tmax, bigT, gamma
        integer  :: M
        real(dp) :: alpha_,tol_
        
        real(dp), dimension(1) :: ft_,t_ 
        integer  :: Nt_
         
        ! Let's make sure default values are captured 
        if(present(alpha))then
           alpha_=alpha
        else
           alpha_=0
        end if
        
        if(present(tol))then
           tol_=tol
        else
           tol_=1.d-12
        end if
        
        if(present(M_opt))then
           M =M_opt
        else
           M = M__
        end if

        ! Nt = size(t)

        tmax  = maxval(t)
        tmin  = minval(t)
        bigT = 2.d0*tmax
        gamma = alpha_-log(tol_)/(2.d0*bigT)
        call invlap(ft, t, tmin, tmax, F, M, gamma, Nt)
        
    end subroutine m_invlap
!   end function m_invlap
  
    !------------------------------------------------------------------------
    subroutine invlap(ft, t, tmin, tmax, fp, M, gamma, Nt ) 
    !------------------------------------------------------------------------
        real(dp), intent(out), dimension(Nt) :: ft
        real(dp), intent(in), dimension(Nt) :: t   ! vector of times
        real(dp), intent(in) :: tmin, tmax
        complex(dp), intent(in), dimension(0:2*M) :: fp
        integer,  intent(in) :: M, Nt
        real(dp), intent(in) :: gamma

        complex(dp), dimension(0:2*M,0:M) :: e
        complex(dp), dimension(0:2*M-1,0:M) :: q  ! column 0 is not used
        complex(dp), dimension(0:2*M) :: d
        complex(dp), dimension(0:2*M+1,Nt) :: A,B
        complex(dp), dimension(Nt) :: z,h2M,R2Mz
        integer :: r, rq, n
        real(dp) :: pi, bigt
             
        ! pi = 3.141592653589793238462643383276
        pi = 3.141592653589793238462643383276d0
        ! pi =   3.1415926535897931d0
        bigt = 2.d0 * tmax

        !a[0] = a[0] / 2.0  # zero term is halved
    
        ! build up e and q tables. superscript is now row index, subscript column
        e(:,:) = cmplx(0.d0,0.d0,dp)
        q(:,:) = cmplx(0.d0,0.d0,dp)
        q(0,1) = fp(1)/(fp(0)/2.0) ! half first term
        q(1:2*M-1,1) = fp(2:2*M) / fp(1:2*M-1)
        !q(:,1) = fp(1:2*M) / fp(0:2*M-1)

        do r = 1,M  ! step through columns
            e(0:2*(M-r+1)-2,r) = q(1:2*(M-r+1)-1,r) - q(0:2*(M-r+1)-2,r) + e(1:2*(M-r+1)-1,r-1)
            if (r < M) then  ! one column fewer for q
                rq = r+1
                q(0:2*(M-rq+1)-1,rq) = q(1:2*(M-rq+1),rq-1) * e(1:2*(M-rq+1),rq-1) / e(0:2*(M-rq+1)-1,rq-1)
            endif
        end do
    
        ! build up d vector (index shift: 1)
        d(:) = cmplx(0.d0,0.d0,dp)
        d(0) = fp(0)/2.0 ! half first term
        d(1:2*M-1:2) = -q(0,1:M) ! these 2 lines changed after niclas
        d(2:2*M:2) = -e(0,1:M) 
    
        ! build A and B vectors (Hollenbeck claims an index shift of 2, but that may be related to the matlab code)
        ! now make into matrices, one row for each time
        A(:,:) = cmplx(0.d0,0.d0,dp)
        B(:,:) = cmplx(0.d0,0.d0,dp)
        A(1,:) = d(0)
        B(0:1,:) = cmplx(1.d0,0.d0,dp)
    
        z = exp( cmplx(0.d0,1.d0,dp) * pi * t / bigt )
        ! after niclas back to the paper (not: z = exp(-i*pi*t/T))
        do n = 2, 2*M+1
            A(n,:) = A(n-1,:) + d(n-1) * z * A(n-2,:)  ! different index 
            B(n,:) = B(n-1,:) + d(n-1) * z * B(n-2,:)  ! shift for d!
        end do
        
        ! double acceleration
        h2M = 0.5d0 * ( 1.d0 + ( d(2*M-1) -d(2*M) ) * z )
        R2Mz = -h2M * ( 1.d0 - sqrt( 1.d0 + d(2*M) * z / h2M**2 ) )
    
        A(2*M+1,:) = A(2*M,:) + R2Mz * A(2*M-1,:)
        B(2*M+1,:) = B(2*M,:) + R2Mz * B(2*M-1,:)
        
        ! inversion
        ft = ( 1.d0/bigt * exp(gamma*t) * real( A(2*M+1,:) / B(2*M+1,:) ) )        
        
    end subroutine invlap
    
!--------------------------------------------------------
    RECURSIVE FUNCTION factorial(n) RESULT(res)
!--------------------------------------------------------
        INTEGER*4   n
        INTEGER*8 res
        IF(n.EQ.0) THEN
           res = 1
        ELSE
           res = n*factorial(n-1)
        END IF
    END FUNCTION factorial

! Let's find the Laplance Space Evaluation Points
!-------------------------------------------------------------------------------------------
SUBROUTINE lsepGS(s, t, n) ! result(s)
!-------------------------------------------------------------------------------------------
! Laplance Space Evaluation Points for the STEHFEST algorithm 
! ROOM FOR IMPROVEMENT - ROOM FOR IMPROVEMENT
! Ealuate vector log(2)*I and set it as global variable
! same for v(I) = V_INIT * VSUM;
    real(dp),  intent(in)                :: t
    integer*4, intent(in)                :: n
    real(dp) , dimension(n), intent(out) :: s
    integer*4                :: I

    real(dp)                 :: a

    ! Performance of Horizontal Well, by Erdal Ozkan
    ! PhD Thesis University of Tulsa Page 42
    ! In our experience, as a starting guess, N can be
    ! choosen as 8 for inversion of infinite acting system
    ! solution and 16 for inversion of bounded system solution
    !if(present(n_))then
    !  if(n_<10) then
    !    n = 10
    !  end if
    !else
    !  n = 16
    !end if
    
    a = log(2.d0) / t;
    !for I = 1 : n
    !write(*,*) 'Inside lsepGS n=',n
    DO I=1,n
        s(I) = a * I;
        !write(*,*) 'log(2)*I',log(2.d0)*I
        !write(*,*) 's=',s(I)
    END DO
    ! return
END SUBROUTINE lsepGS

! Inverse Laplace using GAVER-STEHFEST
subroutine m_invlapGS(ft,F,t,n)
! function f = invlapGS(F,t,n,P1,P2,P3,P4,P5,P6,P7,P8,P9);
!%'=======================================================================
!%'     Function:  Stehfet - COMPUTES GAVER-STEHFEST INVERSION ALGORITHM
!%'     Principle Author:  THOMAS A. BLASINGAME -- TEXAS A&M UNIVERSITY
!%'     Converted to Visual Basic by: M. Wael Helmy, July 1997
!%'     Converted to Matlab       by: Elkin Arroyo-Negrete, Sep 2012
!%'     Converted to Fortran      by: Elkin Arroyo-Negrete, Nov 11 2012
!%'
!%'     The Gaver-Stehfest Algorithm given as:
!%'
!%'                               n
!%'                f(n,td,a) = a SUM  V(i)  * F(a*i)
!%'                              i=1
!%'     where
!%'                a = ln 2 / td
!%'
!%'     and, the V(i)'s are given as:
!%'
!%'                         Min(i,n/2)          k^(n/2) * (2k)!
!%'   V(i) = (-1)^(n/2+i) *    SUM     -----------------------------------
!%'                         k=(i+1)/2   (n/2-k)! k! (k-1)! (i-k)! (2k-i)!
!%'
!%'  The details are given in the STEHFEST paper : " Algortihm
!%'  368: Numerical Inversion of Laplace Transforms ", Communi-
!%'  cations of the ACM, Volume 13, Number 1, January 1970.
!%'  Correction for V(i) given in Vol 13, Num 10, October 1970.
!%'
!%'  Definition of Variables:
!%'
!%'     n = Number of terms in series (always even, 6<n<20)
!%'     td = Time of evaluation (dimensionless for simplicity)
!%'     a = Modal value of Gaver distribution function
!%'     a*i = "s" for evaluating the Laplace space function F(s)
!%'     F(s) = F(a*i) = Laplace space function
!%'     v(i) = Stehfest extrapolation coefficients
!%'     f(n,td,a) = Approximate inversion function for F(s)
!%'                (here it is used as the const rate solution, pwd)
!%'     pwd = const rate solution
!%'     pd_dtd = pressure derivative w.r.t time
!%'     pd_dlntd = pressure derivative w.r.t natural logarithm of time
!%'     qwd = const pressure solution
!%'     Cumd = dimensionless cum. prod.
!%'
!%'     In Laplace Space: L(qwd) = 1/(L(pwd)*s*s)
!%'                       L(Cumd) = 1/(L(pwd)*s*s*s)
!%'
!%'=======================================================================

!%Dim pwd As Double, dp_dlntd As Double, dp_dtd As Double, qwd As Double
!%Dim rd As Double, Cumd As Double
!%Dim V_INIT As Double, VSUM As Double, TOP As Double, BOTTOM As Double
!%Dim T1 As Double, T2 As Double
!%Dim B1 As Double, B2 As Double, B3 As Double, B4 As Double, B5 As Double
!%Dim a As Double, s As Double, FS As Double
!%Dim K_START As Long, K_END As Long
!%Dim n As Long, I As Long, k As Long
!%Dim v(20) As Double

real(dp) , intent(in) ,dimension(20) :: F   ! User needs to evaluate your function before passing it
real(dp) , intent(out) :: ft                ! Laplace Transformed values of F at t        
real(dp) , intent(in)  :: t                 ! evaluation times
integer*4, intent(in)  :: n

real(dp) :: pwd, dp_dlntd, dp_dtd, qwd, rd, Cumd
real(dp) :: V_INIT, VSUM, TOP, BOTTOM
real(dp) :: T1, T2, FS
real(dp) :: B1, B2, B3, B4, B5
real(dp) :: a, s
real(dp) , dimension(20) :: v 
integer*4:: k, I
integer*4:: K_START, K_END

!%'=======================================================================
!%'Read the Stehfest Parameter n (6 < n < 20; only even number)
!%'and calculate the v(i)'s and store in the Array v(i)
!%'=======================================================================


! write(*,*) 'Inside m_invlapGS'
! for I = 1 : n,
DO I=1, n
    V_INIT = (-1.d0) ** (n / 2 + I);
    K_START = floor((I + 1) / 2.d0);
    K_END = min(I, n / 2);
    VSUM = 0.d0;
    ! for k = K_START : K_END
    DO k = K_START, K_END
        T1 = k ** (n / 2);
        T2 = factorial(2 * k);
        TOP = T1 * T2;
        B1 = factorial(n / 2 - k);
        B2 = factorial(k);
        B3 = factorial(k - 1);
        B4 = factorial(I - k);
        B5 = factorial(2 * k - I);
        BOTTOM = B1 * B2 * B3 * B4 * B5;
        VSUM = VSUM + TOP / BOTTOM;
    END DO
    v(I) = V_INIT * VSUM;
    !write(*,*) 'Evaluating v(I)=',v(I), ' I=',I, ' n=',n
END DO

!%'=======================================================================
!%'Initialization
!%'=======================================================================
a = log(2.d0) / t;
qwd = 0.d0;

!%'=======================================================================
!%'LOOP FOR STEHFEST EXTRAPOLATION FORMULA
!%'=======================================================================
      
! for I = 1 : n
DO I=1,n
    ! s = a * I;
    !%'===================================================================
    !%'Calls the Laplace space solution function FS = F(s)
    !%'===================================================================
    !% find F argument, call F with it, get 'a'  
    !command = ['FS = ' F '(s'];
    !if nargin > 3,  			% pass on parameters
    !  for iarg = 1:nargin-3,
	  !  command = [command ',P' int2str(iarg)];
    !  end
    !end
    !command = [command ');'];
    !eval(command);
    
    !%'===================================================================
    !%'Calculates the real time solution (inversion of Laplace solution)
    !%'===================================================================
    qwd = qwd + v(I) * F(I);
    ! write(*,*) 'F(I)=',F(I);
END DO

!%'=======================================================================
!%'Final results
!%'=======================================================================      
qwd = a * qwd;
ft = qwd;
end subroutine m_invlapGS

!=======================================================================
! Inverse Laplace using GAVER-STEHFEST
subroutine m_invlapGSMultOut(ft,dft,F,dF,t,n)
! function f = invlapGS(F,t,n,P1,P2,P3,P4,P5,P6,P7,P8,P9);
!%'=======================================================================
!%'     Function:  Stehfet - COMPUTES GAVER-STEHFEST INVERSION ALGORITHM
!%'     Principle Author:  THOMAS A. BLASINGAME -- TEXAS A&M UNIVERSITY
!%'     Converted to Visual Basic by: M. Wael Helmy, July 1997
!%'     Converted to Matlab       by: Elkin Arroyo-Negrete, Sep 2012
!%'     Converted to Fortran      by: Elkin Arroyo-Negrete, Nov 11 2012
!%'
!%'     The Gaver-Stehfest Algorithm given as:
!%'
!%'                               n
!%'                f(n,td,a) = a SUM  V(i)  * F(a*i)
!%'                              i=1
!%'     where
!%'                a = ln 2 / td
!%'
!%'     and, the V(i)'s are given as:
!%'
!%'                         Min(i,n/2)          k^(n/2) * (2k)!
!%'   V(i) = (-1)^(n/2+i) *    SUM     -----------------------------------
!%'                         k=(i+1)/2   (n/2-k)! k! (k-1)! (i-k)! (2k-i)!
!%'
!%'  The details are given in the STEHFEST paper : " Algortihm
!%'  368: Numerical Inversion of Laplace Transforms ", Communi-
!%'  cations of the ACM, Volume 13, Number 1, January 1970.
!%'  Correction for V(i) given in Vol 13, Num 10, October 1970.
!%'
!%'  Definition of Variables:
!%'
!%'     n = Number of terms in series (always even, 6<n<20)
!%'     td = Time of evaluation (dimensionless for simplicity)
!%'     a = Modal value of Gaver distribution function
!%'     a*i = "s" for evaluating the Laplace space function F(s)
!%'     F(s) = F(a*i) = Laplace space function
!%'     v(i) = Stehfest extrapolation coefficients
!%'     f(n,td,a) = Approximate inversion function for F(s)
!%'                (here it is used as the const rate solution, pwd)
!%'     pwd = const rate solution
!%'     pd_dtd = pressure derivative w.r.t time
!%'     pd_dlntd = pressure derivative w.r.t natural logarithm of time
!%'     qwd = const pressure solution
!%'     Cumd = dimensionless cum. prod.
!%'
!%'     In Laplace Space: L(qwd) = 1/(L(pwd)*s*s)
!%'                       L(Cumd) = 1/(L(pwd)*s*s*s)
!%'
!%'=======================================================================

!%Dim pwd As Double, dp_dlntd As Double, dp_dtd As Double, qwd As Double
!%Dim rd As Double, Cumd As Double
!%Dim V_INIT As Double, VSUM As Double, TOP As Double, BOTTOM As Double
!%Dim T1 As Double, T2 As Double
!%Dim B1 As Double, B2 As Double, B3 As Double, B4 As Double, B5 As Double
!%Dim a As Double, s As Double, FS As Double
!%Dim K_START As Long, K_END As Long
!%Dim n As Long, I As Long, k As Long
!%Dim v(20) As Double

real(dp) , intent(in) ,dimension(20) :: F   ! User needs to evaluate your function before passing it
real(dp) , intent(in) ,dimension(20) :: dF  ! User needs to evaluate your function before passing it
real(dp) , intent(out) :: ft                ! Laplace Transformed values of F at t 
real(dp) , intent(out) :: dft               ! Laplace Transformed values of F at t       
real(dp) , intent(in)  :: t                 ! evaluation times
integer*4, intent(in)  :: n

real(dp) :: pwd, dp_dlntd, dp_dtd, dqwd, qwd, rd, Cumd
real(dp) :: V_INIT, VSUM, TOP, BOTTOM
real(dp) :: T1, T2, FS
real(dp) :: B1, B2, B3, B4, B5
real(dp) :: a, s
real(dp) , dimension(20) :: v 
integer*4:: k, I
integer*4:: K_START, K_END

!%'=======================================================================
!%'Read the Stehfest Parameter n (6 < n < 20; only even number)
!%'and calculate the v(i)'s and store in the Array v(i)
!%'=======================================================================


! write(*,*) 'Inside m_invlapGS'
! for I = 1 : n,
DO I=1, n
    V_INIT = (-1.d0) ** (n / 2 + I);
    K_START = floor((I + 1) / 2.d0);
    K_END = min(I, n / 2);
    VSUM = 0.d0;
    ! for k = K_START : K_END
    DO k = K_START, K_END
        T1 = k ** (n / 2);
        T2 = factorial(2 * k);
        TOP = T1 * T2;
        B1 = factorial(n / 2 - k);
        B2 = factorial(k);
        B3 = factorial(k - 1);
        B4 = factorial(I - k);
        B5 = factorial(2 * k - I);
        BOTTOM = B1 * B2 * B3 * B4 * B5;
        VSUM = VSUM + TOP / BOTTOM;
    END DO
    v(I) = V_INIT * VSUM;
    !write(*,*) 'Evaluating v(I)=',v(I), ' I=',I, ' n=',n
END DO

!%'=======================================================================
!%'Initialization
!%'=======================================================================
a = log(2.d0) / t;
qwd = 0.d0;

!%'=======================================================================
!%'LOOP FOR STEHFEST EXTRAPOLATION FORMULA
!%'=======================================================================
      
! for I = 1 : n
DO I=1,n
    ! s = a * I;
    !%'===================================================================
    !%'Calls the Laplace space solution function FS = F(s)
    !%'===================================================================
    !% find F argument, call F with it, get 'a'  
    !command = ['FS = ' F '(s'];
    !if nargin > 3,  			% pass on parameters
    !  for iarg = 1:nargin-3,
	  !  command = [command ',P' int2str(iarg)];
    !  end
    !end
    !command = [command ');'];
    !eval(command);
    
    !%'===================================================================
    !%'Calculates the real time solution (inversion of Laplace solution)
    !%'===================================================================
    qwd = qwd + v(I) * F(I);
    dqwd= dqwd+ v(I) * dF(I); 
    ! write(*,*) 'F(I)=',F(I);
END DO

!%'=======================================================================
!%'Final results
!%'=======================================================================      
qwd = a * qwd;
dqwd= a * dqwd;
ft = qwd;
dft=dqwd;
end subroutine m_invlapGSMultOut


end module invlaptrans
