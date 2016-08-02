module ran_mod

  ! Module contains three functions:
  !
  ! ran1() returns a uniform random number between 0 - 1
  ! spread(min,max) returns random number between min - max
  ! normal(mean,sd) returns a normal deviate
  !
  ! from: http://www.sdsc.edu/~tkaiser/f90.html
  !
  ! MapsF 2011

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real function ran1()  ! Returns random number between 0 - 1

    implicit none

    real :: x

    call random_number(x) ! Built in fortran 90 random number function

    ran1 = x

  end function ran1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real function spread(min,max)  ! Returns random number between min - max

    implicit none

    real,intent(in) :: min,max


    spread = (max - min) * ran1() + min

  end function spread

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real function normal(mean,sd) ! Returns a normal distribution
    ! mean = mean
    ! sd   = standard deviation (square-root of variance)

    implicit none

    real,intent(in) :: mean,sd

    real :: fac,rsq,r1,r2,tmp

    real, save :: gsave

    logical, save :: flag = .false.


    if (flag) then
       ! Extra deviate handy, so return it and unset the flag

       tmp = gsave

       flag = .false.

    else
       ! No extra deviate handy
       ! so pick two uniform numbers from -1 to 1
       ! and check is they are in the unit circle

       rsq = 2.

       do while(rsq.ge.1..or.rsq.eq.0.)

          r1 = 2.*ran1()-1.

          r2 = 2.*ran1()-1.

          rsq = r1*r1+r2*r2

       enddo

       ! Box-Muller transform to get two normal deviates
       fac = sqrt(-2.*log(rsq)/rsq)

       ! Return one and save one for next time
       tmp = r2*fac

       gsave = r1*fac

       flag = .true.

    endif

    normal = tmp*sd+mean

    return

  end function normal

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module ran_mod
