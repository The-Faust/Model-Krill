PROGRAM test_prog2
use class_krill
use listKrill_mod
use ran_mod

implicit none 

type(listKrill) :: list
type(Krill) :: k
integer :: i


real, parameter :: T = 12.0     ! TEMPORARY
real, parameter :: PHYTO = 0.5  ! TEMPORARY
real, parameter :: ZOO = 0.5    ! TEMPORARY


do i=1,10
        

        call k%init_krill(spread(20.0,45.0), 1, 1)
        call list%addKrill(k)
end do

 call list%printList()

END PROGRAM test_prog2


