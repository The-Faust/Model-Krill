! utility_krill_mod.f90

module utility_krill_mod

use class_krill
use listKrill_mod
use ran_mod

contains
	subroutine evolveList(listOfkrill)
		class(listKrill) :: listOfkrill
		class(*), pointer :: curr
		
		call listOfkrill%reset()

		do while(listOfkrill%moreValues())			
			select type(curr => listOfkrill%currentValue())
			class is (Krill)
				call curr%grow()
				call curr%molt()
			end select

			call listOfkrill%next()		
		end do
		
!		call listOfkrill%printList()
	end subroutine evolveList

	subroutine generateKrill(n, T, phyto, zoo)
		class(listKrill) :: list
		class(Krill) :: akrill

		integer, intent(in) :: n
		real, intent(in) :: T
		real, intent(in) :: phyto
		real, intent(in) :: zoo

		do i = 1, n
			call akrill%init_krill(spread(20.0,45.0), ran1(), ran1())
			list%addKrill(akrill)
		end do
end module utility_krill_mod
