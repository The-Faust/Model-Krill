! utility_krill_mod.f90

module utility_krill_mod

use class_krill
use listKrill_mod

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
		
		call listOfkrill%printList()
	end subroutine evolveList
end module utility_krill_mod
