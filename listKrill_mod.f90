!! listKrill_mod.f90

module listKrill_mod
	use class_krill
	use abstList_mod

	private
	public :: listKrill
	type, extends(list) :: listKrill
		contains
			procedure, public :: addKrill ! add krill in list
			procedure, public :: current => currentKrill ! get krill pointed by iterator
			procedure, public :: printList => printListKrill ! print the attributes of all krills in list
	end type listKrill

contains 

	subroutine printListKrill(this)
		class(listKrill) :: this
		class(*), pointer :: curr

		call this%reset()

		do while(this%moreValues())
			curr => this%currentValue()

			select type(curr)
			type is (Krill)
				call curr%debug()
			end select
			
			call this%next()
		end do
		
		call this%reset()
	end subroutine printListKrill

	subroutine addKrill(this, value)
		class(listKrill) :: this
		class(Krill) :: value
		class(*), allocatable :: v

		allocate(v, source = value)
		call this%addValue(v)
	end subroutine addKrill

	function currentKrill(this)
		type(Krill) :: currentKrill
		class(listKrill) :: this
		class(*), pointer :: v

		v => this%currentValue()
		select type(v)
		type is (Krill)
			currentKrill = v
		end select
	end function currentKrill
end module listKrill_mod
