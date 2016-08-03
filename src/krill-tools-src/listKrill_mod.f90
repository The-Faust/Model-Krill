!! listKrill_mod.f90

module listKrill_mod
	use class_krill
	use abstList_mod

	private
	public :: listKrill
	type, extends(list) :: listKrill 
		contains
			! functions
			procedure :: n_krill ! count number of krills in list
			procedure, public :: current => currentKrill ! get krill pointed by iterator

			! subroutines
			procedure, public :: addKrill ! add krill in list
			procedure, public :: printList => printListKrill ! print the attributes of all krills in list
			procedure, public :: thisKrill ! allow acces to a specific krill 
			procedure, public :: netcdfKrill ! generate a netcdf file for list of krills "much fun! such Krills!"
	end type listKrill

contains
	integer function n_krill(this)
		class(listKrill) :: this
		integer :: n

		n = 0
		
		call this%reset()
		do while(this%moreValues())
			n = n + 1
			call this%next()
		end do

		n_krill = n
	end function n_krill

	subroutine thisKrill(this, krillPosition)
		class(listKrill) :: this
		integer :: krillPosition

		call this%reset()
		do i =  1, (krillPosition - 1)
			call this%next()
		end do
	end subroutine thisKrill
	
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
