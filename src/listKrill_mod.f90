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
			procedure, public :: matrixKrill ! return a matrix of all the attributes of all the krills 
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

	function matrixKrill(this)
		class(listKrill) :: this
		class(*), pointer :: curr

		real, dimension(:,:), allocatable :: matrixKrill		
		real, dimension(:), allocatable :: buff_matrix
		integer :: krillNumber 
		
		call this%reset()
		krillNumber = 0

		do while(this%moreValues())
			curr => this%currentValue()
			krillNumber = krillNumber + 1
			
			select type(curr)
			type is (Krill)
				allocate(buff_matrix(18 * krillNumber))
				buff_matrix = [buff_matrix, curr%get_all()]
			end select


			call this%next()
		end do

		matrixKrill = reshape(buff_matrix, (/krillNumber, 18/))
		print *, matrixKrill
		deallocate(buff_matrix)
	end function matrixKrill 				
end module listKrill_mod
