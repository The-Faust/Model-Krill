!! abstList_mod.f90

module abstList_mod
	use link_mod
	private
	public :: list
	type, abstract :: list
		private
			class(link), pointer :: firstLink => null() ! first link in list
			class(link), pointer :: lastLink => null() ! last link in list
			class(link), pointer :: currLink => null() ! list iterator

		contains
			procedure, non_overridable :: addValue ! add class(*) to list
			procedure, non_overridable :: firstValue ! returns value of first link in line
			procedure, non_overridable :: reset ! reset listiterator
			procedure, non_overridable :: next ! increment list iterator
			procedure, non_overridable :: currentValue ! get value from iterLink
			procedure, non_overridable :: moreValues ! more values for iterator?
			procedure(printValues), deferred :: printList ! prints values in list
	end type list

	abstract interface
		subroutine printValues(this)
			import list
			class(list) :: this
		end subroutine printValues
	end interface

contains

	subroutine addvalue(this, value)
		class(list) :: this
		class(*) :: value
		class(link), pointer :: newlink

		if (.not. associated(this%firstLink)) then
			this%firstLink => link(value, this%firstLink)
			this%lastlink => this%firstLink
		else
			newLink => link(value, this%lastLink%nextLink())
			call this%lastLink%setNextLink(newLink)
			this%lastLink => newLink
		endif
	end subroutine addvalue

	function firstValue(this)
		class(list) :: this
		class(*), pointer :: firstValue

		firstValue => this%firstLink%getValue()
	end function firstValue

	function currentValue(this)
		class(list) :: this
		class(*), pointer :: currentValue
		currentValue => this%currLink%getValue()
	end function currentValue

	subroutine next(this)
		class(list) :: this
		this%currLink => this%currLink%nextLink()
	end subroutine next

	function moreValues(this)
		class(list) :: this
		logical moreValues
		moreValues = associated(this%currLink)
	end function moreValues

	subroutine reset(this)
		class(list) :: this
		this%currLink => this%firstLink
	end subroutine reset
end module abstList_mod
		

			
	
