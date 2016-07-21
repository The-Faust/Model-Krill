!! listKrill_mod.f90

module listKrill_mod
	use class_krill
	use abstList_mod
	use netcdf

	private
	public :: listKrill
	type, extends(list) :: listKrill 
		contains
			procedure, public :: addKrill ! add krill in list
			procedure, public :: current => currentKrill ! get krill pointed by iterator
			procedure, public :: printList => printListKrill ! print the attributes of all krills in list
			procedure, public :: matrixKrill ! return a matrix of all the attributes of all the krills
			procedure, public :: thisKrill ! allow acces to a specific krill 
			procedure, public :: netcdfKrill ! generate a netcdf file for list of krills "much fun! such Krills!"
	end type listKrill

contains
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
				buff_matrix = [buff_matrix, curr%get_all()]
			end select


			call this%next()
		end do

		matrixKrill = reshape(buff_matrix, (/ krillNumber, 18 /))
	end function matrixKrill 

	subroutine netcdfKrill(this, datasetName)

		class(listKrill) :: this
		class(*), pointer :: curr

		! variables for the netcdf procedures
		character(*), intent(in) :: datasetName
		character(30) :: path_to_dataset
		integer :: ierr
		integer(kind = 4) :: file_ID, numberKrillID, attributesKrillID, specieID, sexID, sizeID, massID, &
				&	dev_freqID, molt_sizeID, awID, bwID, eiID, a_moltID, b_moltID, k0ID, h0ID, &
				&	AID, r0ID, p_zooID, p_phytoID, w_moltID, matrixID
		integer, dimension(2) :: matrixDim

		! buffer arrays to allocate values to netcdf variables
		integer, dimension(:), allocatable :: all_specie
		real, dimension(:), allocatable :: all_size

		path_to_dataset = "../KrillModelNETCDF/"

		call this%reset()

		do while(this%moreValues())
			curr => this%currentValue()
			
			select type(curr)
			type is (Krill)
				all_specie = [all_specie, curr%get_specie()]
				all_size = [all_size, curr%get_size()]
			end select

			call this%next()
		end do

				! Creation of the netcdf dataset
		ierr = nf90_create(path = trim(path_to_dataset)//trim(datasetName), cmode = nf90_clobber, ncid = file_ID)
		if (ierr/=nf90_noerr) print*,'134',nf90_strerror(ierr)

		! define the dimension
		ierr = nf90_def_dim(file_ID, "numberKrill", nf90_unlimited, numberKrillID)
!		ierr = nf90_def_dim(file_ID, "numberKrill", 10, numberKrillID)
		if (ierr/=nf90_noerr) print*,'139',nf90_strerror(ierr)
!		ierr = nf90_def_dim(file_ID, "attributesKrill", nf90_unlimited, attributesKrillID)
		
		

!		matrixDim = (/ numberKrillID, attributesKrillID /) 

		! creation of the variables for the files
		ierr = nf90_def_var(file_ID, "specie", nf90_int, numberKrillID, specieID)
		if (ierr/=nf90_noerr) print*,'148',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "size", nf90_float, numberKrillID, sizeID)
		if (ierr/=nf90_noerr) print*,'150',nf90_strerror(ierr)
!
	        ierr = nf90_enddef(file_ID)	
		if (ierr/=nf90_noerr) print*,'153',nf90_strerror(ierr)
!
		! allocate values to variables
		ierr = nf90_put_var(file_ID, specieID, all_specie, (/1/), (/3/) )
		if (ierr/=nf90_noerr) print*,'157',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, sizeID, all_size, (/1/), (/3/))
		if (ierr/=nf90_noerr) print*,'159',nf90_strerror(ierr)

		! closing file
		ierr = nf90_close(file_ID)
		if (ierr/=nf90_noerr) print*,'163',nf90_strerror(ierr)
!		
!		! deallocating all allocatable arrays
!		deallocate(all_specie)
!		deallocate(all_sex)
!		deallocate(all_size)
!		deallocate(all_mass)
!		deallocate(all_dev_freq)
!		deallocate(all_molt_size)
!		deallocate(all_aw)
!		deallocate(all_bw)
!		deallocate(all_ei)
!		deallocate(all_a_molt)
!		deallocate(all_b_molt)
!		deallocate(all_k0)
!		deallocate(all_h0)
!		deallocate(all_A)
!		deallocate(all_r0)
!		deallocate(all_p_zoo)
!		deallocate(all_p_phyto)
!		deallocate(all_w_molt)
	end subroutine netcdfKrill
end module listKrill_mod
