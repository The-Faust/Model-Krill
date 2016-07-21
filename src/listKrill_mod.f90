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
!		class(*), pointer :: curr

		! variables for the netcdf procedures
		character(*), intent(in) :: datasetName
		character(30) :: path_to_dataset
		integer :: ierr
		integer(kind = 4) :: file_ID, numberKrillID, attributesKrillID, specieID, sexID, sizeID, massID, &
				&	dev_freqID, molt_sizeID, awID, bwID, eiID, a_moltID, b_moltID, k0ID, h0ID, &
				&	AID, r0ID, p_zooID, p_phytoID, w_moltID, matrixID
		integer, dimension(2) :: matrixDim

!		! buffer arrays to allocate values to netcdf variables
!		integer, dimension(:), allocatable :: all_specie
!		integer, dimension(:), allocatable :: all_sex
!		real, dimension(:), allocatable :: all_size
!		real, dimension(:), allocatable :: all_mass
!		real, dimension(:), allocatable :: all_dev_freq
!		real, dimension(:), allocatable :: all_molt_size
!		real, dimension(:), allocatable :: all_aw
!		real, dimension(:), allocatable :: all_bw
!		real, dimension(:), allocatable :: all_ei
!		real, dimension(:), allocatable :: all_a_molt
!		real, dimension(:), allocatable :: all_b_molt
!		real, dimension(:), allocatable :: all_k0
!		real, dimension(:), allocatable :: all_h0
!		real, dimension(:), allocatable :: all_A
!		real, dimension(:), allocatable :: all_r0
!		real, dimension(:), allocatable :: all_p_zoo
!		real, dimension(:), allocatable :: all_p_phyto
!		real, dimension(:), allocatable :: all_w_molt 

		path_to_dataset = "../KrillModelNETCDF/"

!		call this%reset()
!
!		do while(this%moreValues())
!			curr => this%currentValue()
!			
!			select type(curr)
!			type is (Krill)
!				all_specie = [all_specie, curr%get_specie()]
!				all_sex = [all_sex, curr%get_sex()]
!				all_size = [all_size, curr%get_size()]
!				all_mass = [all_mass, curr%get_mass()]
!				all_dev_freq = [all_dev_freq, curr%get_dev_freq()]
!				all_molt_size = [all_molt_size, curr%get_molt_size()]
!				all_aw = [all_aw, curr%get_aw()]
!				all_bw = [all_bw, curr%get_bw()]
!				all_ei = [all_ei, curr%get_ei()]
!				all_a_molt = [all_a_molt, curr%get_a_molt()]
!				all_b_molt = [all_b_molt, curr%get_b_molt()]
!				all_k0 = [all_k0, curr%get_k0()]
!				all_h0 = [all_h0, curr%get_h0()]
!				all_A = [all_A, curr%get_A()]
!				all_r0 = [all_r0, curr%get_r0()]
!				all_p_zoo = [all_p_zoo, curr%get_p_zoo()]
!				all_p_phyto = [all_p_phyto, curr%get_p_phyto()]
!				all_w_molt = [all_w_molt, curr%get_w_molt()]
!			end select
!
!			call this%next()
!		end do

				! Creation of the netcdf dataset
		ierr = nf90_create(path = path_to_dataset//datasetName, cmode = nf90_clobber/nf90_hdf5, ncid = file_ID)

		! define the dimension
		ierr = nf90_def_dim(file_ID, "numberKrill", nf90_unlimited, numberKrillID)
		ierr = nf90_def_dim(file_ID, "attributesKrill", nf90_unlimited, attributesKrillID)

		matrixDim = (/ numberKrillID, attributesKrillID /) 

		! creation of the variables for the files
!		ierr = nf90_def_var(file_ID, "specie", nf90_int, numberKrillID, specieID)
!		ierr = nf90_def_var(file_ID, "sex", nf90_int, numberKrillID, sexID)
!		ierr = nf90_def_var(file_ID, "size", nf90_float, numberKrillID, sizeID)
!		ierr = nf90_def_var(file_ID, "mass", nf90_float, numberKrillID, massID)
!		ierr = nf90_def_var(file_ID, "dev_freq", nf90_float, numberKrillID, dev_freqID)
!		ierr = nf90_def_var(file_ID, "molt_size", nf90_float, numberKrillID, molt_sizeID)
!		ierr = nf90_def_var(file_ID, "aw", nf90_float, numberKrillID, awID)
!		ierr = nf90_def_var(file_ID, "bw", nf90_float, numberKrillID, bwID)
!		ierr = nf90_def_var(file_ID, "ei", nf90_float, numberKrillID, eiID)
!		ierr = nf90_def_var(file_ID, "a_molt", nf90_float, numberKrillID, a_moltID)
!		ierr = nf90_def_var(file_ID, "b_molt", nf90_float, numberKrillID, b_moltID)
!		ierr = nf90_def_var(file_ID, "k0", nf90_float, numberKrillID, k0ID)
!		ierr = nf90_def_var(file_ID, "h0", nf90_float, numberKrillID, h0ID)
!		ierr = nf90_def_var(file_ID, "A", nf90_float, numberKrillID, AID)
!		ierr = nf90_def_var(file_ID, "r0", nf90_float, numberKrillID, r0ID)
!		ierr = nf90_def_var(file_ID, "p_zoo", nf90_float, numberKrillID, p_zooID)
!		ierr = nf90_def_var(file_ID, "p_phyto", nf90_float, numberKrillID, p_phytoID)
!		ierr = nf90_def_var(file_ID, "w_molt", nf90_float, numberKrillID, w_moltID)
		ierr = nf90_def_var(file_ID, "matrix_of_krill", nf90_real, matrixDim, matrixID)
!		
!
!		! allocate values to variables
!		ierr = nf90_put_var(file_ID, specieID, all_specie)
!		ierr = nf90_put_var(file_ID, sexID, all_sex)
!		ierr = nf90_put_var(file_ID, sizeID, all_size)
!		ierr = nf90_put_var(file_ID, massID, all_mass)
!		ierr = nf90_put_var(file_ID, dev_freqID, all_dev_freq)
!		ierr = nf90_put_var(file_ID, molt_sizeID, all_molt_size)
!		ierr = nf90_put_var(file_ID, awID, all_aw)
!		ierr = nf90_put_var(file_ID, bwID, all_bw)
!		ierr = nf90_put_var(file_ID, eiID, all_ei)
!		ierr = nf90_put_var(file_ID, a_moltID, all_a_molt)
!		ierr = nf90_put_var(file_ID, b_moltID, all_b_molt)
!		ierr = nf90_put_var(file_ID, k0ID, all_k0)
!		ierr = nf90_put_var(file_ID, h0ID, all_h0)
!		ierr = nf90_put_var(file_ID, AID, all_A)
!		ierr = nf90_put_var(file_ID, r0ID, all_r0)
!		ierr = nf90_put_var(file_ID, p_zooID, all_p_zoo)
!		ierr = nf90_put_var(file_ID, p_phytoID, all_p_phyto)
!		ierr = nf90_put_var(file_ID, w_moltID, all_w_molt)
		ierr = nf90_put_var(file_ID, matrixID, this%matrixKrill())
		! closing file
		ierr = nf90_close(file_ID)
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
