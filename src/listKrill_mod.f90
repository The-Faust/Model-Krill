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
!			procedure, public :: matrixKrill ! return a matrix of all the attributes of all the krills
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

!	subroutine matrixKrill(this)
!		class(listKrill) :: this
!		class(*), pointer :: curr
!
!		real, dimension(:,:), allocatable :: Krills		
!		real, dimension(:), allocatable :: buff_matrix
!		integer :: krillNumber 
!		
!		call this%reset()
!		krillNumber = 0
!
!		do while(this%moreValues())
!			curr => this%currentValue()
!			krillNumber = krillNumber + 1
!			
!			select type(curr)
!			type is (Krill)
!				buff_matrix = [buff_matrix, curr%get_all()]
!			end select
!
!
!			call this%next()
!		end do
!
!		Krills = reshape(buff_matrix, (/krillNumber, 18/))
!		print *, Krills
!
!		deallocate(buff_matrix)
!		deallocate(Krills)
!	end subroutine matrixKrill 

	subroutine netcdfKrill(this, datasetName)

		class(listKrill) :: this
		class(*), pointer :: curr

		! buffer arrays to allocate values to netcdf variables
		integer, dimension(:), allocatable :: all_specie
		integer, dimension(:), allocatable :: all_sex
		real, dimension(:), allocatable :: all_size
		real, dimension(:), allocatable :: all_mass
		real, dimension(:), allocatable :: all_dev_freq
		real, dimension(:), allocatable :: all_molt_size
		real, dimension(:), allocatable :: all_aw
		real, dimension(:), allocatable :: all_bw
		real, dimension(:), allocatable :: all_ei
		real, dimension(:), allocatable :: all_a_molt
		real, dimension(:), allocatable :: all_b_molt
		real, dimension(:), allocatable :: all_k0
		real, dimension(:), allocatable :: all_h0
		real, dimension(:), allocatable :: all_A
		real, dimension(:), allocatable :: all_r0
		real, dimension(:), allocatable :: all_p_zoo
		real, dimension(:), allocatable :: all_p_phyto
		real,dimension(:), allocatable :: all_w_molt 

		! variables for the netcdf procedures
		character(*) :: datasetName
		character(30) :: path_to_dataset
		integer :: status
		integer(kind = 4) :: ncid, numberKrillID, specieID, sexID, sizeID, massID, &
				&	dev_freqID, molt_sizeID, awID, bwID, eiID, a_moltID, b_moltID, k0ID, h0ID, &
				&	AID, r0ID, p_zooID, p_phytoID, w_moltID

		path_to_dataset = "../KrillModelNETCDF/"

		! Creation of the netcdf dataset
		status = nf90_create(path_to_dataset//datasetName, nf90_noclobber/nf90_hdf5, ncid)

		! definition of dimensions for the dataset
		status = nf90_def_dim(ncid, "numberKrill", nf90_unlimited, numberKrillID)

		! creation of the variables for the files
		status = nf90_def_var(ncid, "specie", nf90_int, numberKrillID, specieID)
		status = nf90_def_var(ncid, "sex", nf90_int, numberKrillID, sexID)
		status = nf90_def_var(ncid, "size", nf90_float, numberKrillID, sizeID)
		status = nf90_def_var(ncid, "mass", nf90_float, numberKrillID, massID)
		status = nf90_def_var(ncid, "dev_freq", nf90_float, numberKrillID, dev_freqID)
		status = nf90_def_var(ncid, "molt_size", nf90_float, numberKrillID, molt_sizeID)
		status = nf90_def_var(ncid, "aw", nf90_float, numberKrillID, awID)
		status = nf90_def_var(ncid, "bw", nf90_float, numberKrillID, bwID)
		status = nf90_def_var(ncid, "ei", nf90_float, numberKrillID, eiID)
		status = nf90_def_var(ncid, "a_molt", nf90_float, numberKrillID, a_moltID)
		status = nf90_def_var(ncid, "b_molt", nf90_float, numberKrillID, b_moltID)
		status = nf90_def_var(ncid, "k0", nf90_float, numberKrillID, k0ID)
		status = nf90_def_var(ncid, "h0", nf90_float, numberKrillID, h0ID)
		status = nf90_def_var(ncid, "A", nf90_float, numberKrillID, AID)
		status = nf90_def_var(ncid, "r0", nf90_float, numberKrillID, r0ID)
		status = nf90_def_var(ncid, "p_zoo", nf90_float, numberKrillID, p_zooID)
		status = nf90_def_var(ncid, "p_phyto", nf90_float, numberKrillID, p_phytoID)
		status = nf90_def_var(ncid, "w_molt", nf90_float, numberKrillID, w_moltID)

		call this%reset()

		do while(this%moreValues())
			curr => this%currentValue()
			
			select type(curr)
			type is (Krill)
				all_specie = [all_specie, curr%get_specie()]
				all_sex = [all_sex, curr%get_sex()]
				all_size = [all_size, curr%get_size()]
				all_mass = [all_mass, curr%get_mass()]
				all_dev_freq = [all_dev_freq, curr%get_dev_freq()]
				all_molt_size = [all_molt_size, curr%get_molt_size()]
				all_aw = [all_aw, curr%get_aw()]
				all_bw = [all_bw, curr%get_bw()]
				all_ei = [all_ei, curr%get_ei()]
				all_a_molt = [all_a_molt, curr%get_a_molt()]
				all_b_molt = [all_b_molt, curr%get_b_molt()]
				all_k0 = [all_k0, curr%get_k0()]
				all_h0 = [all_h0, curr%get_h0()]
				all_A = [all_A, curr%get_A()]
				all_r0 = [all_r0, curr%get_r0()]
				all_p_zoo = [all_p_zoo, curr%get_p_zoo()]
				all_p_phyto = [all_p_phyto, curr%get_p_phyto()]
				all_w_molt = [all_w_molt, curr%get_w_molt()]
			end select

			call this%next()
		end do


		! allocate values to variables
		status = nf90_put_var(ncid, specieID, all_specie, stride)
		status = nf90_put_var(ncid, sexID, all_sex, stride)
		status = nf90_put_var(ncid, sizeID, all_size, stride)
		status = nf90_put_var(ncid, massID, all_mass, stride)
		status = nf90_put_var(ncid, dev_freqID, all_dev_freq, stride)
		status = nf90_put_var(ncid, molt_sizeID, all_molt_size, stride)
		status = nf90_put_var(ncid, awID, all_aw, stride)
		status = nf90_put_var(ncid, bwID, all_bw, stride)
		status = nf90_put_var(ncid, eiID, all_ei, stride)
		status = nf90_put_var(ncid, a_moltID, all_a_molt, stride)
		status = nf90_put_var(ncid, b_moltID, all_b_molt, stride)
		status = nf90_put_var(ncid, k0ID, all_k0, stride)
		status = nf90_put_var(ncid, h0ID, all_h0, stride)
		status = nf90_put_var(ncid, AID, all_A, stride)
		status = nf90_put_var(ncid, r0ID, all_r0, stride)
		status = nf90_put_var(ncid, p_zooID, all_p_zoo, stride)
		status = nf90_put_var(ncid, p_phytoID, all_p_phyto, stride)
		status = nf90_put_var(ncid, w_moltID, all_w_molt, stride)
		
		status = nf90_close(ncid)

		deallocate(all_specie)
		deallocate(all_sex)
		deallocate(all_size)
		deallocate(all_mass)
		deallocate(all_dev_freq)
		deallocate(all_molt_size)
		deallocate(all_aw)
		deallocate(all_bw)
		deallocate(all_ei)
		deallocate(all_a_molt)
		deallocate(all_b_molt)
		deallocate(all_k0)
		deallocate(all_h0)
		deallocate(all_A)
		deallocate(all_r0)
		deallocate(all_p_zoo)
		deallocate(all_p_phyto)
		deallocate(all_w_molt)
	end subroutine netcdfKrill
end module listKrill_mod
