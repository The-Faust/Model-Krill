!! listKrill_mod.f90

module listKrill_mod
	use class_krill
	use abstList_mod
	use netcdf

	private
	public :: listKrill
	type, extends(list) :: listKrill 
		contains
			! functions
			procedure :: matrixKrill ! return a matrix of all the attributes of all the krills
			procedure :: n_krill
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

	function matrixKrill(this)
		class(listKrill) :: this
		class(*), pointer :: curr

		real, dimension(:,:), allocatable :: matrixKrill		
		real, dimension(:), allocatable :: buff_matrix
		
		call this%reset()

		do while(this%moreValues())
			curr => this%currentValue()
			
			select type(curr)
			type is (Krill)
				buff_matrix = [buff_matrix, curr%get_all()]
			end select


			call this%next()
		end do

		matrixKrill = reshape(buff_matrix, (/ this%n_krill(), 18 /))
	end function matrixKrill 

	subroutine netcdfKrill(this, datasetName)
		class(listKrill) :: this
		class(*), pointer :: curr

		! iterator
		integer :: i

		! variables for the netcdf procedures
		character(*), intent(in) :: datasetName
		integer :: n
		character(30) :: path_to_dataset
		integer :: ierr
		integer(kind = 4) :: file_ID, numberKrillID, attributesKrillID, specieID, sexID, sizeID, massID, &
				&	dev_freqID, molt_sizeID, awID, bwID, eiID, a_moltID, b_moltID, k0ID, h0ID, &
				&	AID, r0ID, p_zooID, p_phytoID, w_moltID, matrixID
		integer, dimension(2) :: matrixDim

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
		real, dimension(:), allocatable :: all_w_molt 

		path_to_dataset = "../KrillModelNETCDF/"
		n = this%n_krill()
		print *, "number of krills = ", this%n_krill()

		allocate(all_specie(n))
		allocate(all_sex(n))
		allocate(all_size(n))
		allocate(all_mass(n))
		allocate(all_dev_freq(n))
		allocate(all_molt_size(n))
		allocate(all_aw(n))
		allocate(all_bw(n))
		allocate(all_ei(n))
		allocate(all_a_molt(n))
		allocate(all_b_molt(n))
		allocate(all_k0(n))
		allocate(all_h0(n))
		allocate(all_A(n))
		allocate(all_r0(n))
		allocate(all_p_zoo(n))
		allocate(all_p_phyto(n))
		allocate(all_w_molt(n))

		call this%reset()
		
		i = 1
		
		do while(this%moreValues())
			curr => this%currentValue()
			
			select type(curr)
			type is (Krill)
				all_specie(i) = curr%get_specie()
				all_sex(i) = curr%get_sex()
				all_size(i) = curr%get_size()
				all_mass(i) = curr%get_mass()
				all_dev_freq(i) = curr%get_dev_freq()
				all_molt_size(i) = curr%get_molt_size()
				all_aw(i) = curr%get_aw()
				all_bw(i) = curr%get_bw()
				all_ei(i) = curr%get_ei()
				all_a_molt(i) = curr%get_a_molt()
				all_b_molt(i) = curr%get_b_molt()
				all_k0(i) = curr%get_k0()
				all_h0(i) = curr%get_h0()
				all_A(i) = curr%get_A()
				all_r0(i) = curr%get_r0()
				all_p_zoo(i) = curr%get_p_zoo()
				all_p_phyto(i) = curr%get_p_phyto()
				all_w_molt(i) = curr%get_w_molt()
			end select
			
			i = i + 1
			call this%next()
		end do

		! Creation of the netcdf dataset
		ierr = nf90_create(path = trim(path_to_dataset)//datasetName, cmode = nf90_clobber, ncid = file_ID)
		if (ierr/=nf90_noerr) print*,'173',nf90_strerror(ierr)

		! define the dimension
		ierr = nf90_def_dim(file_ID, "numberKrill", n, numberKrillID)
		if (ierr/=nf90_noerr) print*,'134',nf90_strerror(ierr)	

		! creation of the variables for the files
		ierr = nf90_def_var(file_ID, "specie", nf90_int, numberKrillID, specieID)
		if (ierr/=nf90_noerr) print*,'186',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "sex", nf90_int, numberKrillID, sexID)
		if (ierr/=nf90_noerr) print*,'188',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "size", nf90_float, numberKrillID, sizeID)
		if (ierr/=nf90_noerr) print*,'190',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "mass", nf90_float, numberKrillID, massID)
		if (ierr/=nf90_noerr) print*,'192',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "dev_freq", nf90_float, numberKrillID, dev_freqID)
		if (ierr/=nf90_noerr) print*,'194',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "molt_size", nf90_float, numberKrillID, molt_sizeID)
		if (ierr/=nf90_noerr) print*,'196',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "aw", nf90_float, numberKrillID, awID)
		if (ierr/=nf90_noerr) print*,'198',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "bw", nf90_float, numberKrillID, bwID)
		if (ierr/=nf90_noerr) print*,'200',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "ei", nf90_float, numberKrillID, eiID)
		if (ierr/=nf90_noerr) print*,'202',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "a_molt", nf90_float, numberKrillID, a_moltID)
		if (ierr/=nf90_noerr) print*,'204',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "b_molt", nf90_float, numberKrillID, b_moltID)
		if (ierr/=nf90_noerr) print*,'206',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "k0", nf90_float, numberKrillID, k0ID)
		if (ierr/=nf90_noerr) print*,'208',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "h0", nf90_float, numberKrillID, h0ID)
		if (ierr/=nf90_noerr) print*,'210',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "A", nf90_float, numberKrillID, AID)
		if (ierr/=nf90_noerr) print*,'212',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "r0", nf90_float, numberKrillID, r0ID)
		if (ierr/=nf90_noerr) print*,'214',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "p_zoo", nf90_float, numberKrillID, p_zooID)
		if (ierr/=nf90_noerr) print*,'216',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "p_phyto", nf90_float, numberKrillID, p_phytoID)
		if (ierr/=nf90_noerr) print*,'218',nf90_strerror(ierr)
		ierr = nf90_def_var(file_ID, "w_molt", nf90_float, numberKrillID, w_moltID)
		if (ierr/=nf90_noerr) print*,'220',nf90_strerror(ierr)

		! end definition of file
		ierr = nf90_enddef(file_ID)	
		if (ierr/=nf90_noerr) print*,'224',nf90_strerror(ierr)	

		! allocate values to variables
		ierr = nf90_put_var(file_ID, specieID, all_specie, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'228',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, sexID, all_sex, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'230',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, sizeID, all_size, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'232',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, massID, all_mass, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'234',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, dev_freqID, all_dev_freq, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'236',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, molt_sizeID, all_molt_size, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'238',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, awID, all_aw, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'240',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, bwID, all_bw, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'242',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, eiID, all_ei, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'244',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, a_moltID, all_a_molt, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'246',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, b_moltID, all_b_molt, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'248',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, k0ID, all_k0, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'250',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, h0ID, all_h0, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'252',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, AID, all_A, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'254',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, r0ID, all_r0, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'256',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, p_zooID, all_p_zoo, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'258',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, p_phytoID, all_p_phyto, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'260',nf90_strerror(ierr)
		ierr = nf90_put_var(file_ID, w_moltID, all_w_molt, (/1/), (/n/))
		if (ierr/=nf90_noerr) print*,'262',nf90_strerror(ierr)

		! closing file
		ierr = nf90_close(file_ID)

		! deallocating all allocatable arrays
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
