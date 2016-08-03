! utility_krill_mod.f90

module utility_krill_mod

use class_krill
use listKrill_mod
use ran_mod
use netcdf

contains
	subroutine evolveList(listOfkrill, path_to_netcdfDataset)
		class(listKrill) :: listOfkrill
		class(*), pointer :: curr

		real :: temp
		real :: zoofoo
		real :: phytofoo

		character(*), intent(in), optional :: path_to_netcdfDataset
		
		call listOfkrill%reset()

		if (present(path_to_netcdfDataset))
		
		else
			do while(listOfkrill%moreValues())			
				select type(curr => listOfkrill%currentValue())
				class is (Krill)
					call curr%grow()
					call curr%molt()
				end select

				call listOfkrill%next()		
			end do
		endif
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
			call akrill%set_T(T)
			call akrill%set_phyto(phyto)
			call akrill%set_zoo(zoo)

			list%addKrill(akrill)
		end do
	end subroutine generateKrill

	subroutine netcdfKrill(akrillList, datasetNameWrite, period, jumpTime, datasetNameRead)
		class(listKrill) :: akrillList
		class(*), pointer :: curr

		! iterators
		integer :: i, j, k

		! variables for the netcdf procedures
		character(*), intent(in) :: datasetNameWrite
		character(*), intent(in), optional :: datasetNameRead
		character(30) :: path_to_output_dataset, path_to_input_dataset
		integer :: ioerr, period, jumpTime, numbertime, n
		integer(kind = 4) :: file_ID, numberKrillID, attributesKrillID, specieID, sexID, sizeID, massID, &
				&	dev_freqID, molt_sizeID, awID, bwID, eiID, a_moltID, b_moltID, k0ID, h0ID, &
				&	AID, r0ID, p_zooID, p_phytoID, w_moltID, matrixID, timeID

		! buffer arrays to allocate values to netcdf variables
		integer, dimension(:,:), allocatable :: all_specie
		integer, dimension(:,:), allocatable :: all_sex
		real, dimension(:,:), allocatable :: all_size
		real, dimension(:,:), allocatable :: all_mass

		path_to_output_dataset = "../../KrillModelNETCDF/outputKrill"
		path_to_input_dataset = "../../KrillModelNETCDF/inputValues"
		n = akrillList%n_krill()
		numbertime = time/jumpTime
		print *, "number of krills = ", akrillList%n_krill()

		print *, "allocating memory for buffer arrays"
		allocate(all_specie(n, numbertime))
		allocate(all_sex(n, numbertime))
		allocate(all_size(n, numbertime))
		allocate(all_mass(n, numbertime))

		! Creation of the netcdf dataset
		print *, "creation of netcdf file : ", datasetNameWrite
		ioerr = nf90_create(path = trim(path_to_output_dataset)//datasetNameWrite, cmode = nf90_clobber, ncid = file_ID)
		if (ioerr /= nf90_noerr) print*,'173',nf90_strerror(ioerr)

		! define the dimension
		ioerr = nf90_def_dim(file_ID, "numberKrill", n, numberKrillID)
		if (ioerr /= nf90_noerr) print*, 'NUMBERKRILL_ERROR', nf90_strerror(ioerr)
		ioerr = nf90_def_dim(file_ID, "time", nf90_unlimited, timeID)
		if (ioerr /= nf90_noerr) print*, 'time_ERROR', nf90_strerror(ioerr)	

		! creation of the variables for the files
		print *, "creation of file variables"
		ioerr = nf90_def_var(file_ID, "specie", nf90_int, (/numberKrillID, timeID/), specieID)
		if (ioerr /= nf90_noerr) print *, 'CRE_SPECIE_ERROR',nf90_strerror(ioerr)
		ioerr = nf90_def_var(file_ID, "sex", nf90_int, (/numberKrillID, timeID/), sexID)
		if (ioerr /= nf90_noerr) print *, 'CRE_SEX_ERROR',nf90_strerror(ioerr)
		ioerr = nf90_def_var(file_ID, "size", nf90_float, (/numberKrillID, timeID/), sizeID)
		if (ioerr /= nf90_noerr) print *, 'CRE_SIZE_ERROR',nf90_strerror(ioerr)
		ioerr = nf90_def_var(file_ID, "mass", nf90_float, (/numberKrillID, timeID/), massID)
		if (ioerr /= nf90_noerr) print *, 'CRE_MASS_ERROR',nf90_strerror(ioerr)

		! end definition of file
		ioerr = nf90_enddef(file_ID)	
		if (ioerr /= nf90_noerr) print*,'DEF_ERROR',nf90_strerror(ioerr)

		! filling up dimesions and values
		j = 1

		do k = 1, period
			call akrillList%reset()
		
			i = 1

			if (mod(real(k), real(jumpTime)) == 0) then
				print *, "putting values in buffer arrays"
				do while(akrillList%moreValues())
					curr => akrillList%currentValue()
			
					select type(curr)
					type is (Krill)
						all_specie(i, j) = curr%get_specie()
						all_sex(i, j) = curr%get_sex()
						all_size(i, j) = curr%get_size()
						all_mass(i, j) = curr%get_mass()
					end select
			
					i = i + 1
					call akrillList%next()
				end do

				j = j + 1	

				! allocate values to variables
			endif
			
			! making krills continue  their daily lives
			call evolvelist(akrillList, trim(path_to_input_dataset)//datasetNameRead)
		end do

		print *, "allocating values to file variables"
		ioerr = nf90_put_var(file_ID, specieID, all_specie, (/1, 1/), (/n, numbertime/))
		if (ioerr /= nf90_noerr) print *, 'PUT_SPECIE_ERROR',nf90_strerror(ioerr)
		ioerr = nf90_put_var(file_ID, sexID, all_sex, (/1, 1/), (/n, numbertime/))
		if (ioerr /= nf90_noerr) print *, 'PUT_SEX_ERROR',nf90_strerror(ioerr)
		ioerr = nf90_put_var(file_ID, sizeID, all_size, (/1, 1/), (/n, numbertime/))
		if (ioerr /= nf90_noerr) print *, 'PUT_SIZE_ERROR',nf90_strerror(ioerr)
		ioerr = nf90_put_var(file_ID, massID, all_mass, (/1, 1/), (/n, numbertime/))
		if (ioerr /= nf90_noerr) print *, 'PUT_MASS_ERROR',nf90_strerror(ioerr)

		! closing file
		ioerr = nf90_close(file_ID)
		if (ioerr /= nf90_noerr) print *, 'CLOSE_ERROR', nf90_strerror(ioerr) 

		! deallocating all allocatable arrays
		print *, "deallocating memory of buffer arrays"
		deallocate(all_specie)
		deallocate(all_sex)
		deallocate(all_size)
		deallocate(all_mass)

		print *, "creation of file (", datasetNameWrite, ") complete"
	end subroutine netcdfKrill
end module utility_krill_mod
