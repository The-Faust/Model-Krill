! utility_krill_mod.f90

module utility_krill_mod

use class_krill
use listKrill_mod
use netcdf

contains
	subroutine evolveList(listOfkrill)
		class(listKrill) :: listOfkrill
		class(*), pointer :: curr
		

                !character(*), intent(in), optional :: netcdfDataset

		call listOfkrill%reset()
                
               ! if (present(netcdfDataset))
                
                !else 

        		do while(listOfkrill%moreValues())			
	        		select type(curr => listOfkrill%currentValue())
		        	class is (Krill)
			        	call curr%grow()
				        call curr%molt()
		        	end select

			        call listOfkrill%next()		
	        	end do
		!endif
		!call listOfkrill%printList()
	end subroutine evolveList

        subroutine netcdfKrill(akrillList, datasetNameWrite, period, jump_time, datasetNameRead) 
                class(listKrill) :: akrillList
                class(*), pointer :: curr

                ! iterators
                integer :: i, j, k

                ! variables for the netcdf procedures
                character(*), intent(in) :: datasetNameWrite
                character(*), intent(in), optional :: datasetNameRead
                character(30) :: path_to_dataset
                integer :: ioerr, period, jump_time
                integer(kind = 4) :: file_ID, numberKrillID, attributesKrillID,specieID, sexID, sizeID, massID, &
                                &       dev_freqID, molt_sizeID, awID, bwID,eiID, a_moltID, b_moltID, k0ID, h0ID, &
                                &       AID, r0ID, p_zooID, p_phytoID, w_moltID,matrixID, periodID
                integer :: numberTime, n

                ! buffer arrays to allocate values to netcdf variables
                integer, allocatable :: all_specie(:,:)
                integer, allocatable :: all_sex(:,:)
                real, allocatable :: all_size(:,:)
                real, allocatable :: all_mass(:,:)
               

                path_to_dataset = "../KrillModelNETCDF/"
                n = akrillList%n_krill()
                numberTime = period/jump_time
                print *, "number of krills = ", akrillList%n_krill()

                print *, "allocating memory for buffer arrays"
                allocate(all_specie(n, numberTime), all_sex(n, numberTime))
                !allocate(all_sex(n, numberTime))
                allocate(all_size(n, numberTime), all_mass(n, numberTime))
                !allocate(all_mass(n, numberTime))

                ! Creation of the netcdf dataset
                print *, "creation of netcdf file : ", datasetNameWrite
                ioerr = nf90_create(path = trim(path_to_dataset)//datasetNameWrite, cmode = nf90_clobber, ncid = file_ID)
                if (ioerr /= nf90_noerr) print*,'173',nf90_strerror(ioerr)

                ! define the dimension
                ioerr = nf90_def_dim(file_ID, "numberKrill", n, numberKrillID)
                if (ioerr /= nf90_noerr) print*, 'NUMBERKRILL_ERROR', nf90_strerror(ioerr)
                ioerr = nf90_def_dim(file_ID, "period", nf90_unlimited, periodID)
                if (ioerr /= nf90_noerr) print*, 'TIME_ERROR', nf90_strerror(ioerr)    

                ! creation of the variables for the files
                print *, "creation of file variables"
                ioerr = nf90_def_var(file_ID, "specie", nf90_int,(/numberKrillID, periodID/), specieID)
                if (ioerr /= nf90_noerr) print *,'CRE_SPECIE_ERROR',nf90_strerror(ioerr)
                ioerr = nf90_def_var(file_ID, "sex", nf90_int, (/numberKrillID,periodID/), sexID)
                if (ioerr /= nf90_noerr) print *,'CRE_SEX_ERROR',nf90_strerror(ioerr)
                ioerr = nf90_def_var(file_ID, "size", nf90_float,(/numberKrillID, periodID/), sizeID)
                if (ioerr /= nf90_noerr) print *,'CRE_SIZE_ERROR',nf90_strerror(ioerr)
                ioerr = nf90_def_var(file_ID, "mass", nf90_float,(/numberKrillID, periodID/), massID)
                if (ioerr /= nf90_noerr) print *,'CRE_MASS_ERROR',nf90_strerror(ioerr)

                ! end definition of file
                ioerr = nf90_enddef(file_ID)    
                if (ioerr /= nf90_noerr) print*,'DEF_ERROR',nf90_strerror(ioerr)

                ! filling up dimesions and values
                j = 1

                do k = 1, period
                        call akrillList%reset()
                
                        i = 1

                        print *, "putting values in buffer arrays"

                        if (mod(real(k), real(jump_time)) == 0) then
                                do while(akrillList%moreValues())
                                        curr => akrillList%currentValue()
                        
                                        select type(curr)
                                        type is (Krill)
                                                all_specie(i, j) =curr%get_specie()
                                                all_sex(i, j) = curr%get_sex()
                                                all_size(i, j) = curr%get_size()
                                                all_mass(i, j) = curr%get_mass()
                                        end select
                        
                                        i = i + 1
                                        call akrillList%next()
                                end do

                                j = j + 1       

                                ! allocate values to variables
                                print *, "allocating values to file variables"
                                ioerr = nf90_put_var(file_ID, specieID,all_specie, (/1, 1/), (/n, numberTime/))
                                if (ioerr /= nf90_noerr) print *,'PUT_SPECIE_ERROR',nf90_strerror(ioerr)
                                ioerr = nf90_put_var(file_ID, sexID, all_sex,(/1, 1/), (/n, numberTime/))
                                if (ioerr /= nf90_noerr) print *,'PUT_SEX_ERROR',nf90_strerror(ioerr)
                                ioerr = nf90_put_var(file_ID, sizeID, all_size,(/1, 1/), (/n, numberTime/))
                                if (ioerr /= nf90_noerr) print *,'PUT_SIZE_ERROR',nf90_strerror(ioerr)
                                ioerr = nf90_put_var(file_ID, massID, all_mass,(/1, 1/), (/n, numberTime/))
                                if (ioerr /= nf90_noerr) print *,'PUT_MASS_ERROR',nf90_strerror(ioerr)
                        endif
                        
                        ! making krills continue  their lives
                        call evolvelist(akrillList)
                end do
                ! closing file
                ioerr = nf90_close(file_ID)
                if (ioerr /= nf90_noerr) print *, 'CLOSE_ERROR',nf90_strerror(ioerr) 

                ! deallocating all allocatable arrays
                print *, "deallocating memory of buffer arrays"
                deallocate(all_specie)
                deallocate(all_sex)
                deallocate(all_size)
                deallocate(all_mass)

                print *, "creation of file (", datasetNameWrite, ") complete"
        end subroutine netcdfKrill

end module utility_krill_mod
