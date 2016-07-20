module netcdf_utils
    use netcdf
    implicit none
    private

    character(100), parameter :: ERR_CREATE = "failed to create netcdf file"
    character(100), parameter :: ERR_DECLARE_DIMENSION = "failed to declare dimension"
    character(100), parameter :: ERR_DECLARE_VARIABLE = "failed to declare variable"
    character(100), parameter :: ERR_END_DEFINE = "failed to exit define mode"
    character(100), parameter :: ERR_CLOSE = "failed to close netcdf file"
    character(100), parameter :: ERR_PUT_VARIABLE = "failed to put variable in file"

    public :: netcdf_utils_save
contains
    subroutine fail_if_error(ioerr, err)
        integer :: ioerr
        character(100) :: err

        if (ioerr /= nf90_noerr) then
            print *, trim(err), ":", trim(nf90_strerror(ioerr))
            stop "ERROR"
        end if
    end subroutine fail_if_error


    subroutine netcdf_utils_save(filename, dim_name, time_measures, var_measures, var_definitions)
        character(100) :: filename, dim_name
        character(100), dimension (:) :: var_definitions
        real, dimension(:) :: time_measures
        real, dimension(:, :) :: var_measures
        integer :: ioerr
        integer :: ncid, timedimid, dimid, timeid
        integer, dimension(:), allocatable :: varids
        integer :: i, j, nb_val, nb_var

        nb_val = size(time_measures)
        nb_var = size(var_definitions)

        allocate(varids(nb_var))

        ! Create netcdf file
        ioerr = nf90_create(trim(filename), nf90_clobber, ncid)
        call fail_if_error(ioerr, ERR_CREATE)

        ! Declare each dimension
        ioerr = nf90_def_dim(ncid, "time", nb_val, timedimid)
        call fail_if_error(ioerr, ERR_DECLARE_DIMENSION)

        ioerr = nf90_def_dim(ncid, dim_name, 1, dimid)
        call fail_if_error(ioerr, ERR_DECLARE_DIMENSION)

        ! Declare each variable
        ioerr = nf90_def_var(ncid, "time", nf90_real, (/timedimid/), timeid)
        call fail_if_error(ioerr, ERR_DECLARE_VARIABLE)

        do i = 1, nb_var
            ioerr = nf90_def_var(ncid, var_definitions(i), nf90_real, (/timedimid, dimid/), varids(i))
            call fail_if_error(ioerr, ERR_DECLARE_VARIABLE)
        end do

        ! End declarations
        ioerr = nf90_enddef(ncid)
        call fail_if_error(ioerr, ERR_END_DEFINE)

        ! Write Data to file
        ioerr = nf90_put_var(ncid, timeid, time_measures)
        call fail_if_error(ioerr, ERR_PUT_VARIABLE)

        do j = 1, nb_var
            ioerr = nf90_put_var(ncid, varids(j), var_measures(j,:))
            call fail_if_error(ioerr, ERR_PUT_VARIABLE)
        end do

        ! Close netcdf file
        ioerr = nf90_close(ncid)
        call fail_if_error(ioerr, ERR_CLOSE)
    end subroutine netcdf_utils_save
end module netcdf_utils
