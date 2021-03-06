PROGRAM test_prog2


use class_krill
use listKrill_mod
use utility_krill_mod
use ran_mod
use netcdf

implicit none 

type(listKrill) :: list
type(Krill) :: k
integer :: i,j,period,nb_ind
integer :: species,sex
real :: sizer,aw,bw,ei,a_molt,b_molt,k0,h0,A,r0,p_zoo,p_phyto,w_molt 
integer :: ioerr

real, parameter :: T = 12.0     ! TEMPORARY
real, parameter :: PHYTO = 0.5  ! TEMPORARY
real, parameter :: ZOO = 0.5    ! TEMPORARY

namelist /physio_nml/nb_ind,period,species,sex,aw,bw,ei,a_molt,b_molt,k0,h0,A,r0,p_zoo,p_phyto,w_molt 

open(10,file='physio.list',status='old',action='read',err=1,iostat=ioerr)
1 if(ioerr/=0) stop '!!! Problem opening file physio.list !!!'

read(10,nml=physio_nml)

close(10)

do i=1,nb_ind
        
        sizer = spread(20.0,40.0)
        call k%init_krill(sizer,species,sex,aw,bw,ei,a_molt,b_molt,k0,h0,A,r0,p_zoo,p_phyto,w_molt,T,PHYTO,ZOO)
        call list%addKrill(k)
end do

 call list%printList()
 
do j= 1,period

        call evolvelist(list)
        call list%netcdfKrill('test2.nc')
end do 

print *, list%matrixKrill()


END PROGRAM test_prog2
