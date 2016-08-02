PROGRAM test_prog1

use class_krill
use listKrill_mod
use utility_krill_mod
use netcdf

type(listKrill) :: list
type(Krill) :: bernard, roger, olivia

	call bernard%init_krill(1.0, 1, 1)
	call roger%init_krill(2.0, 0, 0)
	call olivia%init_krill(3.0, 1, 1)

	call list%addKrill(bernard)
	call list%addKrill(roger)
	call list%addKrill(olivia)

!	call list%printList()

!	call evolveList(list)
	
!	print *, list%matrixKrill()

	call list%netcdfKrill("tes1.nc")
END PROGRAM test_prog1


