PROGRAM test_prog2
use class_krill
use listKrill_mod

type(listKrill) :: list

type(Krill) :: k

do i=1,10
        

        call k%init_krill(1.0, 1, 1)
        call list%addKrill(k)
end do

 call list%printList()

END PROGRAM test_prog2


