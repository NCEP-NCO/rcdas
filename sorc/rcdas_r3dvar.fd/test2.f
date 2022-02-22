         include 'types.h'
         include "PARMETA.comm"
         include "mpp.h"
         include "my_comm.h"
         include "r3dv_data.comm"

	call init
	write(*,*) '>>r3dvex_bayes associated ', associated(gridcom(igrid)%rxcom)

	stop
	end
