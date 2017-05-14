subroutine calibration_pso_ga
use mymodule
implicit none

integer :: i,j,m,n,int_index,seed_num


gado:do i=1,ga_recur_rank

	! pop:do j=1,population_rank
		!if(mod(myid,population_rank) /= (j-1)) cycle pop
	call calibration(i,mod(myid,population_rank))
	! end do pop
	
end do gado

end subroutine calibration_pso_ga
