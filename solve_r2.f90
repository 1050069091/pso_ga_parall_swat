subroutine solve_r2(obser_vals,simu_vals,size,r2)
	integer,intent(in) :: size
	real,dimension(size),intent(in) :: obser_vals,simu_vals
	real,intent(inout) :: r2
	real :: ave_obser=0.0,ave_simu=0.0,sub_up=0.0,sub_down_1=0.0,sub_down_2=0.0

	!write(*,*) obser_vals,simu_vals
	do i=1,size
		ave_obser = ave_obser + obser_vals(i)
		ave_simu = ave_simu + simu_vals(i)
	end do
	ave_obser = ave_obser / size
	ave_simu = ave_simu / size
	!write(*,*) ave_obser,ave_simu,size 

	do i=1,size
		! write(*,*) obser_vals(i) - simu_vals(i)
		sub_up = sub_up + (obser_vals(i) - ave_obser)*(simu_vals(i) - ave_simu)
		sub_down_1 = sub_down_1 + (obser_vals(i) - ave_obser)**2
		sub_down_2 = sub_down_2 + (simu_vals(i) - ave_simu)**2
	!	write(*,*) sub_up,sub_down_1,sub_down_2
	end do
	! write(*,*) 'hello'
	r2 = sub_up**2 / (sub_down_1 * sub_down_2)
	!write(*,*) sub_up,sub_down_1,sub_down_2,r2
	!write(*,*) sub_up**2,sub_down_1*sub_down_2,r2
end subroutine solve_r2
