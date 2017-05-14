subroutine solve_ns(obser_vals,simu_vals,size,ns)
	integer,intent(in) :: size
	real,dimension(size),intent(in) :: obser_vals,simu_vals
	real,intent(inout) :: ns
	real :: ave_obser=0.0,sub_up=0.0,sub_down=0.0

    !write(*,*) simu_vals

	ave_obser=0.0
    sub_up=0.0
    sub_down=0.0
	do i=1,size
		ave_obser = ave_obser + obser_vals(i)
		! write(*,*) ave_obser
	end do
	ave_obser = ave_obser / size
	! write(*,*) 'ave_obser:',ave_obser
	do i=1,size
		sub_up = sub_up + (obser_vals(i) - simu_vals(i))**2
		sub_down = sub_down + (obser_vals(i) - ave_obser)**2
		! write(*,*) sub_up,sub_down
	end do
	
	ns = 1 - sub_up / sub_down
	! write(*,*) ns
end subroutine solve_ns
