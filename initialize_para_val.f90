subroutine initialize_para_val(seed_num)
use mymodule
integer,intent(in) :: seed_num
integer :: allocate_err = 0,para_file_rank = 0
real,dimension(:),pointer :: tmp_place_p,tmp_speed_p




!随机初始化种群中个体的位置和速度
!call init_random_seed()

! do k=1,seed_rank
	do i=1,para_rank

		if(sub_or_hru_arr(i) == 0) then !该参数是每sub就一个
    		para_file_rank = substream_rank
    	else 
    		para_file_rank = hru_rank
    	end if

    	allocate(tmp_place_p(para_file_rank),stat=allocate_err)
    	if(allocate_err/= 0) then
    		write(*,*) 'allocate tmp_place_p error'
			stop
		end if

		allocate(tmp_speed_p(para_file_rank),stat=allocate_err)
    	if(allocate_err/= 0) then
    		write(*,*) 'allocate tmp_speed_p error'
			stop
		end if
        
    	do j=1,para_file_rank
           call random_number(tmp_place_p(j))   !随机化每个文件中的对应参数值
           ! write(*,*) tmp_place_p(j)
           tmp_place_p(j) = min_para_arr(i) + tmp_place_p(j)*(max_para_arr(i)-min_para_arr(i))
           ! write(*,*) tmp_place_p(j)
           ! write(*,*) "--------------------------"
           call random_number(tmp_speed_p(j))    !随机化每个文件中的对应参数的速度
           tmp_speed_p(j) = speed_rate_spo*(2*tmp_speed_p(j)-1)*(max_para_arr(i)-min_para_arr(i))
    	end do
    	! write(*,*) "----------------------------------------------------------------------------"
        para_start_value_3arr(seed_num,i)%p => tmp_place_p
        para_start_value_3arr(seed_num,i)%size = para_file_rank
        para_start_speed_3arr(seed_num,i)%p => tmp_speed_p
        para_start_speed_3arr(seed_num,i)%size = para_file_rank
!        write(*,*) seed_num,'::place:::: ',para_start_value_3arr(seed_num,i)%p
!        write(*,*) seed_num,'::speed:::: ',para_start_speed_3arr(seed_num,i)%p
	end do
! end do

end subroutine initialize_para_val
