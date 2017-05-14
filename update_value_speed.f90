subroutine update_value_speed
use mymodule
implicit none

integer :: para_file_rank = 0,m,n,i,j,seed_num
real :: tmp_real

! outter:do m=1,population_rank
	! if(mod(myid,population_rank) /= (m-1)) cycle outter
    inner:do n=1,seed_rank_in_one_pop
        if(mod(n-1,newprocsnum) /= newmyid) cycle inner
        seed_num = mod(myid,population_rank)*seed_rank_in_one_pop+n
	    do i=1,para_rank
		    if(sub_or_hru_arr(i) == 0) then !该参数是每sub就一个
    		    para_file_rank = substream_rank
            else 
    		    para_file_rank = hru_rank
		    end if
            do j=1,para_file_rank
                call random_number(first_random_spo)
                call random_number(second_random_spo)
                para_start_speed_3arr(seed_num,i)%p(j) = inertia_factor_spo*para_start_speed_3arr(seed_num,i)%p(j) &
                    + first_speed_factor*first_random_spo*(p_best_arr3(n,i)%p(j)-para_start_value_3arr(seed_num,i)%p(j)) &
                    + second_speed_factor*second_random_spo*(g_best_arr2(i)%p(j)-para_start_value_3arr(seed_num,i)%p(j))
                tmp_real = para_start_value_3arr(seed_num,i)%p(j) + para_start_speed_3arr(seed_num,i)%p(j)
                if(tmp_real <= max_para_arr(i) .and. tmp_real >= min_para_arr(i)) then
                    para_start_value_3arr(seed_num,i)%p(j) = tmp_real
                end if
            end do
	    end do
	end do inner
! end do outter
end subroutine update_value_speed
