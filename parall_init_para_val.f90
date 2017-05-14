subroutine parall_init_para_value()
use mymodule
implicit none

integer :: i,j,seed_num,allocate_err = 0

allocate(para_start_value_3arr(seed_rank,para_rank),stat=allocate_err)
if(allocate_err/= 0) then
    write(*,*) 'allocate para_start_value_arr3 error'
	stop
end if

allocate(para_start_speed_3arr(seed_rank,para_rank),stat=allocate_err)
if(allocate_err/= 0) then
    write(*,*) 'allocate para_start_speed_arr3 error'
	stop
end if

allocate(files_included_by_each_para_arr(para_rank),stat=allocate_err)
if(allocate_err/= 0) then
    write(*,*) 'allocate files_included_by_each_para_arr error'
	stop
end if

!随机初始化种群中个体的位置和速度
call init_random_seed(myid+1)
outter:do i=1,population_rank
	if(mod(myid,population_rank) /= (i-1)) cycle outter
    inner:do j=1,seed_rank_in_one_pop
        if(mod(j-1,newprocsnum) /= newmyid) cycle inner
        seed_num = (i-1)*seed_rank_in_one_pop+j
        !write(*,*) "myid:",myid,numprocs,"newmyid",newmyid,newprocsnum,"seed_num:",seed_num
	    !初始化需要率定参数的值：在各参数范围内随机生成
	    call initialize_para_val(seed_num)
	end do inner
end do outter

!write(*,*) "myid:",myid,numprocs,"newmyid",newmyid,newprocsnum,"seed_num:",seed_numi,para_start_value_3arr

end subroutine parall_init_para_value
