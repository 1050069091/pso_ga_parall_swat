subroutine calibration(ga_recur_id,popid)
use mymodule
implicit none

integer,intent(in) :: popid,ga_recur_id
integer :: seed_num,i,j,k,m,n,int_i,int_index
real :: fn_obj_val = 0.0
real,dimension(observed_data_rank) :: simu_datas_arr
integer :: best_recur_num=1,best_seed_num=1

outter:do i=1,recur_rank
    int_i = 1
    tmp_g_best_obj_fn_val = -99999999.0
    inner:do j=1,seed_rank_in_one_pop
        if(mod(j-1,newprocsnum) /= newmyid) cycle inner
        seed_num = popid*seed_rank_in_one_pop+j
        if(.not.(i == 1 .and. seed_num == 1 .and. is_use_befor == 1)) then
		    call modify_para_in_inputfile(seed_num)
        end if
        call system('./swat2012_627_paral '//myid_str//' '//trim(swat_input_file_postfix_names)//' > /dev/null')
		!获得模拟值，并写入文件中
		call extract_swat_output(simu_datas_arr)
        !write(*,*) myid
		if(fn_type == 1) then
			call solve_r2(observed_data_val_arr,simu_datas_arr,observed_data_rank,fn_obj_val)
            !write(*,"(' 进化代数:',I4,' 种群编号:',I4,' 递归编号:',I4,&
            !    '    种子编号:',I4,A10,A6,F10.4,'    <-------- by node',I3)") &
            !    ga_recur_id,popid+1,i,j,'------->','  R2=',fn_obj_val,myid
		else
			call solve_ns(observed_data_val_arr,simu_datas_arr,observed_data_rank,fn_obj_val)
            !write(*,"(' 进化代数:',I4,' 种群编号:',I4,' 递归编号:',I4,&
            !    '    种子编号:',I4,A10,A6,F10.4,'    <-------- by node',I3)") &
            !    ga_recur_id,popid+1,i,j,'------->','  NS=',fn_obj_val,myid
		end if
		if(fn_obj_val > p_best_obj_fn_val_arr(j)) then
			p_best_obj_fn_val_arr(j) = fn_obj_val
			do k = 1,para_rank
				p_best_arr3(j,k)%p => para_start_value_3arr(seed_num,k)%p
				p_best_arr3(j,k)%size = para_start_value_3arr(seed_num,k)%size
			end do
            
			if(fn_obj_val > g_best_obj_fn_val) then
				do k = 1,para_rank
					g_best_arr2(k)%p => para_start_value_3arr(seed_num,k)%p
					g_best_arr2(k)%size = para_start_value_3arr(seed_num,k)%size
                    g_best_obj_fn_val = fn_obj_val
                    best_recur_num = i
                    best_seed_num = seed_num
				end do
                call system('mv -f out/'//trim(obser_val_name)&
                    //myid_str//'.txt out/best_sim_data.txt'//trim(myid_str))
                call system("cp -f `ls | grep '[.]"//myid_str//"$'` ./out")
			end if
		end if

        if(fn_obj_val > tmp_g_best_obj_fn_val) then
            tmp_g_best_obj_fn_val = fn_obj_val
        end if

        obj_fn_valuses_in_one_process(int_i) = fn_obj_val
        int_i = int_i + 1

    end do inner

    !开始通信
    best_sim_recur_id = best_recur_num
    best_sim_seed_id = best_seed_num

    call pso_communication_to_get_best_values(ga_recur_id,i,popid)

    if(g_best_obj_fn_val >= obj_fn_threshold) exit outter

    old_g_best_obj_fn_val = g_best_obj_fn_val

	call update_value_speed()

end do outter



call ga_communication_to_get_best_values(ga_recur_id)

end subroutine calibration
