subroutine parall_pre_calibration
use mymodule
implicit none


integer :: i,allocate_err1=0,allocate_err2=0,allocate_err3=0,allocate_err4=0,lentrim=1,all_lentrim

allocate(p_best_obj_fn_val_arr(seed_rank_in_one_pop),stat=allocate_err1)
allocate(p_best_arr3(seed_rank_in_one_pop,para_rank),stat=allocate_err2)
allocate(g_best_arr2(para_rank),stat=allocate_err3)
!allocate(simu_datas_arr(observed_data_rank),stat=allocate_err4)

if(allocate_err1 + allocate_err2 + allocate_err3 /= 0) then
    write(*,*) 'in parall_pre_calibration.f90:12,allocate err!'
	stop
end if

do i=1,seed_rank_in_one_pop
	p_best_obj_fn_val_arr(i) = -999999999.0
end do

lentrim = 1
do i=1,file_kind_rank
    lentrim = lentrim + len_trim(detail_each_file_include_para_arr(i)%postfile_name)
    all_lentrim = len_trim(swat_input_file_postfix_names)
    swat_input_file_postfix_names(all_lentrim+1:all_lentrim+lentrim) = detail_each_file_include_para_arr(i)%postfile_name(1:lentrim)
end do

files_need_modi=0
!write(*,*) mod(myid,population_rank)*seed_rank_in_one_pop+newmyid+1
do i=1,para_rank
    files_need_modi = para_start_value_3arr&
        (mod(myid,population_rank)*seed_rank_in_one_pop+newmyid+1,i)%size + files_need_modi
end do

!write(*,*) myid,numprocs,newmyid,newprocsnum

allocate(g_best_place_vals_arr(files_need_modi),stat=allocate_err1)
if(allocate_err1 /= 0) then
    write(*,*) 'in parall_pre_calibration.f90:35,allocate err!'
    stop
end if

allocate(para_values_2arr_in_one_process(new_max_interval,files_need_modi),stat=allocate_err1)
allocate(obj_fn_valuses_in_one_process(new_max_interval),stat=allocate_err2)
allocate(seed_nums_in_one_process(new_max_interval),stat=allocate_err3)

if(allocate_err1 + allocate_err2 + allocate_err3 /= 0) then
    write(*,*) 'in parall_pre_calibration.f90:22,allocate err!'
  stop
end if


end subroutine parall_pre_calibration
