subroutine ga_communication_to_get_best_values(recur_num)
use mymodule
use mpi
implicit none

integer,intent(in) :: recur_num

integer ierr,recv_buff_size,recv_myid,int_index,seed_num,int_j
real recv_obj_fn_values(new_max_interval*numprocs)

real send_para_values(new_max_interval*files_need_modi),rate_obj_fn_val_arr(seed_rank)
real recv_para_values(numprocs*new_max_interval*files_need_modi)

integer int_obj_fn_val(seed_rank)&!,send_seed_num(new_max_interval)&
    ,plus_int_obj_fn_val(seed_rank),seed_nums_sorted(seed_rank)

real new_all_para_values_arr(numprocs*new_max_interval*files_need_modi)

real random_real_val
integer parent_index1,parent_index2,myid1,myid2,interval1,interval2&
    ,count_ga,intersect_para_index,intersect_index1,intersect_index2,inter_time

real para_val_in_one_seed_arr1(files_need_modi),para_val_in_one_seed_arr2(files_need_modi),tmp_real

integer seed_count,tmp_interval,tmp_myid,i,j,m,n,int_i,tmp_numprocs,new_numprocs,tmp_popu,tmp_seed_num


int_i = 1
gacycle:do j=1,seed_rank_in_one_pop
    if(mod(j-1,newprocsnum) /= newmyid) cycle gacycle
    seed_num = mod(myid,population_rank)*seed_rank_in_one_pop+j
    int_index = 0
    do m=1,para_rank
        do n=1,para_start_value_3arr(seed_num,m)%size
            para_values_2arr_in_one_process(int_i,int_index+n) = p_best_arr3(j,m)%p(n)
            !para_values_2arr_in_one_process(int_i,int_index+n) = para_start_value_3arr(seed_num,m)%p(n)
        end do
        int_index = int_index + para_start_value_3arr(seed_num,m)%size
    end do
    !send_send_num(int_i) = seed_num
    int_i = int_i + 1
end do gacycle

recv_buff_size=new_max_interval*files_need_modi
recv_myid=numprocs-1

do i=1,new_max_interval
   send_para_values((i-1)*files_need_modi+1:i*files_need_modi) = &
                        para_values_2arr_in_one_process(i,1:files_need_modi)

end do

!聚集各个体目标函数值到最后进程
call MPI_GATHER(obj_fn_valuses_in_one_process,new_max_interval,MPI_REAL&
                ,recv_obj_fn_values,new_max_interval,MPI_REAL&
                ,recv_myid,mycomm,ierr)
!call MPI_GATHER(send_seed_num,new_max_interval,MPI_INTEGER&
!                ,seed_nums_sorted,new_max_interval,MPI_INTEGER&
!                ,recv_myid,mycomm,ierr)
!聚集各个体染色体上的基因到目标进程
call MPI_GATHER(send_para_values,recv_buff_size&
                ,MPI_REAL,recv_para_values,recv_buff_size&
                ,MPI_REAL,recv_myid,mycomm,ierr)

 if(myid==recv_myid) then

     int_index = 1
     int_j = 1
     do m=1,population_rank
     if(m<=mod(numprocs,population_rank)) then
         tmp_numprocs = ceiling(numprocs/(population_rank*1.0))
     else
         tmp_numprocs = floor(numprocs/(population_rank*1.0))
     end if
     do i=1,tmp_numprocs
         do j=1,new_max_interval
             int_i = (j-1)*tmp_numprocs+i 
             if(int_i <= seed_rank_in_one_pop) then 
                 seed_num = (m-1)*seed_rank_in_one_pop+int_i
                 seed_nums_sorted(int_j) = seed_num
                 int_obj_fn_val(int_j) = 10000*recv_obj_fn_values(int_index) 
                 int_j = int_j + 1
             end if
             int_index = int_index + 1
         end do
     end do
     end do

     !write(*,*) int_j,int_index,int_obj_fn_val
     call quick_sort(int_obj_fn_val,seed_nums_sorted,1,seed_rank)
     !write(*,*) int_j,int_index,int_obj_fn_val

     call date_and_time(b(1), b(2), b(3), date_time2)
     inter_time = date_time2(7)-date_time1(7) + 60*(date_time2(6)-date_time1(6)) &
            + 60*60*(date_time2(5)-date_time1(5)) + 60*60*60*(date_time2(4)-date_time1(4))&
            + 60*60*60*12*(date_time2(3)-date_time1(3))
     write(*,"(' ***************************遗传算法进化代数:',I4,&
        ' 最大目标函数值:',F8.4,'******',' 耗时:',I6,'s*****************************************')") &
        recur_num,(int_obj_fn_val(seed_rank))/10000.0,inter_time

     if(int_obj_fn_val(seed_rank) > g_best_int_obj_fn_val) then

         g_best_int_obj_fn_val = int_obj_fn_val(seed_rank)
         best_sim_myid = mod(seed_nums_sorted(seed_rank)-1,numprocs)
         call format_para_fn(5,best_sim_myid+1,best_sim_myid_str)

         best_sim_recur_id = recur_num
         best_sim_seed_id = seed_nums_sorted(seed_rank)

     end if

     plus_int_obj_fn_val(1) = 0
     do i=1,seed_rank
         if(i==1) then
             plus_int_obj_fn_val(1) = int_obj_fn_val(1)
         else
             plus_int_obj_fn_val(i) = int_obj_fn_val(i) + plus_int_obj_fn_val(i-1)
         end if
     end do

     do i=1,seed_rank
         rate_obj_fn_val_arr(i) = plus_int_obj_fn_val(i) / (plus_int_obj_fn_val(seed_rank)*1.0)
     end do

    !call init_random_seed(numprocs) 
     seed_count = 1
     outter:do 
         !选择两条染色体
         call random_number(random_real_val)  
         call find_index_choosed(0.8+0.2*random_real_val,rate_obj_fn_val_arr,seed_rank,parent_index1)
         !write(*,*) random_real_val,rate_obj_fn_val_arr,seed_rank,parent_index1
!         inner:do 
             call random_number(random_real_val)  
             call find_index_choosed(0.8+0.2*random_real_val,rate_obj_fn_val_arr,seed_rank,parent_index2)
!             if(parent_index2 /= parent_index1) exit inner

!         end do inner
         !write(*,*) random_real_val,rate_obj_fn_val_arr,seed_rank,parent_index2

         tmp_popu = floor((seed_nums_sorted(parent_index1)-1)/(seed_rank_in_one_pop*1.0))
         tmp_seed_num = mod((seed_nums_sorted(parent_index1)-1),seed_rank_in_one_pop) + 1
         tmp_numprocs = 0
         do m=0,tmp_popu-1
            if(m<mod(numprocs,population_rank)) then
                tmp_numprocs = tmp_numprocs + ceiling(numprocs/(population_rank*1.0))
            else
                tmp_numprocs = tmp_numprocs + floor(numprocs/(population_rank*1.0))
            end if

         end do

         if(tmp_popu<mod(numprocs,population_rank)) then
                new_numprocs = ceiling(numprocs/(population_rank*1.0))
         else
                new_numprocs = floor(numprocs/(population_rank*1.0))
         end if
         myid1 = mod(tmp_seed_num-1,new_numprocs)
         interval1 = floor((tmp_seed_num-1)/(new_numprocs*1.0))
         intersect_index1 = (tmp_numprocs+myid1)*new_max_interval*files_need_modi+interval1*files_need_modi

         !write(*,*) seed_nums_sorted(parent_index1),&
         !    tmp_popu,tmp_seed_num,tmp_numprocs,new_numprocs,myid1,interval1,intersect_index1

         tmp_popu = floor((seed_nums_sorted(parent_index2)-1)/(seed_rank_in_one_pop*1.0))
         tmp_seed_num = mod((seed_nums_sorted(parent_index2)-1),seed_rank_in_one_pop) + 1
         tmp_numprocs = 0
         do m=0,tmp_popu-1
            if(m<mod(numprocs,population_rank)) then
                tmp_numprocs = tmp_numprocs + ceiling(numprocs/(population_rank*1.0))
            else
                tmp_numprocs = tmp_numprocs + floor(numprocs/(population_rank*1.0))
            end if

         end do

         if(tmp_popu<mod(numprocs,population_rank)) then
                new_numprocs = ceiling(numprocs/(population_rank*1.0))
         else
                new_numprocs = floor(numprocs/(population_rank*1.0))
         end if
         myid2 = mod(tmp_seed_num-1,new_numprocs)
         interval2 = floor((tmp_seed_num-1)/(new_numprocs*1.0))
         intersect_index2 = (tmp_numprocs+myid2)*new_max_interval*files_need_modi+interval2*files_need_modi

         !write(*,*) seed_nums_sorted(parent_index2),&
         !    tmp_popu,tmp_seed_num,tmp_numprocs,new_numprocs,myid2,interval2,intersect_index2

         para_val_in_one_seed_arr1 =&
             recv_para_values(intersect_index1+1:intersect_index1+files_need_modi)
         para_val_in_one_seed_arr2 =&
             recv_para_values(intersect_index2+1:intersect_index2+files_need_modi)

         !交叉操作
         call random_number(random_real_val)
         if(random_real_val <= intersect_rate) then
             count_ga = files_need_modi * intersect_rate
             do i=1,count_ga
                call random_number(random_real_val)
                intersect_para_index = ceiling(files_need_modi * random_real_val)

                tmp_real = para_val_in_one_seed_arr1(intersect_para_index)
                para_val_in_one_seed_arr1(intersect_para_index) = para_val_in_one_seed_arr2(intersect_para_index)
                para_val_in_one_seed_arr2(intersect_para_index) = tmp_real

             end do
         end if

         !变异操作
         call varition_para_values(para_val_in_one_seed_arr1,files_need_modi,recur_num)
         call varition_para_values(para_val_in_one_seed_arr2,files_need_modi,recur_num)

         !把新个体（染色体）添加到新种群中
         tmp_popu = floor((seed_count-1)/(seed_rank_in_one_pop*1.0))
         tmp_seed_num = mod(seed_count-1,seed_rank_in_one_pop) + 1
         tmp_numprocs = 0
         do m=0,tmp_popu-1
            if(m<mod(numprocs,population_rank)) then
                tmp_numprocs = tmp_numprocs + ceiling(numprocs/(population_rank*1.0))
            else
                tmp_numprocs = tmp_numprocs + floor(numprocs/(population_rank*1.0))
            end if
         end do
         if(tmp_popu<mod(numprocs,population_rank)) then
                new_numprocs = ceiling(numprocs/(population_rank*1.0))
         else
                new_numprocs = floor(numprocs/(population_rank*1.0))
         end if
         myid1 = mod(tmp_seed_num-1,new_numprocs)
         interval1 = floor((tmp_seed_num-1)/(new_numprocs*1.0))
         intersect_index1 = (tmp_numprocs+myid1)*new_max_interval*files_need_modi+interval1*files_need_modi
         !write(*,*) seed_count,&
         !    tmp_popu,tmp_seed_num,tmp_numprocs,new_numprocs,myid1,interval1,intersect_index1
         new_all_para_values_arr(intersect_index1+1:intersect_index1+files_need_modi)&
             = para_val_in_one_seed_arr1(1:files_need_modi)
         seed_count = seed_count + 1
         if(seed_count > seed_rank) exit outter

         
         tmp_popu = floor((seed_count-1)/(seed_rank_in_one_pop*1.0))
         tmp_seed_num = mod(seed_count-1,seed_rank_in_one_pop) + 1
         tmp_numprocs = 0
         do m=0,tmp_popu-1
            if(m<mod(numprocs,population_rank)) then
                tmp_numprocs = tmp_numprocs + ceiling(numprocs/(population_rank*1.0))
            else
                tmp_numprocs = tmp_numprocs + floor(numprocs/(population_rank*1.0))
            end if
         end do
         if(tmp_popu<mod(numprocs,population_rank)) then
                new_numprocs = ceiling(numprocs/(population_rank*1.0))
         else
                new_numprocs = floor(numprocs/(population_rank*1.0))
         end if
         myid1 = mod(tmp_seed_num-1,new_numprocs)
         interval1 = floor((tmp_seed_num-1)/(new_numprocs*1.0))
         intersect_index1 = (tmp_numprocs+myid1)*new_max_interval*files_need_modi+interval1*files_need_modi
         !write(*,*) seed_count,&
         !    tmp_popu,tmp_seed_num,tmp_numprocs,new_numprocs,myid1,interval1,intersect_index1
         new_all_para_values_arr(intersect_index1+1:intersect_index1+files_need_modi)&
             = para_val_in_one_seed_arr2(1:files_need_modi)
         seed_count = seed_count + 1
         if(seed_count > seed_rank) exit outter
     end do outter
!     !write(*,*) max_para_arr
!     !write(*,*) min_para_arr
     !write(*,*) "-------------------->new:",new_all_para_values_arr
     !write(*,*) "-------------------->old:",recv_para_values
 end if

! !write(*,*) recur_num,myid,"older----:",recv_para_values

 call MPI_SCATTER(new_all_para_values_arr,recv_buff_size&
                 ,MPI_REAL,recv_para_values,recv_buff_size&
                 ,MPI_REAL,numprocs-1,mycomm,ierr)

! !write(*,*) recur_num,myid,"later----:",recv_para_values


do i=1,new_max_interval
    para_values_2arr_in_one_process(i,1:files_need_modi) = &
        recv_para_values((i-1)*files_need_modi+1:i*files_need_modi)
end do

int_i = 1
gacycle1:do j=1,seed_rank_in_one_pop
    if(mod(j-1,newprocsnum) /= newmyid) cycle gacycle1
    seed_num = mod(myid,population_rank)*seed_rank_in_one_pop+j
    int_index = 0
    do m=1,para_rank
        do n=1,para_start_value_3arr(seed_num,m)%size
            !para_values_2arr_in_one_process(int_i,int_index+n) = p_best_arr3(j,m)%p(n)
            para_start_value_3arr(seed_num,m)%p(n) = para_values_2arr_in_one_process(int_i,int_index+n) 
        end do
        int_index = int_index + para_start_value_3arr(seed_num,m)%size
    end do
    int_i = int_i + 1
end do gacycle1

end subroutine ga_communication_to_get_best_values
