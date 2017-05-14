subroutine pso_communication_to_get_best_values(recur_num,pso_recur_num,popid)
use mymodule
use mpi
implicit none

integer,intent(in) :: recur_num,pso_recur_num,popid
integer sendpair(2),recv(2),senddata(1),recvdata(1)
integer ierr,index,int_old_best_obj_fn_val,best_sim_newmyid,inter_time,i,j

sendpair(1) = g_best_obj_fn_val * 10000
sendpair(2) = myid

senddata(1) = tmp_g_best_obj_fn_val*10000
int_old_best_obj_fn_val = old_g_best_obj_fn_val * 10000

call MPI_ALLREDUCE(sendpair,recv,1,MPI_2INTEGER,MPI_MAXLOC,newcomm,ierr)
call MPI_ALLREDUCE(senddata,recvdata,1,MPI_INTEGER,MPI_MAX,newcomm,ierr)

!write(*,*) "myid,recv:",myid,recv,'popid',popid,newmyid,int_old_best_obj_fn_val
best_sim_newmyid = floor(recv(2)*1.0/population_rank)
!write(*,*) "best_myid:---->",recv(2),"myid",myid,"newmyid",newmyid,best_sim_newmyid,files_need_modi

if(newmyid == 0) then

    call date_and_time(b(1), b(2), b(3), date_time2)
    inter_time = date_time2(7)-date_time1(7) + 60*(date_time2(6)-date_time1(6)) &
            + 60*60*(date_time2(5)-date_time1(5)) + 60*60*60*(date_time2(4)-date_time1(4))&
            + 60*60*60*12*(date_time2(3)-date_time1(3))
    write(*,"(' <<<<<<<<<<<遗传算法进化代数:',I4,' 粒子群算法迭代数:',I4,' 种群编号:',&
        I4,' 最大目标函数值:',F8.4,'>>>>>>',' 耗时:',I6,'s<<<<<<<<<< by process ',I4)") &
        recur_num,pso_recur_num,popid,recvdata(1)/10000.0,inter_time,myid
end if

if(recv(1)-int_old_best_obj_fn_val /= 0) then
index = 0
best_sim_myid = recv(2)
if(myid==recv(2)) then


!广播种群最优参数信息
    do i=1,para_rank
        do j=1,g_best_arr2(i)%size
            g_best_place_vals_arr(index+j) = g_best_arr2(i)%p(j)
        end do
        index = index + g_best_arr2(i)%size
    end do
end if

g_best_obj_fn_val = recv(1) / 10000.0

call MPI_BCAST(g_best_place_vals_arr&
                ,files_need_modi,MPI_REAL,best_sim_newmyid,newcomm,ierr)

if(myid /= recv(2)) then

!接受种群最优参数信息
    do i=1,para_rank
        do j=1,g_best_arr2(i)%size
            g_best_arr2(i)%p(j) = g_best_place_vals_arr(index+j) 
        end do
        index = index + g_best_arr2(i)%size
    end do
end if
end if

end subroutine pso_communication_to_get_best_values
