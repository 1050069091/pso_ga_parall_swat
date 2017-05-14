subroutine communication_to_get_best_values()
use mymodule
use mpi
integer sendpair(2),recv(2)
integer ierr,index,int_old_best_obj_fn_val

sendpair(1) = g_best_obj_fn_val * 1000
sendpair(2) = myid

int_old_best_obj_fn_val = old_g_best_obj_fn_val * 1000

call MPI_ALLREDUCE(sendpair,recv,1,MPI_2INTEGER,MPI_MAXLOC,mycomm,ierr)
!write(*,*) "myid,recv:",myid,recv

!write(*,*) "myid:",myid,"recv:",recv(1),'int_old',int_old_best_obj_fn_val
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
        !write(*,*) "myid,index:---->",myid,index,g_best_arr2(i)%size
    end do

    !call system('cp -f out/'//trim(obser_val_name)&
    !                        //myid_str//'.txt out/best_sim_data.txt')
    !call system("cp -f `ls | grep '[.]"//myid_str//"$'` ./out")
    !write(*,*) myid_str
    
end if

g_best_obj_fn_val = recv(1) / 1000.0
!write(*,*) "myid:---->",myid,g_best_place_vals_arr

call MPI_BCAST(g_best_place_vals_arr&
                ,files_need_modi,MPI_REAL,recv(2),mycomm,ierr)

if(myid /= recv(2)) then

!接受种群最优参数信息
    do i=1,para_rank
        do j=1,g_best_arr2(i)%size
            g_best_arr2(i)%p(j) = g_best_place_vals_arr(index+j) 
        end do
        index = index + g_best_arr2(i)%size
    end do
end if
!write(*,*) myid,":post:------->",g_best_place_vals_arr
end if

end subroutine communication_to_get_best_values
