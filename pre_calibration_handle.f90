subroutine pre_calibration_handle
use mymodule
use mpi
      
integer color,ierr,asend(1),brecv(1)

call date_and_time(a(1), a(2), a(3), date_time1)

!获取要率定的参数信息
call read_para_info()
!如果进程数大于种子数，无并行的必要，报错停止
if(seed_rank < numprocs) then
	write(*,"('error: process_num(',I4,' ) > seed_num(',I4,'  )')") numprocs,seed_rank
	stop 
end if

if(population_rank > numprocs) then
	write(*,"('error: 种群数(',I4,' ) > 进程数(',I4,'  )')") population_rank,numprocs
	stop 
end if

!划分子通信空间
color = mod(myid,population_rank)
call MPI_COMM_SPLIT(mycomm,color,myid,newcomm,ierr)
!call MPI_COMM_GROUP(mycomm,color,ierr)
!call MPI_COMM_CREATE(mycomm,color,newcomm,ierr)
call MPI_COMM_RANK(newcomm,newmyid,ierr) 
call MPI_COMM_SIZE(newcomm,newprocsnum,ierr) 

interval = ceiling(seed_rank / (numprocs*1.0))

! interval = seed_rank / numprocs
! if(seed_rank >  numprocs*interval) then
!     interval = interval + 1
! end if

newinterval = ceiling(seed_rank_in_one_pop / (newprocsnum*1.0))

!write(*,*) "myid:",myid,newinterval,new_max_interval,seed_rank_in_one_pop,newprocsnum

call format_para_fn(len(myid_str),myid+1,myid_str)

asend(1) = newinterval
call MPI_ALLREDUCE(asend,brecv,1,MPI_INTEGER,MPI_MAX,mycomm,ierr)
new_max_interval = brecv(1)

!write(*,*) "myid:",myid,newinterval,new_max_interval

end subroutine pre_calibration_handle
