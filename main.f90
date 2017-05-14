program main
use mymodule
use mpi
implicit none

!并行定义  
integer :: rc,ierr

call MPI_INIT(ierr)
call MPI_COMM_DUP(MPI_COMM_WORLD,mycomm,ierr)
call MPI_COMM_RANK(mycomm,myid,ierr);
call MPI_COMM_SIZE(mycomm,numprocs,ierr);


call pre_calibration_handle()


!读取需要抽取的模拟结果信息
call read_extract_info()
!获得每个sub中含有的hru信息
call read_sub_hru()
!获得观测数据
call read_observed()
!根据进程号，拷贝需要修改的swat文件
call init_copy_input_files()

!并行初始化率定的参数值
call parall_init_para_value()

!并行率定前的准备工作
call parall_pre_calibration()

!write(*,*) myid,numprocs,newmyid,newprocsnum
!开始率定
call calibration_pso_ga()

call post_calibration_handle()

call MPI_COMM_FREE(newcomm,ierr)
call MPI_COMM_FREE(mycomm,ierr)
call MPI_FINALIZE(rc)

end program main
