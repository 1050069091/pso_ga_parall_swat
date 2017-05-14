subroutine  post_calibration_handle
use mymodule
use mpi

integer :: ierr,inter_time
!integer date_time1(8),date_time2(8)
!character*10 a(3),b(3)

call system('rm -f input_file_names'//myid_str//'.txt')
call system('rm -f `ls | grep "'//myid_str//'$"`')

!call system('rm -f '//trim(obser_val_name)//myid_str//'.txt')
call system('rm -f out/'//trim(obser_val_name)//myid_str//'.txt')

!call system('find ./out -name "*.'//myid_str//'" | awk -F "." '//"'"&
!    //'{print "."$2"'//'."$3}'//"' | xargs -i mv -f {}."//myid_str//" {}")
!write(*,*) ('find ./out -name "*.'//myid_str//'" | awk -F "." '//"'"&
!    //'{print "."$2"'//'."$3}'//"' | xargs -i mv -f {}."//myid_str//" {}")

if(myid == numprocs-1) then

    call system('find ./out -name "*.'//myid_str//'" | awk -F "." '//"'"&
        //'{print "."$2"'//'."$3}'//"' | xargs -i mv -f {}."//myid_str//" {}")
    call system('cp -f out/best_sim_data.txt'//trim(best_sim_myid_str)//' out/best_sim_data.txt')
    g_best_obj_fn_val = (g_best_int_obj_fn_val)/10000.0
    call date_and_time(b(1), b(2), b(3), date_time2)
    inter_time = date_time2(7)-date_time1(7) + 60*(date_time2(6)-date_time1(6)) &
            + 60*60*(date_time2(5)-date_time1(5)) + 60*60*60*(date_time2(4)-date_time1(4))&
            + 60*60*60*12*(date_time2(3)-date_time1(3))
    write(*,'(/)') 
    write(*,*) '*************************************************************'
    write(*,*) '*************多级并行粒子群和遗传算法率定swat参数**********************'
    write(*,'(/)') 
    write(*,'(" 开始时间: ",A21,"   结束时间: ",A21,"    历时:",I5,A)') &
            (a(1)(1:8)//' '//a(2)(1:2)//':'//a(2)(3:4)//':'//a(2)(5:6)//':'//a(2)(8:10)),&
            (b(1)(1:8)//' '//b(2)(1:2)//':'//b(2)(3:4)//':'//b(2)(5:6)//':'//b(2)(8:10)),inter_time,'s'
    write(*,'(/)') 
    write(*,'(" 率定的参数个数:",I3," 迭代次数:",I5,"  种子个数:",I5,"  进程数:",I5)') &
            para_rank,recur_rank,seed_rank,numprocs
    write(*,'(/)') 
    write(*,'(" the best simulate: the recur_num =",I3,"    the seed_num =",I3)') &
            best_sim_recur_id,best_sim_seed_id
    write(*,'(/)') 
    if(g_best_obj_fn_val < obj_fn_threshold) then
        write(*,'(" best object function value(",F8.4,") not arrive the threshold(",F4.2,")")') &
                g_best_obj_fn_val, obj_fn_threshold
    else    
        write(*,'(" best object function value(",F8.4,") arrive the threshold(",F4.2,")")') &
                g_best_obj_fn_val, obj_fn_threshold
    end if
    write(*,'(/)') 
    write(*,*) '*************************************************************'
    write(*,*) '*************************************************************'
    
    !call system('cp -f out/* ./')
    !call system('rm -f best_sim_data.txt*')
!else
    !call system('rm -f `ls ./out | grep "'//myid_str//'$"`')
    !call system('rm -f best_sim_data.txt'//trim(myid_str))
    !call system('rm -f `find ./out -name "*.'//trim(myid_str)//'"`')
end if
    call MPI_BARRIER(mycomm,ierr)
    call system('rm -f `find out -name "*'//trim(myid_str)//'"`')
end subroutine post_calibration_handle
