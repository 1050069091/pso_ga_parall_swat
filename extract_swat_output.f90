subroutine extract_swat_output(simu_data_arr)
use mymodule

!integer,intent(in) :: seed_num
real,dimension(observed_data_rank),intent(out) :: simu_data_arr

integer :: read_fileid,write_fileid
integer :: open_err = 0,read_err = 0,write_err = 0;
character(len=100) :: iomsg
!character(len=5) :: strnum
integer :: col_in_file
character(len=10) :: col_in_file_str = ''
character(len=1024) :: buff_str
real :: simu_result
integer :: month,year,tmp_j=1

!call format_para_fn(5,seed_num,strnum)

col_in_file = 26 + (obser_col_num-5)*12
call num2str(col_in_file,col_in_file_str)

! write(*,*) trim(col_in_file_str),strnum

!write (*,*) 'swat_in/'//trim(simu_result_fn)
! write (*,*) 'out/'//trim(obser_val_name)//trim(strnum)//'.txt'
read_fileid = myid + 10
! write(*,*) 'hello world1'
open(unit=read_fileid,file=trim(simu_result_fn)//trim(myid_str),status='old',&
								action='read',iostat=open_err,iomsg=iomsg)!打开老文件
if(open_err /= 0) then
    write(*,*) iomsg
	stop
end if
! write(*,*) 'hello world2'
write_fileid = myid + numprocs + 20

open(unit=write_fileid,file='out/'//trim(obser_val_name)//myid_str//'.txt',&
					status='replace',action='write',iostat=open_err,iomsg=iomsg)!创建新文件文件
if(open_err /= 0) then
    write(*,*) iomsg
	stop
end if


write(write_fileid,400,iostat=write_err,iomsg=iomsg) val_substream_num_arr
if(write_err /= 0) then
    write(*,*) iomsg
	stop
end if

!读写指针跳到第一行有数据的行
call skim_rows_in_file(10+val_substream_num_arr-3,read_fileid,read_err,iomsg)
if(read_err /= 0) then
    write(*,*) iomsg
	stop
end if

tmp_j = 1
outter: do i=1,(end_simu_year-begin_simu_year+1)
	year = begin_simu_year+i-1
	inner: do j=1,12
		read(read_fileid,'(t20,I6,t'//trim(col_in_file_str)//',E12.4)',iostat=read_err,iomsg=iomsg) month,simu_result
		if(read_err /= 0) then
    		write(*,*) iomsg
			stop
		end if
		!将模拟结果写入文件
		write(write_fileid,401,iostat=write_err,iomsg=iomsg) year,month,simu_result
		if(write_err /= 0) then
    		write(*,*) iomsg
			stop
		end if
	    !获得模拟结果数据
            !write(*,*) year,observed_data_year_arr(tmp_j),month,observed_data_month_arr(tmp_j),tmp_j
		if((year == observed_data_year_arr(tmp_j)) .and. (month == observed_data_month_arr(tmp_j))) then
			simu_data_arr(tmp_j) = simu_result
            !write(*,*) simu_data_arr(tmp_j)
			tmp_j = tmp_j + 1
		end if

		call skim_rows_in_file(substream_rank-2,read_fileid,read_err,iomsg)
		if(read_err /= 0) then
			if (read_err < 0) then
				exit outter
			end if
   			write(*,*) iomsg
			stop
		end if 
	end do inner
	!跳过年模拟量
	call skim_rows_in_file(substream_rank-1,read_fileid,read_err,iomsg)
	if(read_err /= 0) then
		if (read_err < 0) then
			exit outter
		end if
    	write(*,*) iomsg
		stop
	end if	
end do outter
! write(*,*) write_fileid,read_fileid
!write(*,*) simu_data_arr

close(write_fileid)
close(read_fileid)

400 format ('reach(subbasin) number:',I8,///,'year    month    simu_value')
401 format (I4,I8,E16.4)
end subroutine extract_swat_output
