subroutine read_simulated(fileid,filename,simu_data_arr)
use mymodule

integer,intent(in) :: fileid
character(len=*),intent(in) :: filename
real,dimension(observed_data_rank),intent(inout) :: simu_data_arr

integer :: year,month,tmp_j = 1
real :: tmp_val
integer :: open_err = 0,read_err = 0
character(len=50) :: iomsg

! write(*,*) simu_data_arr
! write(*,*) fileid , filename

open(unit=fileid,file=filename,status='old',action='read',iostat=open_err,iomsg=iomsg)!打开文件
if(open_err /= 0) then
    write(*,*) iomsg
	stop
end if 

read(fileid,600,iostat=read_err,iomsg=iomsg)
if(read_err /= 0) then
    write(*,*) iomsg
	stop
end if 

do i=1,12*(end_simu_year-begin_simu_year+1)
	read(fileid,*,iostat=read_err,iomsg=iomsg) year,month,tmp_val
	if(read_err /= 0) then
    	write(*,*) iomsg
		stop
	end if

	if((year == observed_data_year_arr(tmp_j)) .and. (month == observed_data_month_arr(tmp_j))) then
		simu_data_arr(tmp_j) = tmp_val
		tmp_val = tmp_val + 1
	end if
	
end do

close(fileid)

600 format (///) 
end subroutine read_simulated
