subroutine read_observed
use mymodule

integer :: open_err = 0,read_err = 0,allocate1_err = 0,allocate2_err = 0,allocate3_err = 0,tmp
character(len=50) :: iomsg

open(unit=observed_fid,file=observed_fn,status='old',action='read',iostat=open_err,iomsg=iomsg)!打开文件
if(open_err /= 0) then
    write(*,*) iomsg
	stop
end if 

read(observed_fid,500,iostat=read_err,iomsg=iomsg) fn_type,obj_fn_threshold,observed_data_rank
if(read_err /= 0) then
    write(*,*) iomsg
	stop
end if 


allocate(observed_data_val_arr(observed_data_rank),stat=allocate1_err)
allocate(observed_data_month_arr(observed_data_rank),stat=allocate2_err)
allocate(observed_data_year_arr(observed_data_rank),stat=allocate3_err)

if(allocate1_err + allocate2_err + allocate3_err /= 0) then
	write(*,*) 'allocate error'
end if

do i=1,observed_data_rank
	read(observed_fid,*,iostat=read_err,iomsg=iomsg) tmp,observed_data_month_arr(i),&
													observed_data_year_arr(i),observed_data_val_arr(i)
	if(read_err /= 0) then
    	write(*,*) iomsg
		stop
	end if 
	! write(*,*) observed_data_month_arr(i),observed_data_year_arr(i),observed_data_val_arr(i)
end do

! write(*,*) 'observed_fid:',observed_fid
close(observed_fid)

500 format (I2,/,F4.2,/,I6,//) 
end subroutine read_observed
