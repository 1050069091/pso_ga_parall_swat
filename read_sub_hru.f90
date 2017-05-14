subroutine read_sub_hru
use mymodule

integer :: sub_hru_inf_open_err = 0,sub_hru_inf_read_err = 0,allocate_err = 0,allocate_err1 = 0

open(unit=sub_hru_inf_fid,file=sub_hru_inf_fn,status='old',action='read',iostat=sub_hru_inf_open_err)!打开文件
if(sub_hru_inf_open_err /= 0) then
    write(*,*) 'open sub_hru_info.txt error'
	stop
end if 

allocate(sub_hru_info_arr(substream_rank),stat=allocate_err)
allocate(add_sub_hru_info_arr(substream_rank),stat=allocate_err1)

if(allocate_err + allocate_err1 /= 0) then
    write(*,*) 'allocate sub_hru_info_arr error'
	stop
end if


read(sub_hru_inf_fid,*,iostat=sub_hru_inf_read_err) !跳过题头
if(sub_hru_inf_read_err /= 0) then
    write(*,*) 'read sub_hru_info.txt error'
	stop
end if

do i=1,substream_rank
	read(sub_hru_inf_fid,300,iostat=sub_hru_inf_read_err) sub_hru_info_arr(i)
	if(sub_hru_inf_read_err /= 0) then
    	write(*,*) 'read sub_hru_info.txt error'
		stop
	end if
	if(i == 1) then
		add_sub_hru_info_arr(i) = 0
	else
		add_sub_hru_info_arr(i) = add_sub_hru_info_arr(i-1) + sub_hru_info_arr(i)
	end if
end do

! write(*,*) 'sub_hru_inf_fid:',sub_hru_inf_fid
close(sub_hru_inf_fid)

300 format (t8,I8)

end subroutine read_sub_hru
