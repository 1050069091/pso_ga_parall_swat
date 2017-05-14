subroutine read_extract_info
use mymodule
integer :: extr_inf_open_err = 0,extr_inf_read_err = 0,allocate_err = 0
!character(len=15) str_read_format

open(unit=extractinf_fid,file=extract_inf_fn,status='old',action='read',iostat=extr_inf_open_err)!打开文件
if(extr_inf_open_err /= 0) then
    write(*,*) 'open extract_inf.txt error'
	stop
end if 

read(extractinf_fid,200,iostat=extr_inf_read_err) &
simu_result_fn,obser_val_name,obser_col_num,hru_rank,substream_rank,val_substream_num_arr
if(extr_inf_read_err /= 0) then
    write(*,*) 'read extract_inf.txt error'
	stop
end if 

read(extractinf_fid,201,iostat=extr_inf_read_err) begin_simu_year,end_simu_year,time_step
if(extr_inf_read_err /= 0) then
    write(*,*) 'read extract_inf.txt error'
	stop
end if

! write(*,*) 'extractinf_fid:',extractinf_fid
!关闭文件
close(extractinf_fid)

!读取数据的模式
200 format (/,A,//,A,//,I4,///,I4,//,I4,//,I4)
201 format (//,I5,//,I5,//,I2)

end subroutine read_extract_info
