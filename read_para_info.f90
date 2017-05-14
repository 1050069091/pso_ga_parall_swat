subroutine read_para_info
use mymodule

!1.预处理
!在文件par_inf.txt中读出如下信息：
!递归次数recur_rank，
!种子个数（每次递归模拟次数）seed_rank,
!需要模拟的参数个数para_rank,各参数最大最小值
integer :: par_inf_open_err = 0,par_inf_read_err = 0,allocate_err = 0,allocate1_err = 0,&
allocate2_err = 0,allocate3_err = 0,allocate4_err = 0,allocate5_err = 0,allocate6_err = 0,allocate7_err = 0,&
allocate8_err = 0,allocate9_err = 0,allocate10_err = 0
character(len=50) :: iomsg

integer,dimension(:),pointer :: tmp_int_arr
integer,dimension(:),pointer :: tmp_int_col_arr
integer :: tmp_int
! character(len=MAX_FILE_POSTFIX_NAME_LEN) :: tmp_str
! integer :: tmp_int

!par_inf.txt中参数信息数组
!character(len=MAX_PARA_INFO_LEN), allocatable, dimension(:) :: para_info_arr

open(unit=parinf_fid,file=par_inf_fn,status='old',action='read',iostat=par_inf_open_err,iomsg=iomsg)!打开文件
if(par_inf_read_err /= 0) then
    write(*,*) iomsg
	stop
end if 

read(parinf_fid,100,iostat=par_inf_read_err,iomsg=iomsg) &
	population_rank,seed_rank_in_one_pop,uncertainty_rate,recur_rank,speed_rate_spo,&
	first_speed_factor,second_speed_factor,inertia_factor_spo,ga_recur_rank,&
	intersect_rate,variation_rate,is_use_befor,para_rank

if(par_inf_read_err /= 0) then
    write(*,*) iomsg
	stop
end if 

seed_rank = population_rank * seed_rank_in_one_pop

! write(*,*) "par_inf.txt:",&
! 	recur_rank,seed_rank,obj_fn_threshold,speed_rate_spo,&
! 	first_speed_factor,second_speed_factor,inertia_factor_spo,is_use_befor,para_rank

!动态分配内存
allocate(para_name_arr(para_rank),stat=allocate2_err)
allocate(para_low_nu_arr(para_rank),stat=allocate3_err)
allocate(max_para_arr(para_rank),stat=allocate4_err)
allocate(min_para_arr(para_rank),stat=allocate5_err)
allocate(para_file_postfix_arr(para_rank),stat=allocate6_err)
allocate(sub_or_hru_arr(para_rank),stat=allocate7_err)
allocate(digit_rank(para_rank),stat=allocate10_err)

if(allocate2_err+allocate3_err+allocate4_err+allocate5_err+allocate6_err+allocate7_err+allocate10_err /= 0) then
    write(*,*) 'allocate error'
	stop
end if 


!读取参数信息
do i=1,para_rank
	read(parinf_fid,*,iostat=par_inf_read_err,iomsg=iomsg) &
	para_name_arr(i),para_file_postfix_arr(i),para_low_nu_arr(i),min_para_arr(i),max_para_arr(i),digit_rank(i)
	if(par_inf_read_err /= 0) then
    	write(*,*) iomsg
		stop
	end if
end do



read(parinf_fid,'(/,I4,/)',iostat=par_inf_read_err,iomsg=iomsg) file_kind_rank

if(par_inf_read_err /= 0) then
    	write(*,*) iomsg
		stop
end if

allocate(detail_each_file_include_para_arr(file_kind_rank),stat=allocate8_err)

if(allocate8_err /= 0) then
    write(*,*) 'allocate error'
	stop
end if


do i=1,file_kind_rank
	read(parinf_fid,*,iostat=par_inf_read_err,iomsg=iomsg) detail_each_file_include_para_arr(i)%postfile_name,&
														   detail_each_file_include_para_arr(i)%sub_or_hru,&
					                                       detail_each_file_include_para_arr(i)%size
	if(par_inf_read_err /= 0) then
    	write(*,*) iomsg
		stop
	end if
	! write(*,*) detail_each_file_include_para_arr(i)%postfile_name,&
	! 		   detail_each_file_include_para_arr(i)%sub_or_hru,&
	! 		   detail_each_file_include_para_arr(i)%size
end do

! write(*,*) 'parinf_fid: ',parinf_fid
close(parinf_fid)!关闭文件
!write(*,100) recur_rank,seed_rank,obj_fn_threshold,is_use_befor,para_rank

do i=1,file_kind_rank
    tmp_int = 1
	allocate(tmp_int_arr(detail_each_file_include_para_arr(i)%size),stat=allocate1_err)
	allocate(tmp_int_col_arr(detail_each_file_include_para_arr(i)%size),stat=allocate2_err)
	if(allocate1_err + allocate2_err /= 0) then
    	write(*,*) 'allocate error'
		stop
	end if

	do j=1,para_rank
		! do k=1,detail_each_file_include_para_arr(i)%size
			! write(*,*) trim(detail_each_file_include_para_arr(i)%postfile_name),trim(para_file_postfix_arr(j)) 
			if(trim(detail_each_file_include_para_arr(i)%postfile_name) == trim(para_file_postfix_arr(j))) then
				tmp_int_arr(tmp_int) = j
				tmp_int_col_arr(tmp_int) = para_low_nu_arr(j)
				sub_or_hru_arr(j) = detail_each_file_include_para_arr(i)%sub_or_hru
				tmp_int = tmp_int + 1
			end if
		! end do
	end do
	detail_each_file_include_para_arr(i)%p => tmp_int_arr
	detail_each_file_include_para_arr(i)%p_col_arr => tmp_int_col_arr
	! write(*,*) sub_or_hru_arr
end do


!读取数据的模式
100 format (I4,/,I4,/,F4.2,//,I4,/,F4.2,/,F4.2,/,F4.2,/,F4.2,//,I4,/,F4.2,/,F4.2,//,I2,//,I4,/)

end subroutine read_para_info
