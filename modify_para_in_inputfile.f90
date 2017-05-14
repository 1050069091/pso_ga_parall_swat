subroutine modify_para_in_inputfile(seed_num)
use mymodule
integer, intent(in) :: seed_num

character(len=5) :: file_sub_name
character(len=4) :: file_hru_name
character(len=25) :: para_file_name


integer :: tmp_int
real,dimension(:),pointer :: tmp_value_arr
! integer,dimension(:),pointer :: tmp_col_arr

integer :: allocate_err1 = 0,allocate_err2 = 0

!修改文件
do i = 1,substream_rank
	call format_para_fn(5,i,file_sub_name)
	! tmp_int = 1
	do j = 1,file_kind_rank

		allocate(tmp_value_arr(detail_each_file_include_para_arr(j)%size),stat=allocate_err1)
		! allocate(tmp_col_arr(detail_each_file_include_para_arr(j)%size),stat=allocate_err2)
		if(allocate_err1 /= 0) then
    		write(*,*) 'allocate error'
			stop
		end if

		if(detail_each_file_include_para_arr(j)%sub_or_hru == 0) then
			file_hru_name = '0000'
			para_file_name = file_sub_name // file_hru_name // &
                trim(detail_each_file_include_para_arr(j)%postfile_name)//'.'//trim(myid_str)
			do n=1,detail_each_file_include_para_arr(j)%size
				tmp_value_arr(n) = para_start_value_3arr(seed_num,detail_each_file_include_para_arr(j)%p(n))%p(i)
			end do

			call modify_para_value(tmp_value_arr,detail_each_file_include_para_arr(j)%p_col_arr,&
									detail_each_file_include_para_arr(j)%size,trim(para_file_name),seed_num+6,&
									detail_each_file_include_para_arr(j)%p)
		else
			do k=1,sub_hru_info_arr(i)
    			call format_para_fn(4,k,file_hru_name)
			    para_file_name = file_sub_name // file_hru_name // &
                    trim(detail_each_file_include_para_arr(j)%postfile_name)//'.'//trim(myid_str)
                !write(*,*) para_file_name,detail_each_file_include_para_arr(j)%size,'  end'
    			do n=1,detail_each_file_include_para_arr(j)%size
                    !write(*,*)'hello world:51',detail_each_file_include_para_arr(j)%p(n),add_sub_hru_info_arr(i)
					tmp_value_arr(n) = para_start_value_3arr&
										(seed_num,detail_each_file_include_para_arr(j)%p(n))%p(add_sub_hru_info_arr(i)+k)
				end do

				call modify_para_value(tmp_value_arr,detail_each_file_include_para_arr(j)%p_col_arr,&
									detail_each_file_include_para_arr(j)%size,trim(para_file_name),seed_num+6,&
									detail_each_file_include_para_arr(j)%p)
    			! write(*,*) para_file_name
    			tmp_int = tmp_int + 1
    		end do
		end if
		deallocate(tmp_value_arr)
	end do
	! write(*,*) "********************************************"
end do


end subroutine modify_para_in_inputfile
