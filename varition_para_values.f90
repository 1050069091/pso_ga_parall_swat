!变异操作
subroutine varition_para_values(real_para_values_arr,arr_size,cur_num)
use mymodule
implicit none

integer,intent(in) :: arr_size,cur_num
real,intent(out) :: real_para_values_arr(arr_size)

integer :: count_ga,varition_para_index,para_index,sum_count,i,j
real :: my_varition_rate_in_file,random_real_val,tmp_real

my_varition_rate_in_file = variation_rate * (1-cur_num/(1.5*recur_rank))

call random_number(random_real_val)
if(random_real_val <= my_varition_rate_in_file) then
    
    count_ga = arr_size * my_varition_rate_in_file

    outter:do i=1,count_ga
        call random_number(random_real_val)
        varition_para_index = ceiling(arr_size * random_real_val)
        sum_count = 0
        inner:do j=1,para_rank
            sum_count = sum_count + files_included_by_each_para_arr(j)
            if(varition_para_index<=sum_count) then
                para_index = j
                exit inner
            end if

        end do inner

        call random_number(random_real_val)
        tmp_real = real_para_values_arr(varition_para_index) +&
            random_real_val*my_varition_rate_in_file*&
            (max_para_arr(para_index)-min_para_arr(para_index))

        if(tmp_real >= min_para_arr(para_index)&
            .and.tmp_real<=max_para_arr(para_index)) then
            real_para_values_arr(varition_para_index) = tmp_real
        end if

    end do outter

end if
    
end subroutine varition_para_values
