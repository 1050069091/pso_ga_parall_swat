module mymodule

!use iso_fortran_env , only : int64 !// 使用内部函数获得int64的Kind值
!integer :: int64 = 8

!------start---定义一些常量-------------------------------
!参数名的最大长度
integer, parameter :: MAX_PARA_NAME_LEN = 20
!文件后缀名的最大长度
integer, parameter :: MAX_FILE_POSTFIX_NAME_LEN = 8
!参数信息总最大长度
!integer, parameter :: MAX_PARA_INFO_LEN = 30
!------end---定义一些常量-------------------------------
!----------------------------------------------------------------------------------------------------------------------

!------start---自定义一些类型-------------------------------
!一维实数数组指针
type :: real_arr_p
    integer :: size
	real,dimension(:),pointer :: p
end type
!二维实数数组指针
type :: real_2arr_p
	real,dimension(:,:),pointer :: p
end type
!一维整型数组指针
type :: diy_int_p_arr
	integer :: sub_or_hru
    character(len=MAX_FILE_POSTFIX_NAME_LEN) :: postfile_name
    integer :: size
	integer,dimension(:),pointer :: p
	integer,dimension(:),pointer :: p_col_arr
end type

!------end---自定义一些类型-------------------------------

!进程信息
integer :: myid,numprocs,interval,my_interval,newinterval,&
    new_max_interval,mycomm,newcomm,newmyid,newprocsnum
character(len=5) :: myid_str

integer :: best_sim_myid,best_sim_recur_id,best_sim_seed_id

integer :: g_best_int_obj_fn_val = -999999999
character(len=5) :: best_sim_myid_str=''

integer date_time1(8),date_time2(8)
character*10 a(3),b(3)

!------start---------par_inf.txt中的参数值---------------------------------------------
!par_inf.txt文件打开的id号和路径名
integer :: parinf_fid=8
character(len=14) :: par_inf_fn='in/par_inf.txt'

!pso递归次数,总种子个数
integer :: recur_rank=0,seed_rank=0,ga_recur_rank=0,population_rank=0,seed_rank_in_one_pop=0

real :: uncertainty_rate
!是否使用上次的参数值，0:不使用；1:使用
integer :: is_use_befor = 0

!spo 方程中的一系列可变参数
real :: speed_rate_spo,first_speed_factor,second_speed_factor,inertia_factor_spo,first_random_spo,second_random_spo
!ga 算法的参数
real :: intersect_rate,variation_rate

!需要模拟的参数个数
integer ::para_rank=0
!各参数的名字，所在文件的后缀名，及其行号
character(len=MAX_PARA_NAME_LEN), allocatable, dimension(:) :: para_name_arr
character(len=MAX_FILE_POSTFIX_NAME_LEN), allocatable, dimension(:) :: para_file_postfix_arr
integer, allocatable, dimension(:) :: para_low_nu_arr
!该参数文件是每sub一个还是每hru一个
integer, allocatable, dimension(:) :: sub_or_hru_arr
!各参数的最大和最小值
real, allocatable, dimension(:) :: max_para_arr
real, allocatable, dimension(:) :: min_para_arr

integer, allocatable, dimension(:) :: digit_rank

!需要修改的包含这些参数的文件种类数
integer :: file_kind_rank
!各参数的所在文件的后缀名
! character(len=MAX_FILE_POSTFIX_NAME_LEN), allocatable, dimension(:) :: each_para_file_postfix_arr
!每个文件种类中包含的需要率定的参数的个数
! integer,allocatable,dimension(:) :: file_kind_para_rank_arr
!每个文件种类中包含的需要率定的参数的编号
type(diy_int_p_arr),allocatable,dimension(:) :: detail_each_file_include_para_arr

integer,allocatable,dimension(:) :: files_included_by_each_para_arr
character(len=400) :: swat_input_file_postfix_names = ''
!-------end--------par_inf.txt中的参数值------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------


!------start---------extract_inf.txt中的参数值---------------------------------------------
integer :: extractinf_fid = 9
character(len=18) :: extract_inf_fn='in/extract_inf.txt'

!swat 模拟结果输出文件名
character(len=20) :: simu_result_fn
!观测值的名字
character(len=20) :: obser_val_name
!观测值在输出文件中的列数
integer :: obser_col_num = 0

!总的HRU数
integer :: hru_rank = 0
!总的子流域数
integer :: substream_rank = 0
!观测该模拟量的子流域编号
integer :: val_substream_num_arr = 0;
! !获得该观测量的子流域编号
! integer, allocatable, dimension(:) :: val_substream_nums_arr

!模拟的开始年份和结束年份,和率定步长（1=monthly, 2=yearly）
integer :: begin_simu_year,end_simu_year,time_step
!-------end--------extract_inf.txt中的参数值------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------

!-------start--------observed.txt中的参数值------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------
integer :: observed_fid = 12
character(len=15) :: observed_fn='in/observed.txt'

!Objective function type, 1=r2,2=NS
integer :: fn_type = 1
!when this values of obj fn is reached, process terminates
!目标函数的预期值
real :: obj_fn_threshold != 0.9
!real :: fn_object_val
!number of data points 
integer :: observed_data_rank
!data points
real,allocatable,dimension(:) :: observed_data_val_arr
integer,allocatable,dimension(:) :: observed_data_year_arr
integer,allocatable,dimension(:) :: observed_data_month_arr

!-------end--------observed.txt中的参数值------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------

!-------start--------根据种子数初始化各参数值------------------------------------------------
type(real_arr_p), allocatable, dimension(:,:) :: para_start_value_3arr
type(real_arr_p), allocatable, dimension(:,:) :: para_start_speed_3arr

type(real_arr_p), allocatable, dimension(:,:) :: p_best_arr3
type(real_arr_p), allocatable, dimension(:) :: g_best_arr2
real,allocatable,dimension(:) :: p_best_obj_fn_val_arr
real :: g_best_obj_fn_val = -99999999.0
real :: tmp_g_best_obj_fn_val = -99999999.0
real :: old_g_best_obj_fn_val = -99999999.0
real,allocatable,dimension(:) :: g_best_place_vals_arr
integer :: files_need_modi

real, allocatable, dimension(:,:) :: para_values_2arr_in_one_process
real,allocatable,dimension(:) :: obj_fn_valuses_in_one_process
integer,allocatable,dimension(:) :: seed_nums_in_one_process
!-------end--------根据种子数初始化各参数值------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------


!-------start--------sub_hru_info.txt中的hru信息------------------------------------------------
integer :: sub_hru_inf_fid = 10
character(len=20) :: sub_hru_inf_fn = "in/sub_hru_info.txt"
integer,allocatable,dimension(:) :: sub_hru_info_arr
integer,allocatable,dimension(:) :: add_sub_hru_info_arr
!-------end--------sub_hru_info.txt中的hru信息------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------

!----start----工具函数------------------------------------------------------------------------
!!!!!组织参数文件名函数
contains
subroutine format_para_fn(len,num,str)

	integer,intent(in) :: len,num
	character(len=len),intent(inout) :: str
	integer :: tmp_num , tmp

	if (num >= 10**len) then
		write(*,*) "error:num >= ",10**len
		stop
	end if

	! write(*,*) 'hello'
	! call num2str(num,str,len)
	tmp_num = num
	do i=1,len
    	tmp = tmp_num / 10**(len-i)
		str(i:i) = achar(tmp+48)
		! write(*,*) str
		tmp_num = tmp_num - tmp*(10**(len-i))
	end do

end subroutine format_para_fn

!整数转化为字符串
subroutine num2str(num,str)

	integer,intent(in) :: num
	character(len=*),intent(inout) :: str
	integer :: tmp_int = 1

	do
		if(num / 10**tmp_int == 0) then
			exit
		end if
		tmp_int = tmp_int + 1
	end do

	call format_para_fn(tmp_int,num,str)

end subroutine num2str

!在读文件时，跳过行
subroutine skim_rows_in_file(num,fileid,read_err,iomsg)
	integer,intent(in) :: num,fileid
	integer,intent(inout) :: read_err
	character(len=*) :: iomsg
	character(len=num+2) :: format_str

	format_str(1:1) = '('
	do i=2,num+1
		format_str(i:i) = '/'
	end do
	format_str(num+2:num+2) = ')'

	! write(*,*) format_str

	read(fileid,format_str,iostat=read_err,iomsg=iomsg)

end subroutine skim_rows_in_file

!初始化种子
subroutine init_random_seed(myid)
    integer,intent(in):: myid
    integer :: i, n, clock
    integer, dimension(:), allocatable :: seed
   
    call random_seed(size = n)
    allocate(seed(n))
   
    call system_clock(count=clock)
   
    seed = clock + 37 * (/ (i - 1, i = 1, n) /) + myid ** 4
    call random_seed(put = seed)
   
    deallocate(seed)
end subroutine init_random_seed
! subroutine init_random_seed()    
!    	integer :: ised , i , pid
!    	integer :: t 
!    	integer , allocatable :: sed(:)
!    	call random_seed( size = ised ) !// 获得种子大小
!    	allocate( sed(ised) ) !// 分配种子
!    	call system_clock(t) !// 获得时间
!    	pid = getpid() !// 获得处理器ID
!    	t = ieor(t, int(pid, kind(t))) !// 用 pid 和日期做XOR运算
!    	do i = 1, ised
!        	sed(i) = lcg(t) !// 用线性同余计算种子
!    	end do
!    	call random_seed( put=sed ) !// 给定种子  
! end subroutine Init_Random_Seed

! function lcg(s) !// 线性同余算法 Linear congruential generator
!    integer :: lcg
!    integer :: s
!    if (s == 0) then
!       s = 104729
!    else
!       s = mod(s, 67296)
!    end if
!    s = mod(s * 70273, 67291)
!    lcg = s
! end function lcg

!----end-------工具函数-----------------------------------------------------------------------
recursive subroutine quick_sort(int_arr_obj_fn,seed_nums_arr,low,high)
integer,intent(in) :: low,high
integer,intent(inout) :: int_arr_obj_fn(high-low+1),seed_nums_arr(high-low+1)

integer ::  i,j,p,x,tmp

if(low < high) then
    x = int_arr_obj_fn(low)
    tmp = seed_nums_arr(low)
    i = low
    j = high
    outter:do 
        if(i==j) then
            int_arr_obj_fn(i) = x 
            seed_nums_arr(i) = tmp
            exit outter
        end if
        inner:do 
            if(j<=i.or.int_arr_obj_fn(j)<x) exit inner
            j = j-1
        end do inner

        int_arr_obj_fn(i) = int_arr_obj_fn(j)
        seed_nums_arr(i) = seed_nums_arr(j)

        inner1:do
            if(i>=j.or.int_arr_obj_fn(i)>x) exit inner1
            i = i+1
        end do inner1

        int_arr_obj_fn(j) = int_arr_obj_fn(i)
        seed_nums_arr(j) = seed_nums_arr(i)

        
    end do outter

    call quick_sort(int_arr_obj_fn,seed_nums_arr,low,i-1)
    call quick_sort(int_arr_obj_fn,seed_nums_arr,i+1,high)

end if

end subroutine quick_sort


subroutine find_index_choosed(val,arr,size,index_found)
    real,intent(in) :: val
    integer,intent(in) :: size
    integer,intent(inout) :: index_found
    real,intent(in) :: arr(size)

    index_found = 1
    do i=2,size
        if(val>arr(i-1) .and. val<=arr(i)) then
            index_found = i
            exit
        end if
    end do
end subroutine find_index_choosed

end module mymodule
