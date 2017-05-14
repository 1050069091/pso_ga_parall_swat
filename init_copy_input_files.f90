subroutine init_copy_input_files()
use mymodule

character(len=1024) :: shell_get_input_file_name = 'ls | grep'
integer :: trimlen,trimlen1,iostat
character(len=50) :: iomsg
character(len=20) :: input_file_name

! write(*,*) trim(shell_get_input_file_name)
!write(*,*) is_use_befor,('cp -f ./out/* ./'),('rm -f ./best_sim_data')
!if(is_use_befor == 1) then
!    call system('cp -f ./out/* ./')
!    call system('rm -f best_sim_data.txt')
!end if

open(unit=myid+10,file='input_file_names'//myid_str//'.txt',status='old',action='read',iostat=iostat,iomsg=iomsg)
if(iostat /= 0) then
	do i=1,file_kind_rank
	trimlen = len_trim(shell_get_input_file_name)
	trimlen1 = len_trim(detail_each_file_include_para_arr(i)%postfile_name)
	shell_get_input_file_name(trimlen+1:trimlen+1+trimlen1+4+1)  = ' -e ' &
								// trim(detail_each_file_include_para_arr(i)%postfile_name) // '$'
	end do
	trimlen = len_trim(shell_get_input_file_name)
	shell_get_input_file_name(trimlen+1:trimlen+1+19+5+4) = ' > input_file_names'//myid_str//'.txt'

	call system(trim(shell_get_input_file_name))
	
	open(unit=myid+10,file='input_file_names'//myid_str//'.txt',status='old',action='read',iostat=iostat,iomsg=iomsg)
	if(iostat /= 0) then
		write(*,*) iomsg
		stop
	end if
end if

do
	read(myid+10,*,iostat=iostat,iomsg=iomsg) input_file_name
	! write(*,*) input_file_name
	call system('cp -f '//trim(input_file_name)//' '//trim(input_file_name)//'.'//trim(myid_str))
	if(iostat < 0) then
		exit
	end if
	if(iostat > 0) then
		write(*,*) trim(iomsg)
		stop
	end if
end do

close(myid+10)
! call system('rm -f input_file_names'//str_myid//'.txt')


end subroutine init_copy_input_files
