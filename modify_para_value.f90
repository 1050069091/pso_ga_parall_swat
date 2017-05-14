subroutine modify_para_value(values,colnums,size,filename,fileid,nums)
use mymodule

integer,intent(in) :: size,fileid
real,dimension(size),intent(in) :: values
integer,dimension(size),intent(in) :: colnums,nums
character(len=*),intent(in) :: filename
integer :: open_err = 0,read_err = 0,write_err = 0;
character(len=100) :: iomsg

character(len=1024) :: buff_str
integer :: tmp_int = 0
integer :: write_fileid

character(len=1) :: str_para_id
character(len=MAX_PARA_NAME_LEN),dimension(size) :: para_names 

! write(*,*) size
! write(*,*) values
! write(*,*) colnums
! write(*,*) filename
! write(*,*) "------------------------------------------------"

open(unit=fileid,file=trim(filename),status='old',action='read',iostat=open_err,iomsg=iomsg)!打开老文件
if(open_err /= 0) then
    write(*,*) iomsg
	stop
end if

write_fileid = fileid + seed_rank + 6
open(unit=write_fileid,file=trim(filename)//'.bak',status='replace',action='write',iostat=open_err,iomsg=iomsg)!创建新文件文件
if(open_err /= 0) then
    write(*,*) iomsg
	stop
end if  

tmp_int = 0
outter: do 

	read(fileid,"(A1024)",iostat=read_err,iomsg=iomsg) buff_str
	! write(*,*) trim(buff_str)
	if(read_err < 0) exit outter
	! write(*,*) trim(buff_str)

    tmp_int = tmp_int + 1

    ! write(*,*) tmp_int
    ! if (tmp_int == 1) then
    ! 	write(*,*) len(buff_str),len_trim(trim(buff_str))
    ! end if
    
	inner: do i=1,size

		if(colnums(i) == tmp_int) then
			! write(*,*) digit_rank(i)
			! call num2str(digit_rank(i),str_para_id)
			! write(*,*) str_para_id
			! write(*,*) "(F16."//achar(digit_rank(i)+48)//",A)"
			write(write_fileid,"(F16."//achar(digit_rank(nums(i))+48)//",A)",iostat=write_err,iomsg=iomsg) values(i),trim(buff_str(17:1024))
			! write(*,"(I4,F16.4,A)") tmp_int,values(i),trim(buff_str(17:1024))
			! write(*,*) colnums(i),tmp_int
			if(write_err /= 0) then
				write(*,*) iomsg
				stop
			end if
			cycle outter
		end if
	end do inner

	write(write_fileid,"(A)",iostat=write_err,iomsg=iomsg) trim(buff_str)
	if(read_err + write_err /= 0) then
		write(*,*) iomsg
		stop
	end if
end do outter

! write(*,*) 'modify_para_value:',write_fileid,fileid
close(write_fileid)
close(fileid)

call system('mv -f '//trim(filename)//'.bak '//trim(filename))
!write(*,'(1X,"in file ",A,":")') trim(filename)
!do i=1,size
!    para_names(i) = para_name_arr(nums(i))
!end do
!write(*,*) para_names
!write(*,*) values

end subroutine modify_para_value
