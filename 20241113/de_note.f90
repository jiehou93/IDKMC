    subroutine de_note(dummy_string)
    !提取字符串中的有效字符
    !将dummy_string左对齐，并去除注释符号#及其后面的内容

    implicit none
    integer*4 eff_length,note_location,i
    character*300 dummy_string
                                
    do i=1,len_trim(dummy_string)
        if(ichar(dummy_string(i:i))==9)then
            !trim不清除制表符，将制表符转为空格
            dummy_string(i:i)=' '
        endif
    enddo
    dummy_string=trim(adjustl(dummy_string))  !清除尾部空格，并左对齐
    
    note_location=index(dummy_string,'#')                                       
    if(note_location==0)then                                                    !该行无注释
        eff_length=len_trim(dummy_string)
    else                                                                        !判断注释位置,确定有效内容长度
        eff_length=len_trim(dummy_string(1:note_location-1))
    endif

    dummy_string=dummy_string(1:eff_length)
    end subroutine de_note

