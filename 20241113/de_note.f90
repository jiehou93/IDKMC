    subroutine de_note(dummy_string)
    !��ȡ�ַ����е���Ч�ַ�
    !��dummy_string����룬��ȥ��ע�ͷ���#������������

    implicit none
    integer*4 eff_length,note_location,i
    character*300 dummy_string
                                
    do i=1,len_trim(dummy_string)
        if(ichar(dummy_string(i:i))==9)then
            !trim������Ʊ�������Ʊ��תΪ�ո�
            dummy_string(i:i)=' '
        endif
    enddo
    dummy_string=trim(adjustl(dummy_string))  !���β���ո񣬲������
    
    note_location=index(dummy_string,'#')                                       
    if(note_location==0)then                                                    !������ע��
        eff_length=len_trim(dummy_string)
    else                                                                        !�ж�ע��λ��,ȷ����Ч���ݳ���
        eff_length=len_trim(dummy_string(1:note_location-1))
    endif

    dummy_string=dummy_string(1:eff_length)
    end subroutine de_note

