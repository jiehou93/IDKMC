    subroutine load_para()
    !��PARA.TXT�ж��벿�ֶ���ѧ����
    use typ
    implicit none
    integer*4 GetFileN,file_lines,i,j,k,string_length,note_location,formula(4),dummy_int(4)
    character*300 dummy_string


    open(1001,file='PARA.txt',STATUS='OLD')  
    file_lines=GetFileN(1001)

    read(1001,*)para_range(1,:)                                                     !��ȡ�Զ�������ķ�Χ�������ڴ�
    read(1001,*)para_range(2,:)
    read(1001,*)para_range(3,:)
    read(1001,*)para_range(4,:)
    allocate(ion_para(8,para_range(1,1):para_range(1,2),para_range(2,1):para_range(2,2),para_range(3,1):para_range(3,2),para_range(4,1):para_range(4,2)))
    ion_para=-100

    do i=1,file_lines-4                                                             !��ȡ�߽����
        read(1001,'(A300)')dummy_string

        call de_note(dummy_string)                                                  !��ȡ��Ч����
        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   !����Ϊ��Ч��
        endif

        read(dummy_string,*)formula(1:4)                                            !��ȡȱ�����
        read(dummy_string,*)dummy_int(1:4),ion_para(1:8,formula(1),formula(2),formula(3),formula(4))    !��ȡ����
    enddo
    close(1001)
    end subroutine load_para