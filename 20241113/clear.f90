    subroutine clear()
    !�ͷ�clu�����ڴ棬ɾ�������������������ڴ����
    use typ
    implicit none
    integer*4 i

    current=>root
    do                                          !�������ɾ��������
        if(associated(current%left))then
            current=>current%left
            continue
        elseif(associated(current%right))then
            current=>current%right
            continue
        elseif(associated(current%up))then
            current_previous=>current
            current=>current%up
            deallocate(current_previous)
            if(associated(current%left))then
                nullify(current%left)
            else
                nullify(current%right)
            endif
            continue
        else
            exit
        endif
    enddo

    deallocate(root)
    nullify(root)

    deallocate(clu)                             !ɾ����������
    end subroutine clear
