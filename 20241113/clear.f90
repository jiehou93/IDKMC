    subroutine clear()
    !释放clu数组内存，删除整个二叉树，以免内存溢出
    use typ
    implicit none
    integer*4 i

    current=>root
    do                                          !先序遍历删除二叉树
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

    deallocate(clu)                             !删除对象数组
    end subroutine clear
