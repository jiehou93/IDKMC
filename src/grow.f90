    subroutine grow(i)
    !修改，空指无用指针
    !二叉树生长程序，加入一个对象后，为其新生长一片树叶，根据前一对象树叶各级父节点的饱和关系，决定如何将新树叶链接至二叉树上。
    use typ
    implicit none
    integer*4 i


    allocate(clu(i)%up)                             !新建一个新的对象节点（树叶），链接至对象i上
    clu(i)%up%obj=>clu(i)
    clu(i)%up%rate=0                                !初始化速率为0，这里只是分配空间，还没有添加缺陷信息
    nullify(clu(i)%up%left,clu(i)%up%right)

    current=>clu(i)%up                              !currren指向该节点
    current_previous=>clu(i-1)%up                   !current_previous指向前一个对象节点
    do
        if(.not.associated(current_previous%up))then            !若current_previous%up为根节点，则建立新的根节点，将current和current_previous都连接上去
            allocate(current_previous%up)                       !                   root
            root=>current_previous%up                           !                   /  \
            nullify(root%up,root%obj)                           !   current_previous    current
            root%left=>current_previous 
            root%right=>current
            root%rate=current_previous%rate
            current%up=>root
            height=height+1
            exit                                                !生长完毕，退出循环 
        elseif(.not.associated(current_previous%up%right))then  !若current_previous节点同级的右节点还未长出，则将current连接过去
            current%up=>current_previous%up                     !                               current_previous%up
            current_previous%up%right=>current                  !                                       /  \
            exit                                                !                       current_previous    current
            !生长完毕，退出循环
        else                                                    !若右节点已经长出，则为current新建一个父节点，并将current连接到父节点的左枝
            allocate(current%up)                                !       current_previous%up                 current%up
            nullify(current%up%obj,current%up%up,current%up%right)!           /  \                          /
            current%up%left=>current                            !           ****     current_previous   current
            current=>current%up                                 !两个current都向上移动一级
            current_previous=>current_previous%up
            current%rate=0                                      !父节点的速率初始化为0
        endif
    enddo

    end subroutine