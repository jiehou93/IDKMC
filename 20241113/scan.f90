    subroutine scan()
    !调试用程序，当本程序被调用时,扫描对象列表，检查对象与树叶的链接、对象与元胞链表的索引关系、二叉树各级节点的速率是否有误
    use typ
    implicit none
    integer*4 i,j,k,a,b,c
    real*8 drate
    logical log1
    type(cell_list),pointer::pointer_dummy

    do i=1,nclu                                         !检查所有对象
        current=>clu(i)%up%up
        !if(abs(clu(i)%n)>1000)then
        !    print*,'index error'
        !endif
        !if (clu(i)%up%obj%sn/=i)then                      !检查对象与树叶的链接是否对应
        !    print*,'link error'
        !endif
        !if(abs(clu(i)%up%rate/=clu(i)%rate(3)))then      !检查对象与树叶的速率是否一致
        !    print*,'link error'
        !endif

        a=clu(i)%coord(1)/cell_size(1)+1
        b=clu(i)%coord(2)/cell_size(2)+1
        c=clu(i)%coord(3)/cell_size(3)+1
        
        if(cell(a,b,c)%counter==0)then                      !检查元胞链表是否记录了该缺陷
            print*,'index error'
        endif 
        
        log1=.false.
        pointer_dummy=>cell(a,b,c)
        do j=1,cell(a,b,c)%counter
            pointer_dummy=>pointer_dummy%next
            if(i==pointer_dummy%clu%sn) log1=.true.
        enddo
        
        if(.not. log1)then
            print*,'index error'
        endif 
            


        !do
        !    if (associated(current%right))then          !检查二叉树各级节点速率是否准确
        !        drate=abs(current%rate-current%left%rate-current%right%rate)
        !        if(drate/current%rate<0.01.or.drate==0)then
        !            current=>current%up
        !        else
        !            print*,'rate error'
        !            exit
        !        endif
        !    else
        !        drate=abs(current%rate-current%left%rate)
        !        if(drate/current%rate<0.01.or.drate==0)then
        !            current=>current%up
        !        else
        !            print*,'rate error'
        !            exit
        !        endif
        !    endif
        !    if (.not.associated(current%up))exit
        !enddo
    enddo
    
    do a=1,cell_number(1)
        do b=1,cell_number(2)
            do c=1,cell_number(3)
                log1=.false.
                pointer_dummy=>cell(a,b,c)
                do j=1,cell(a,b,c)%counter
                    pointer_dummy=>pointer_dummy%next
                    if(.not.associated(pointer_dummy%clu)) log1=.true.
                enddo
                if(associated(pointer_dummy%next)) log1=.true.
                if(log1)then
                    print*,'index error'
                endif
            enddo
        enddo
    enddo
    
    end
