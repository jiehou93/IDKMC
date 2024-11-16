subroutine renew_rate_all()
    !速率初始化子程序，根据当前温度，更新整个体系的速率
    use typ
    implicit none
    integer*4 i


    do i=0,nclu                                     !清零二叉树的速率
        current=>clu(i)%up
        do level=height,1,-1
            current%rate=0
            current=>current%up
        enddo
    enddo

    do i=1,nclu                                     !更新二叉树速率为新的温度下的速率
        !if(clu(i)%orien==0.and.clu(i)%formula(1)<0)then!若SIA团簇的取向不一致，则不可移动（此功能未开启，聚合后团簇取之前较大团簇的方向）
        !    clu(i)%rate(1)=0
        !    clu(i)%rate(2)=0
        !    clu(i)%rate(3)=clu(i)%ve*exp(-clu(i)%eb/kb/tem)
        !elseif(clu(i)%orien==0)then                !其他情况均可移动
        !    clu(i)%orien=1
        !    clu(i)%rate(1)=1.0/3.0*clu(i)%vm*exp(-clu(i)%em/kb/tem)
        !    clu(i)%rate(2)=2.0/3.0*clu(i)%vm*exp(-clu(i)%er/kb/tem)
        !    clu(i)%rate(3)=clu(i)%ve*exp(-clu(i)%eb/kb/tem)
        !else
        clu(i)%rate(1)=1.0/3.0*clu(i)%vm*exp(-clu(i)%em/kb/tem)
        clu(i)%rate(2)=clu(i)%rate(1)+2.0/3.0*clu(i)%vm*exp(-clu(i)%er/kb/tem)
        clu(i)%rate(3)=clu(i)%rate(2)+clu(i)%ve*exp(-clu(i)%eb/kb/tem)
        !endif
    enddo

    clu(0)%rate(3)=damage_rate              !外部事件速率
    do i=0,nclu
        current=>clu(i)%up
        do level=height,1,-1                !更新二叉树速率
            current%rate=current%rate+clu(i)%rate(3)
            current=>current%up
        enddo
    enddo

end subroutine renew_rate_all

    
subroutine renew_rate(i)
    !更新第i个缺陷对其各级父节点的速率影响
    use typ
    implicit none
    integer*4 i
    
    current=>clu(i)%up
    current%rate=clu(i)%rate(3)                         !缺陷速率
    do                                                  !重新计算二叉树相应的父节点速率
        current=>current%up
        if(associated(current%right))then
            current%rate=current%left%rate+current%right%rate
        else
            current%rate=current%left%rate
        endif
        if(.not.associated(current%up)) exit
    enddo
end subroutine renew_rate
