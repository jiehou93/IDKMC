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
        clu(i)%rate(1)=1.0/3.0*clu(i)%para(6)*exp(-clu(i)%para(1)/kb/tem)
        clu(i)%rate(2)=clu(i)%rate(1)+2.0/3.0*clu(i)%para(6)*exp(-clu(i)%para(2)/kb/tem)
        clu(i)%rate(3)=clu(i)%rate(2)+clu(i)%para(5)*exp(-clu(i)%para(3)/kb/tem)
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
