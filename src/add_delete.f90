    subroutine add(coord_in,orien,formula)
    !添加团簇子程序，将团簇添加至团簇列表的尾部，并更新元胞列表和二叉树速率
    use typ
    use intf
    implicit none
    integer*4 i,j,n,formula(element),orien,a,b,c,nclu1
    real*8 coord(3),coord_in(3)

    if(nclu>=size(clu)-1) call extend()                     !注意clu(0)不在nclu统计范围内
    nclu1=nclu                                              !避免用全局变量做形参
    if(.not.associated(clu(nclu+1)%up)) call grow(nclu1+1)  !若加入的缺陷团簇还未链接至二叉树，则二叉树生长出一片新树叶，链接至该缺陷团簇

    nclu=nclu+1                                             !团簇总数+1
    call assign_para(formula,clu(nclu)%para)                !团簇参数                   

    coord=coord_in-floor(coord_in/length)*length                  !PBC correction
    if(clu(nclu)%para(4)<critical_radius)then                     !根据团簇大小判断加入哪个元胞列表
        a=coord(1)/cell_size(1)+1                                !计算添加团簇所在的元胞位置
        b=coord(2)/cell_size(2)+1
        c=coord(3)/cell_size(3)+1    
        
        call add_node(cell(a,b,c))
        cell(a,b,c)%counter=cell(a,b,c)%counter+1           !元胞中团簇数+1
        cell(a,b,c)%next%clu=>clu(nclu)                     !写入新节点信息
        clu(nclu)%index=>cell(a,b,c)%next                      
    else
        a=coord(1)/large_cell_size(1)+1                          !计算添加团簇所在的大元胞位置
        b=coord(2)/large_cell_size(2)+1
        c=coord(3)/large_cell_size(3)+1    
        
        call add_node(large_cell(a,b,c))
        large_cell(a,b,c)%counter=large_cell(a,b,c)%counter+1       !元胞中团簇数+1
        large_cell(a,b,c)%next%clu=>clu(nclu)                        !写入新节点信息
        clu(nclu)%index=>large_cell(a,b,c)%next                      
    endif

    clu(nclu)%formula=formula                               !团簇组分
    clu(nclu)%n=sum(abs(formula))                           !包含的缺陷总数
    clu(nclu)%orien=orien                                   !团簇的方位
    clu(nclu)%coord=coord                                   !团簇的相对坐标
    
    clu(nclu)%rate(1)=1.0/3.0*clu(nclu)%para(6)*exp(-clu(nclu)%para(1)/kb/tem)
    clu(nclu)%rate(2)=clu(nclu)%rate(1)+2.0/3.0*clu(nclu)%para(6)*exp(-clu(nclu)%para(2)/kb/tem)
    clu(nclu)%rate(3)=clu(nclu)%rate(2)+clu(nclu)%para(5)*exp(-clu(nclu)%para(3)/kb/tem)
    !endif

    current=>clu(nclu)%up
    do level=height,1,-1                                    !更新二叉树速率
        current%rate=current%rate+clu(nclu)%rate(3)
        current=>current%up
    enddo

    !print*,'add',nclu
    end

    subroutine add_and_vicinity(coord,orien,formula)
    !添加团簇后进行近邻搜索,根据搜索结果选择执行何种反应
    use typ
    implicit none
    integer*4 formula(element),orien,nclu1,v,vicinity,aggregated
    real*8 coord(3)

    call add(coord,orien,formula)
    nclu1=nclu
    v=vicinity(nclu1)
    do while(v>0)
        !if (clu(v)%n>100.or.clu(nclu1)%n>100)then
        !    print*,'error'
        !endif

        call aggregation(nclu1,v,aggregated)                    !注意避免递归调用
        nclu1=aggregated
        if(nclu1>0)then                                         !检查是否发生连续聚合
            v=vicinity(nclu1)
        else
            v=-100000
        endif
    enddo
    if(v>-10) call dele(nclu1)
    end subroutine add_and_vicinity

    subroutine dele(i)
    !将团簇i从团簇列表中删除
    use typ
    use intf
    implicit none
    integer*4 i,ai,bi,ci,j,k,ak,bk,ck,indexj,a3,b3,c3
    real*8 drate
    type(cell_list),pointer::temp_dummy
    
    call dele_node(clu(i)%index)
    if(clu(i)%para(4)<critical_radius)then                        !根据团簇大小判断对哪个元胞链表进行计数器-1
        ai=clu(i)%coord(1)/cell_size(1)+1                        
        bi=clu(i)%coord(2)/cell_size(2)+1
        ci=clu(i)%coord(3)/cell_size(3)+1                            
        cell(ai,bi,ci)%counter=cell(ai,bi,ci)%counter-1     !从元胞链表中删除i
    else
        ai=clu(i)%coord(1)/large_cell_size(1)+1                  
        bi=clu(i)%coord(2)/large_cell_size(2)+1
        ci=clu(i)%coord(3)/large_cell_size(3)+1
        large_cell(ai,bi,ci)%counter=large_cell(ai,bi,ci)%counter-1 !从大元胞链表中删除i
    endif

    k=nclu
    nclu=nclu-1                                                    
    if(i/=k)then                                            !尾部替换法从clu数组中删除i
        call replace(clu(i),clu(k))                         
        call replace(clu(k),clunull)
        
        call renew_rate(k)
        call renew_rate(i)
    else                                                    !直接删除
        call replace(clu(k),clunull)        
        call renew_rate(k)
    endif

    end subroutine dele
