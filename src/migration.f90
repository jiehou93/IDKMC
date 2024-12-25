    subroutine migrate(i,rot)
    !缺陷迁移子程序，计算迁移后的坐标，方位。并更新元胞列表。检索迁移后的坐标是否满足聚合判据。
    use typ
    use intf
    implicit none
    integer*4 vicinity,formula(element),aggregated
    integer*4 i,v,j,jj,k,n,indexj,indexi,orien,orien1,direct,nclu1
    integer*4 a1,b1,c1,a2,b2,c2
    real*8 ran1,coord(3),vector(3)
    logical rot                                 !表示缺陷是否转向的参量
    type(cell_list),pointer::pointer_dummy

    !if(clu(i)%neigen<-1)print*,'mig',clu(i)%neigen

    orien=clu(i)%orien                          !读取团簇的方位

    if(rot)then                                 !若发生转向，随机选择ndirection个方位中的1个
        call random_number(ran1)
        orien1=ran1*ndirection+1
    else                                        !若不转向，方位保持不变或反向
        call random_number(ran1)
        orien1=(int(orien-0.1)/2+ran1)*2.0+1
    endif

    !print*,abs(move(:,orien))-abs(move(:,orien1))

    coord=clu(i)%coord+clu(i)%step*move(:,orien1)   !迁移后的相对坐标（还需进行周期性矫正）
    coord=coord-floor(coord/length)*length          !周期性矫正
    

    if(clu(i)%r<critical_radius)then            !半径小于临界值，更新原胞列表
        a1=clu(i)%coord(1)/cell_size(1)+1                !迁移前团簇所在元胞号
        b1=clu(i)%coord(2)/cell_size(2)+1
        c1=clu(i)%coord(3)/cell_size(3)+1

        a2=coord(1)/cell_size(1)+1                  !迁移后团簇所在元胞号
        b2=coord(2)/cell_size(2)+1
        c2=coord(3)/cell_size(3)+1

        clu(i)%coord(:)=coord(:)                !更新团簇迁移后的相对坐标
        clu(i)%orien=orien1                     !更新团簇的方向
        if(a1==a2.and.b1==b2.and.c1==c2)then    
            !迁移后还在原来元胞，只需更新坐标和方位
        else                                        
            !否则还需更新元胞列表
           
            !删除原节点
		    call dele_node(clu(i)%index)
            cell(a1,b1,c1)%counter=cell(a1,b1,c1)%counter-1
            
            !插入新节点
            call add_node(cell(a2,b2,c2))                             
            cell(a2,b2,c2)%counter=cell(a2,b2,c2)%counter+1
            
            !写入新节点信息
            cell(a2,b2,c2)%next%clu=>clu(i)                        
            clu(i)%index=>cell(a2,b2,c2)%next     
        endif
    else                                            !半径大于临界值，更新大原胞列表
        a1=clu(i)%coord(1)/large_cell_size(1)+1          !迁移前团簇所在大元胞号
        b1=clu(i)%coord(2)/large_cell_size(2)+1
        c1=clu(i)%coord(3)/large_cell_size(3)+1

        a2=coord(1)/large_cell_size(1)+1                 !迁移后团簇所在大元胞号
        b2=coord(2)/large_cell_size(2)+1
        c2=coord(3)/large_cell_size(3)+1

        clu(i)%coord(:)=coord(:)                    !更新坐标和方位
        clu(i)%orien=orien1
        if(a1==a2.and.b1==b2.and.c1==c2)then        
            !迁移后还在原来大元胞，只需更新坐标和方位
        else                                        
            !否则还需更新大元胞列表
            !删除原节点
            call dele_node(clu(i)%index)
            large_cell(a1,b1,c1)%counter=large_cell(a1,b1,c1)%counter-1
            
            call add_node(large_cell(a2,b2,c2))                             !插入新节点
            large_cell(a2,b2,c2)%counter=large_cell(a2,b2,c2)%counter+1
            
            large_cell(a2,b2,c2)%next%clu=>clu(i)                        !写入新节点信息
            clu(i)%index=>large_cell(a2,b2,c2)%next 
        endif
    endif
    
    !根据sink strength计算晶界吸收概率
    call RANDOM_NUMBER(ran1)
    if(ran1<=2.5*(clu(i)%step/grain_radi)**2.0)then
        grain_released=grain_released+clu(i)%formula
        call dele(i)  
    else
        nclu1=i
        v=vicinity(i)

        do while(v>0)                                                   !检查是否聚合
            
            call aggregation(nclu1,v,aggregated)                        !注意避免递归调用
            nclu1=aggregated
            if(nclu1>0)then                                             !检查是否发生连续聚合
                v=vicinity(nclu1)
            else
                v=-100000
            endif
        enddo
        if(v>-10)then
            call dele(nclu1)                                            !absorbed by surface
        endif
    endif

    end    