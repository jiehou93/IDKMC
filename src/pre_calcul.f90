    subroutine pre_calcul()
    !初始化模拟空间与元胞列表。
    use typ
    implicit none
    integer*4 i,j,k,large2small_ratio,n_cell,stat,file_lines,GetFileN
    real*8 dmax,lmin,box(3,2)
    character*300 dummy_string,msg
    
    if(length(1)<1e-6)then
        if(cfg_type=='lmp')then
             write(10,*)'    Box_length is undefined, try reading from POSITION.lmp...'
            !INPUT中未定义盒子尺寸，尝试从POSITION.lmp读取
            open(2000,file='POSITION.lmp',STATUS='OLD',iostat=stat,iomsg=msg) 
            if (stat == 0 ) then
                file_lines=GetFileN(2000)
                if(file_lines/=0) then
                    !读取头文件
                    read(2000,*)
                    read(2000,*)
                    read(2000,*)
                    read(2000,*)
                    read(2000,*)
                    read(2000,*)box(1,:)
                    read(2000,*)box(2,:)
                    read(2000,*)box(3,:)
                    length(:)=box(:,2)-box(:,1)
                    write(10,*)'    Successfully read box_length from POSITION.lmp...'
                endif
            endif
        endif
    endif
    
    if(length(1)>1e-6)then
        !直接设置box大小，元胞网格自动生成,元胞尺寸约为10 A
        write(10,*)'    Box_length is defined, automatically generating cell grids...'
        !确定最大捕获直径，确保大元胞网格尺寸大于该值
        dmax=2*maxval(para_table(4,:,:,:,:))
        lmin=max(20.0,dmax)
        !小元胞网格尺寸在10左右
        large2small_ratio=floor(lmin/10)
        
        if(length(1)<30 .or. length(2)<30 .or. length(3)<30)then
            write(10,*)'    Failed to generate cell grids, box size is too small, please manually define cell grids, or increase box size'
            stop
        else
            large_cell_number=floor(length/lmin)
            large_cell_size=length/large_cell_number
            cell_number=large_cell_number*large2small_ratio
            cell_size=large_cell_size/large2small_ratio
        endif
        
        !网格数量太大容易导致内存不足，逐步增大网格尺寸，降低网格数量
        n_cell=cell_number(1)*cell_number(2)*cell_number(3)
        do while(n_cell>5e6)
            lmin=lmin*1.001
            large_cell_number=floor(length/lmin)
            large_cell_size=length/large_cell_number
            cell_number=large_cell_number*large2small_ratio
            cell_size=large_cell_size/large2small_ratio
            n_cell=cell_number(1)*cell_number(2)*cell_number(3)
        enddo
    elseif(cell_number(1)*cell_number(2)*cell_number(3)/=0)then
        !未设置box大小，但设置了元胞网格，根据元胞网格计算box大小
        write(10,*)'    box_length not defined, set as cell_number*cell_size'
        length=cell_number*cell_size                              !盒子边长
    else
        write(10,*)'    box_length must be set manually, or read from POSITION.lmp'
        stop
    endif
    
    !增加浮点偏移，防止缺陷正好落在边界上时因浮点误差导致越界
    cell_size=cell_size
    large_cell_size=large_cell_size
        
    surface_area=1
    do i=1,3
        if(i==implant_direction)cycle
        surface_area=surface_area*length(i)             !注入方向的表面积
    enddo
    
   !为原胞列表数组分配内存
    allocate(cell(cell_number(1),cell_number(2),cell_number(3)))
    allocate(large_cell(large_cell_number(1),large_cell_number(2),large_cell_number(3)))
    allocate(neib_cell(cell_number(1),cell_number(2),cell_number(3),3,3))
    allocate(large_neib_cell(large_cell_number(1),large_cell_number(2),large_cell_number(3),3,3))
    
    do i=1,cell_number(1)
        do j=1,cell_number(2)
            do k=1,cell_number(3)
                cell(i,j,k)%counter=0                       !元胞链表初始化
                nullify(cell(i,j,k)%clu)
                nullify(cell(i,j,k)%previous)
                nullify(cell(i,j,k)%next)
                
                neib_cell(i,j,k,1,2)=i
                neib_cell(i,j,k,2,2)=j
                neib_cell(i,j,k,3,2)=k
                if(neib_cell(i,j,k,1,2)>1.and.neib_cell(i,j,k,1,2)<cell_number(1))then               !可能性大的事件放在前面，有利于提升效率
                    neib_cell(i,j,k,1,1)=i-1
                    neib_cell(i,j,k,1,3)=i+1    
                elseif(neib_cell(i,j,k,1,2)==1)then
                    neib_cell(i,j,k,1,1)=cell_number(1)
                    neib_cell(i,j,k,1,3)=i+1 
                else
                    neib_cell(i,j,k,1,1)=i-1
                    neib_cell(i,j,k,1,3)=1
                endif

                if(neib_cell(i,j,k,2,2)>1.and.neib_cell(i,j,k,2,2)<cell_number(2))then  
                    neib_cell(i,j,k,2,1)=j-1
                    neib_cell(i,j,k,2,3)=j+1    
                elseif(neib_cell(i,j,k,2,2)==1)then
                    neib_cell(i,j,k,2,1)=cell_number(2)
                    neib_cell(i,j,k,2,3)=j+1 
                else
                    neib_cell(i,j,k,2,1)=j-1
                    neib_cell(i,j,k,2,3)=1
                endif

                if(neib_cell(i,j,k,3,2)>1.and.neib_cell(i,j,k,3,2)<cell_number(3))then  
                    neib_cell(i,j,k,3,1)=k-1
                    neib_cell(i,j,k,3,3)=k+1    
                elseif(neib_cell(i,j,k,3,2)==1)then
                    neib_cell(i,j,k,3,1)=cell_number(3)
                    neib_cell(i,j,k,3,3)=k+1 
                else
                    neib_cell(i,j,k,3,1)=k-1
                    neib_cell(i,j,k,3,3)=1
                endif
            enddo
        enddo
    enddo
    
    do i=1,large_cell_number(1)
        do j=1,large_cell_number(2)
            do k=1,large_cell_number(3)
                large_cell(i,j,k)%counter=0                 !large元胞链表初始化
                nullify(large_cell(i,j,k)%clu)
                nullify(large_cell(i,j,k)%previous)
                nullify(large_cell(i,j,k)%next)
                large_neib_cell(i,j,k,1,2)=i
                large_neib_cell(i,j,k,2,2)=j
                large_neib_cell(i,j,k,3,2)=k
                if(large_neib_cell(i,j,k,1,2)>1.and.large_neib_cell(i,j,k,1,2)<large_cell_number(1))then               !可能性大的事件放在前面，有利于提升效率
                    large_neib_cell(i,j,k,1,1)=i-1
                    large_neib_cell(i,j,k,1,3)=i+1    
                elseif(large_neib_cell(i,j,k,1,2)==1)then
                    large_neib_cell(i,j,k,1,1)=large_cell_number(1)
                    large_neib_cell(i,j,k,1,3)=i+1 
                else
                    large_neib_cell(i,j,k,1,1)=i-1
                    large_neib_cell(i,j,k,1,3)=1
                endif

                if(large_neib_cell(i,j,k,2,2)>1.and.large_neib_cell(i,j,k,2,2)<large_cell_number(2))then  
                    large_neib_cell(i,j,k,2,1)=j-1
                    large_neib_cell(i,j,k,2,3)=j+1    
                elseif(large_neib_cell(i,j,k,2,2)==1)then
                    large_neib_cell(i,j,k,2,1)=large_cell_number(2)
                    large_neib_cell(i,j,k,2,3)=j+1 
                else
                    large_neib_cell(i,j,k,2,1)=j-1
                    large_neib_cell(i,j,k,2,3)=1
                endif

                if(large_neib_cell(i,j,k,3,2)>1.and.large_neib_cell(i,j,k,3,2)<large_cell_number(3))then  
                    large_neib_cell(i,j,k,3,1)=k-1
                    large_neib_cell(i,j,k,3,3)=k+1    
                elseif(large_neib_cell(i,j,k,3,2)==1)then
                    large_neib_cell(i,j,k,3,1)=large_cell_number(3)
                    large_neib_cell(i,j,k,3,3)=k+1 
                else
                    large_neib_cell(i,j,k,3,1)=k-1
                    large_neib_cell(i,j,k,3,3)=1
                endif
            enddo
        enddo
    enddo
    
    !设置默认critical_radius为cell_size最小边长的一半
    critical_radius=minval(cell_size)/2

    !定义一个空缺陷clunull，用来填充被删除的缺陷
    nullify(clunull%up)
    clunull%sn=-100000
    clunull%n=-100000                   !团簇大小
    nullify(clunull%index)              !元胞索引
    clunull%orien=-100000               !方位
    clunull%formula=0                   !成分
    clunull%coord=-100000               !坐标
    clunull%rate(:)=0                   !反应速率为0
    clunull%para(1:3)=100               !能垒为100ev
    clunull%para(4:6)=0                 !半径、尝试频率为0
    clunull%para(7:8)=-100000           !离谱的步长、发射组分
    end subroutine pre_calcul