    subroutine pre_calcul()
    !��ʼ��ģ��ռ���Ԫ���б�
    use typ
    implicit none
    integer*4 i,j,k,large2small_ratio,n_cell
    real*8 dmax,lmin
    
    if(length(1)*length(2)*length(3)/=0)then
        !ֱ������box��С��Ԫ�������Զ�����,Ԫ���ߴ�ԼΪ10 A
        write(10,*)'    Box_length is defined, automatically generating cell grids...'
        !ȷ����󲶻�ֱ����ȷ����Ԫ������ߴ���ڸ�ֵ
        dmax=2*maxval(ion_para(4,:,:,:,:))
        lmin=max(20.0,dmax)
        !СԪ������ߴ���10����
        large2small_ratio=floor(lmin/10)
        
        if(length(1)<20 .or. length(2)<20 .or. length(3)<20)then
            write(10,*)'    Failed to generate cell grids, box size is too small, please manually define cell grids, or increase box size'
            stop
        else
            large_cell_number=floor(length/lmin)
            large_cell_size=length/large_cell_number
            cell_number=large_cell_number*large2small_ratio
            cell_size=large_cell_size/large2small_ratio
        endif
        
        !��������̫�����׵����ڴ治�㣬����������ߴ磬������������
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
        !δ����box��С����������Ԫ�����񣬸���Ԫ���������box��С
        write(10,*)'    box_length not defined, set as cell_number*cell_size'
        length=cell_number*cell_size                              !���ӱ߳�
    else
        write(10,*)'    Neither box_length nor cell grid is defined, no way to continue.'
        stop
    endif
    !���Ӹ���ƫ�ƣ���ֹȱ���������ڱ߽���ʱ�򸡵�����Խ��
    cell_size=cell_size+1E-6
    large_cell_size=large_cell_size+1E-6
        
    surface_area=1
    do i=1,3
        if(i==implant_direction)cycle
        surface_area=surface_area*length(i)             !ע�뷽��ı����
    enddo
    
   !Ϊԭ���б���������ڴ�
    allocate(cell(cell_number(1),cell_number(2),cell_number(3)))
    allocate(large_cell(large_cell_number(1),large_cell_number(2),large_cell_number(3)))
    allocate(neib_cell(cell_number(1),cell_number(2),cell_number(3),3,3))
    allocate(large_neib_cell(large_cell_number(1),large_cell_number(2),large_cell_number(3),3,3))
    
    do i=1,cell_number(1)
        do j=1,cell_number(2)
            do k=1,cell_number(3)
                cell(i,j,k)%counter=0                       !Ԫ�������ʼ��
                nullify(cell(i,j,k)%clu)
                nullify(cell(i,j,k)%previous)
                nullify(cell(i,j,k)%next)
                
                neib_cell(i,j,k,1,2)=i
                neib_cell(i,j,k,2,2)=j
                neib_cell(i,j,k,3,2)=k
                if(neib_cell(i,j,k,1,2)>1.and.neib_cell(i,j,k,1,2)<cell_number(1))then               !�����Դ���¼�����ǰ�棬����������Ч��
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
                large_cell(i,j,k)%counter=0                 !largeԪ�������ʼ��
                nullify(large_cell(i,j,k)%clu)
                nullify(large_cell(i,j,k)%previous)
                nullify(large_cell(i,j,k)%next)
                large_neib_cell(i,j,k,1,2)=i
                large_neib_cell(i,j,k,2,2)=j
                large_neib_cell(i,j,k,3,2)=k
                if(large_neib_cell(i,j,k,1,2)>1.and.large_neib_cell(i,j,k,1,2)<large_cell_number(1))then               !�����Դ���¼�����ǰ�棬����������Ч��
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
    
    !����Ĭ��critical_radiusΪcell_size��С�߳���һ��
    critical_radius=minval(cell_size)/2
    
    move(:,1)=(/1,1,1/)                         !�����ƶ���ʸ
    move(:,2)=(/-1,-1,-1/)
    move(:,3)=(/1,-1,1/)
    move(:,4)=(/-1,1,-1/)
    move(:,5)=(/1,-1,-1/)
    move(:,6)=(/-1,1,1/)
    move(:,7)=(/-1,-1,1/)
    move(:,8)=(/1,1,-1/)
    move=move*sqrt(3.0d0)/3.0d0                       !��һ���ƶ���ʸ


    !����һ����ȱ��clunull��������䱻ɾ����ȱ��
    nullify(clunull%up)
    clunull%sn=-100000
    clunull%n=-100000                   !�Ŵش�С
    nullify(clunull%index)              !Ԫ������
    clunull%orien=-100000               !��λ
    clunull%emit=-100000                !����ʱ��������
    clunull%step=-100000
    clunull%formula=0                   !�ɷ�
    clunull%coord=-100000               !����
    clunull%rate(:)=0                   !��Ӧ����Ϊ0
    clunull%vm=0                        !����Ƶ��Ϊ0
    clunull%ve=0
    clunull%r=0                         !�뾶Ϊ0
    clunull%em=100                      !����Ϊ100ev
    clunull%er=100
    clunull%eb=100
    end subroutine pre_calcul