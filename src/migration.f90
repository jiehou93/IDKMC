    subroutine migrate(i,rot)
    !ȱ��Ǩ���ӳ��򣬼���Ǩ�ƺ�����꣬��λ��������Ԫ���б�����Ǩ�ƺ�������Ƿ�����ۺ��оݡ�
    use typ
    use intf
    implicit none
    integer*4 vicinity,formula(element),aggregated
    integer*4 i,v,j,jj,k,n,indexj,indexi,orien,orien1,direct,nclu1
    integer*4 a1,b1,c1,a2,b2,c2
    real*8 ran1,coord(3),vector(3)
    logical rot                                 !��ʾȱ���Ƿ�ת��Ĳ���
    type(cell_list),pointer::pointer_dummy

    !if(clu(i)%neigen<-1)print*,'mig',clu(i)%neigen

    orien=clu(i)%orien                          !��ȡ�Ŵصķ�λ

    if(rot)then                                 !������ת�����ѡ��ndirection����λ�е�1��
        call random_number(ran1)
        orien1=ran1*ndirection+1
    else                                        !����ת�򣬷�λ���ֲ������
        call random_number(ran1)
        orien1=(int(orien-0.1)/2+ran1)*2.0+1
    endif

    !print*,abs(move(:,orien))-abs(move(:,orien1))

    coord=clu(i)%coord+clu(i)%step*move(:,orien1)   !Ǩ�ƺ��������꣨������������Խ�����
    coord=coord-floor(coord/length)*length          !�����Խ���
    

    if(clu(i)%r<critical_radius)then            !�뾶С���ٽ�ֵ������ԭ���б�
        a1=clu(i)%coord(1)/cell_size(1)+1                !Ǩ��ǰ�Ŵ�����Ԫ����
        b1=clu(i)%coord(2)/cell_size(2)+1
        c1=clu(i)%coord(3)/cell_size(3)+1

        a2=coord(1)/cell_size(1)+1                  !Ǩ�ƺ��Ŵ�����Ԫ����
        b2=coord(2)/cell_size(2)+1
        c2=coord(3)/cell_size(3)+1

        clu(i)%coord(:)=coord(:)                !�����Ŵ�Ǩ�ƺ���������
        clu(i)%orien=orien1                     !�����Ŵصķ���
        if(a1==a2.and.b1==b2.and.c1==c2)then    
            !Ǩ�ƺ���ԭ��Ԫ����ֻ���������ͷ�λ
        else                                        
            !���������Ԫ���б�
           
            !ɾ��ԭ�ڵ�
		    call dele_node(clu(i)%index)
            cell(a1,b1,c1)%counter=cell(a1,b1,c1)%counter-1
            
            !�����½ڵ�
            call add_node(cell(a2,b2,c2))                             
            cell(a2,b2,c2)%counter=cell(a2,b2,c2)%counter+1
            
            !д���½ڵ���Ϣ
            cell(a2,b2,c2)%next%clu=>clu(i)                        
            clu(i)%index=>cell(a2,b2,c2)%next     
        endif
    else                                            !�뾶�����ٽ�ֵ�����´�ԭ���б�
        a1=clu(i)%coord(1)/large_cell_size(1)+1          !Ǩ��ǰ�Ŵ����ڴ�Ԫ����
        b1=clu(i)%coord(2)/large_cell_size(2)+1
        c1=clu(i)%coord(3)/large_cell_size(3)+1

        a2=coord(1)/large_cell_size(1)+1                 !Ǩ�ƺ��Ŵ����ڴ�Ԫ����
        b2=coord(2)/large_cell_size(2)+1
        c2=coord(3)/large_cell_size(3)+1

        clu(i)%coord(:)=coord(:)                    !��������ͷ�λ
        clu(i)%orien=orien1
        if(a1==a2.and.b1==b2.and.c1==c2)then        
            !Ǩ�ƺ���ԭ����Ԫ����ֻ���������ͷ�λ
        else                                        
            !��������´�Ԫ���б�
            !ɾ��ԭ�ڵ�
            call dele_node(clu(i)%index)
            large_cell(a1,b1,c1)%counter=large_cell(a1,b1,c1)%counter-1
            
            call add_node(large_cell(a2,b2,c2))                             !�����½ڵ�
            large_cell(a2,b2,c2)%counter=large_cell(a2,b2,c2)%counter+1
            
            large_cell(a2,b2,c2)%next%clu=>clu(i)                        !д���½ڵ���Ϣ
            clu(i)%index=>large_cell(a2,b2,c2)%next 
        endif
    endif
    
    !����sink strength���㾧�����ո���
    call RANDOM_NUMBER(ran1)
    if(ran1<=2.5*(clu(i)%step/grain_radi)**2.0)then
        grain_released=grain_released+clu(i)%formula
        call dele(i)  
    else
        nclu1=i
        v=vicinity(i)

        do while(v>0)                                                   !����Ƿ�ۺ�
            
            call aggregation(nclu1,v,aggregated)                        !ע�����ݹ����
            nclu1=aggregated
            if(nclu1>0)then                                             !����Ƿ��������ۺ�
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