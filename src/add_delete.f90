    subroutine add(coord_in,orien,formula)
    !����Ŵ��ӳ��򣬽��Ŵ�������Ŵ��б��β����������Ԫ���б�Ͷ���������
    use typ
    use intf
    implicit none
    integer*4 i,j,n,formula(element),orien,a,b,c,nclu1
    real*8 coord(3),coord_in(3)

    if(nclu>=size(clu)-1) call extend()                     !ע��clu(0)����ncluͳ�Ʒ�Χ��
    nclu1=nclu                                              !������ȫ�ֱ������β�
    if(.not.associated(clu(nclu+1)%up)) call grow(nclu1+1)  !�������ȱ���Ŵػ�δ���������������������������һƬ����Ҷ����������ȱ���Ŵ�

    nclu=nclu+1                                             !�Ŵ�����+1
    call assign_para(formula,clu(nclu)%para)                !�Ŵز���                   

    coord=coord_in-floor(coord_in/length)*length                  !PBC correction
    if(clu(nclu)%para(4)<critical_radius)then                     !�����Ŵش�С�жϼ����ĸ�Ԫ���б�
        a=coord(1)/cell_size(1)+1                                !��������Ŵ����ڵ�Ԫ��λ��
        b=coord(2)/cell_size(2)+1
        c=coord(3)/cell_size(3)+1    
        
        call add_node(cell(a,b,c))
        cell(a,b,c)%counter=cell(a,b,c)%counter+1           !Ԫ�����Ŵ���+1
        cell(a,b,c)%next%clu=>clu(nclu)                     !д���½ڵ���Ϣ
        clu(nclu)%index=>cell(a,b,c)%next                      
    else
        a=coord(1)/large_cell_size(1)+1                          !��������Ŵ����ڵĴ�Ԫ��λ��
        b=coord(2)/large_cell_size(2)+1
        c=coord(3)/large_cell_size(3)+1    
        
        call add_node(large_cell(a,b,c))
        large_cell(a,b,c)%counter=large_cell(a,b,c)%counter+1       !Ԫ�����Ŵ���+1
        large_cell(a,b,c)%next%clu=>clu(nclu)                        !д���½ڵ���Ϣ
        clu(nclu)%index=>large_cell(a,b,c)%next                      
    endif

    clu(nclu)%formula=formula                               !�Ŵ����
    clu(nclu)%n=sum(abs(formula))                           !������ȱ������
    clu(nclu)%orien=orien                                   !�Ŵصķ�λ
    clu(nclu)%coord=coord                                   !�Ŵص��������
    
    clu(nclu)%rate(1)=1.0/3.0*clu(nclu)%para(6)*exp(-clu(nclu)%para(1)/kb/tem)
    clu(nclu)%rate(2)=clu(nclu)%rate(1)+2.0/3.0*clu(nclu)%para(6)*exp(-clu(nclu)%para(2)/kb/tem)
    clu(nclu)%rate(3)=clu(nclu)%rate(2)+clu(nclu)%para(5)*exp(-clu(nclu)%para(3)/kb/tem)
    !endif

    current=>clu(nclu)%up
    do level=height,1,-1                                    !���¶���������
        current%rate=current%rate+clu(nclu)%rate(3)
        current=>current%up
    enddo

    !print*,'add',nclu
    end

    subroutine add_and_vicinity(coord,orien,formula)
    !����Ŵغ���н�������,�����������ѡ��ִ�к��ַ�Ӧ
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

        call aggregation(nclu1,v,aggregated)                    !ע�����ݹ����
        nclu1=aggregated
        if(nclu1>0)then                                         !����Ƿ��������ۺ�
            v=vicinity(nclu1)
        else
            v=-100000
        endif
    enddo
    if(v>-10) call dele(nclu1)
    end subroutine add_and_vicinity

    subroutine dele(i)
    !���Ŵ�i���Ŵ��б���ɾ��
    use typ
    use intf
    implicit none
    integer*4 i,ai,bi,ci,j,k,ak,bk,ck,indexj,a3,b3,c3
    real*8 drate
    type(cell_list),pointer::temp_dummy
    
    call dele_node(clu(i)%index)
    if(clu(i)%para(4)<critical_radius)then                        !�����Ŵش�С�ж϶��ĸ�Ԫ��������м�����-1
        ai=clu(i)%coord(1)/cell_size(1)+1                        
        bi=clu(i)%coord(2)/cell_size(2)+1
        ci=clu(i)%coord(3)/cell_size(3)+1                            
        cell(ai,bi,ci)%counter=cell(ai,bi,ci)%counter-1     !��Ԫ��������ɾ��i
    else
        ai=clu(i)%coord(1)/large_cell_size(1)+1                  
        bi=clu(i)%coord(2)/large_cell_size(2)+1
        ci=clu(i)%coord(3)/large_cell_size(3)+1
        large_cell(ai,bi,ci)%counter=large_cell(ai,bi,ci)%counter-1 !�Ӵ�Ԫ��������ɾ��i
    endif

    k=nclu
    nclu=nclu-1                                                    
    if(i/=k)then                                            !β���滻����clu������ɾ��i
        call replace(clu(i),clu(k))                         
        call replace(clu(k),clunull)
        
        call renew_rate(k)
        call renew_rate(i)
    else                                                    !ֱ��ɾ��
        call replace(clu(k),clunull)        
        call renew_rate(k)
    endif

    end subroutine dele
