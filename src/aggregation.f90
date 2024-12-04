    function vicinity(i)
    !Ԫ���б����������ӳ����������Ŵ���Χ27��Ԫ���������Ŵأ�������ۺ����������ص�һ�����������Ŵر�ţ�����ʼλ���ܣ������Ľ�����λ�������жϡ�
    !����-1�����磬����-2����λ��������ֵ����ĳ��ȱ���Ŵأ�����-100000�����޽��ڶ���
    use typ
    implicit none
    integer*4 vicinity
    integer*4 i,j,i1,i0,index,ii,jj,kk,ll,a1,b1,c1
    integer*4 a(3),b(3),large_a(3),large_b(3),large_c(3)
    real*8 x(3),x1(3),x2(3),dis(3),dist,radius
    type(cell_list),pointer::pointer_dummy
    
    vicinity=-100000                                                        !û�н��ڶ���ʱ������ֵĬ��Ϊ-100000

    !���ȱ���Ƿ�ﵽx/y/z���� 
    do j=1,3
        if(pbc(j)==0)then
            if(clu(i)%coord(j)-clu(i)%r<surface_depth)then
                vicinity=-1
                defect_released=defect_released+clu(i)%formula
                return
            elseif(clu(i)%coord(j)+clu(i)%r>length(j)-surface_depth)then
                vicinity=-1
                defect_transmitted=defect_transmitted+clu(i)%formula
                return
            endif
        endif  
    enddo


    x=clu(i)%coord      
    a=x/cell_size+1                 !ȷ��Ԫ�����      

    do ii=1,3                                                           !��һ������27��Ԫ�������ؽ����Ŵر��
        do jj=1,3
            do kk=1,3
                pointer_dummy=>cell(neib_cell(a(1),a(2),a(3),1,ii),neib_cell(a(1),a(2),a(3),2,jj),neib_cell(a(1),a(2),a(3),3,kk))
                do index=1,pointer_dummy%counter                        !������Ԫ���������Ŵ�
                    pointer_dummy=>pointer_dummy%next
                    i1=pointer_dummy%clu%sn                             !��Ԫ�������ж���i1���Ŵر��
                    if(i1==i)cycle                                      !��ֹ�Ŵ�������Ӧ

                    dis=abs(clu(i1)%coord-x)                            !���������Ա߽������£��Ŵ����¼����Ŵصľ���
                    dist=sqrt(min(dis(1),length(1)-dis(1))**2+min(dis(2),length(2)-dis(2))**2+min(dis(3),length(3)-dis(3))**2)

                    if(dist<clu(i)%r+clu(i1)%r)then                     !�����߾���С�ڷ���뾶�����ؽ����Ŵر��
                        vicinity=i1                                     !����������ȱ�ݺ��������
                        return
                    endif
                enddo
            enddo
        enddo
    enddo


    !���¶Դ�Ԫ���б����ͬ��������      
    !���������Ա߽��������г���Ԫ����Χ��27����Ԫ������������������Ԫ����
    
    a=x/large_cell_size+1                                             !ȷ����Ԫ�����      
    do ii=1,3                                                           !��һ������27��Ԫ����������ۺ�������ִ�оۺϲ���
        do jj=1,3
            do kk=1,3
                pointer_dummy=>large_cell(large_neib_cell(a(1),a(2),a(3),1,ii),large_neib_cell(a(1),a(2),a(3),2,jj),large_neib_cell(a(1),a(2),a(3),3,kk))
                do index=1,pointer_dummy%counter
                    pointer_dummy=>pointer_dummy%next              !��Ԫ�������ж���i1���Ŵر��
                    i1=pointer_dummy%clu%sn                                !��ֹ�Ŵ�������Ӧ
                    if(i1==i)cycle                         
                    x1=clu(i1)%coord                                    !�������ۺ��Ŵص�����
                    dis=abs(x1-x)                                       !���������Ա߽������£��Ŵ����¼����Ŵصľ���
                    dist=sqrt(min(dis(1),length(1)-dis(1))**2+min(dis(2),length(2)-dis(2))**2+min(dis(3),length(3)-dis(3))**2)

                    if(dist<clu(i)%r+clu(i1)%r)then                                 !�����߾���С�ڷ���뾶��ִ�оۺϲ���
                        vicinity=i1                                    !����������ȱ�ݺ��������
                        return
                    endif
                enddo
            enddo
        enddo
    enddo
10  end

    subroutine aggregation(i,i1,aggregated)
    !�Ŵؾۺ��ӳ���,�ۺ�i��i1�Ŵأ��γ�һ���е��Ŵء�
    use typ
    use intf
    implicit none
    integer*4 i,i1,i0,index,ii,jj,kk,ll,a1,b1,c1,orien,n,n1,n2,eigen,eigen1,eigen2
    integer*4 a,b,c,formula(element),vicinity,large_a,large_b,large_c,aggregated
    real*8 x(3),x1(3),x2(3),dis(3),dist,radius,ran1,ran2,ran3,alpha,beta,rad,vector(3)


    !!!!!!!!!!!ȷ�����!!!!!!!!!!!!!
    eigen=clu(i)%formula(1)                             !����ۺϺ�ı���ȱ�ݺ���
    eigen1=clu(i1)%formula(1)
    eigen2=eigen+eigen1
    formula=clu(i)%formula+clu(i1)%formula              !�ۺϺ����
    orien=clu(i)%orien
    
    if(eigen2==0)then
        !I-V��ȫ���ϣ�����Ƿ�����������
        if(sum(abs(formula))==0)then
            !���������ɾ������ȱ��
            aggregated=-100000                              
            if(i1==nclu)then                                !��i1��clu�б�ĩ�ˣ���ɾ��i0��ʹ��i1λ���ƶ�
	            call dele(i1) 
                call dele(i) 
            else
                call dele(i) 
                call dele(i1)
            endif
            return
        else
            !�������������ȡ����һ����һ��i1Ϊ��ȱ�ݣ�
            x2=clu(i1)%coord
        endif
    else
        !I-V����ȫ����
        x=clu(i)%coord
        x1=clu(i1)%coord
        dis=abs(x1-x)       
        do ii=1,3
            if(dis(ii)>length(ii)/2) then               !�����Ա߽���
                if(x1(ii)>x(ii))then
                    x1(ii)=x1(ii)-length(ii)
                else
                    x(ii)=x(ii)-length(ii)
                endif
            endif                    
        enddo
        
        if(eigen*eigen1>0)then
            x2=(x*eigen+x1*eigen1)/(eigen+eigen1)           
            !ͬ��ȱ�ݸ��ϣ�����Ϊ����ȱ�ݵļ�Ȩƽ��ֵ
        else
            !���ȱ�ݸ��ϣ�Сȱ�����ȱ���ƶ�
            if(abs(eigen)>abs(eigen1))then
                x2=x+(x-x1)*abs(eigen1)/(abs(eigen1)+abs(eigen))
            else
                x2=x1+(x1-x)*abs(eigen)/(abs(eigen1)+abs(eigen))
            endif
        endif
    endif
                

    if(i1==nclu)then                                     !��i1��clu�б�ĩ�ˣ���ɾ��i0��ʹ��i1λ���ƶ�
	    call dele(i1) 
        call dele(i) 
    else
        call dele(i) 
        call dele(i1)
    endif
    
   
    call add(x2,orien,formula)                      !���ۺϺ���Ŵؿ���һ���µ��Ŵأ�add����ϵ��ȥ
    aggregated=nclu

10  end