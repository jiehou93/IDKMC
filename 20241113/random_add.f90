    subroutine random_add(formula)
    !����ϵ��������һ��ָ���ɷ�ȱ��
    use typ
    implicit none
    integer*4 i,v,k,orien,formula(element),nclu1,vicinity,aggregated
    real*8 ran1,ran2,ran3,ran4,alpha,beta,rad,coord(3)

    !call RANDOM_NUMBER(ran1)                        
    !call RANDOM_NUMBER(ran2)
    !call RANDOM_NUMBER(ran3)
    call RANDOM_NUMBER(ran4)

    call RANDOM_NUMBER(coord)
    coord=coord*length 
    !if(sum(pbc)>=0)then                                 !�ں������������һ������
    !    coord(1)=length(1)*ran1
    !    coord(2)=length(2)*ran2
    !    coord(3)=length(3)*ran3
    !else
    !    alpha=ran1*2*pi
    !    beta=acos(1-2*ran2)                             !ע�⼫�ǺͰ뾶�ķֲ�����������
    !    rad=ran3**(1.0/3.0)*grain_radi                  !�����ξ��������һ������
    !    coord(1)=rad*sin(beta)*cos(alpha)
    !    coord(2)=rad*sin(beta)*sin(alpha)
    !    coord(3)=rad*cos(beta)
    !endif

    orien=ran4*8+1                                      !�������

    call add_and_vicinity(coord,orien,formula)          !�ڸ�λ�ü���һ�������͵�ȱ���Ŵ�
    end subroutine
