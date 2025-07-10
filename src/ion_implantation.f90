    subroutine ion_implantation(i_beam)
    !������ע�����ݿ��������ȡһ�����Ӽ������ˣ�ע�뵽��ϵ��
    use typ
    implicit none
    integer*4 i,j,ion_number,formula(4),orien,vicinity,nclu1,aggregated,i_beam
    real*8 rand_coord(3),coord(3)
    real*8 ran1,alpha,beta,rad
    type(ion_damage),pointer::cascade_pt(:)

    call random_number(ran1)
    call random_number(rand_coord)


    ion_number=ran1*ion_database_size(i_beam)+1
    rand_coord=rand_coord*uniform_damage*length                     !�жϼ�����xyz�������Ƿ���ȷֲ����ǵĻ����÷����������һ�����λ��
    do i=1,3
        if(pbc(i)==0)then
            rand_coord(i)=rand_coord(i)+surface_depth               !ע����沢����0��
        endif
    enddo
                             
    selectcase(i_beam)       
    case(1)
        cascade_pt=>cascade
    case(2)
        cascade_pt=>cascade2
    case(3)
        cascade_pt=>cascade3
    end select
        

    call RANDOM_NUMBER(ran1)
    orien=ran1*ndirection+1                                                  !�������
    coord=cascade_pt(ion_number)%ion_coord                             !���ӵ��������

    if(coord(implant_direction)>surface_depth)then                                  !С��0��ʾ�����뿪�в�
        coord=rand_coord+coord
        formula=(/0,0,0,0/)
		formula(ion_type(i_beam))=1;
        call add_and_vicinity(coord,orien,formula)                  !���ָ������ԭ��

    endif

    do i=1,cascade_pt(ion_number)%vpi
        coord=cascade_pt(ion_number)%vac_coord(:,i)
        if(coord(implant_direction)>surface_depth)then
            coord=rand_coord+coord
            call RANDOM_NUMBER(ran1)
            orien=ran1*ndirection+1                                          !�������
            formula=(/1,0,0,0/)
            call add_and_vicinity(coord,orien,formula)              !��ӿ�λ
        endif
    enddo

    do i=1,cascade_pt(ion_number)%ipi
        coord=cascade_pt(ion_number)%SIA_coord(:,i)
        if(coord(implant_direction)>surface_depth)then
            coord=rand_coord+coord
            call RANDOM_NUMBER(ran1)
            orien=ran1*ndirection+1                                          !�������
            formula=(/-1,0,0,0/)
            call add_and_vicinity(coord,orien,formula)              !���SIA)
        endif
    enddo
    end subroutine

