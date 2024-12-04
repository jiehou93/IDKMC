    subroutine ion_implantation
    !从离子注入氘滞留数据库中随机抽取一个离子及其损伤，注入到体系中
    use typ
    implicit none
    integer*4 i,j,ion_number,formula(4),orien,vicinity,nclu1,aggregated
    real*8 rand_coord(3),coord(3)
    real*8 ran1,alpha,beta,rad


    call random_number(ran1)
    call random_number(rand_coord)


    ion_number=ran1*ion_database_size+1
    rand_coord=rand_coord*uniform_damage*length                     !判断级联在xyz方向上是否均匀分布，是的话，该方向会额外添加一个随机位移
    do i=1,3
        if(pbc(i)==0)then
            rand_coord(i)=rand_coord(i)+surface_depth               !注意表面并不在0处
        endif
    enddo
                             
                      

    call RANDOM_NUMBER(ran1)
    orien=ran1*8+1                                                  !随机方向
    coord=cascade(ion_number)%ion_coord                             !离子的相对坐标

    if(coord(implant_direction)>surface_depth)then                                  !小于0表示离子离开靶材
        coord=rand_coord+coord
        formula=(/0,0,0,0/)
		formula(ion_type)=1;
        call add_and_vicinity(coord,orien,formula)                  !添加指定类型原子

    endif

    do i=1,cascade(ion_number)%vpi
        coord=cascade(ion_number)%vac_coord(:,i)
        if(coord(implant_direction)>surface_depth)then
            coord=rand_coord+coord
            call RANDOM_NUMBER(ran1)
            orien=ran1*8+1                                          !随机方向
            formula=(/1,0,0,0/)
            call add_and_vicinity(coord,orien,formula)              !添加空位
        endif
    enddo

    do i=1,cascade(ion_number)%ipi
        coord=cascade(ion_number)%SIA_coord(:,i)
        if(coord(implant_direction)>surface_depth)then
            coord=rand_coord+coord
            call RANDOM_NUMBER(ran1)
            orien=ran1*8+1                                          !随机方向
            formula=(/-1,0,0,0/)
            call add_and_vicinity(coord,orien,formula)              !添加SIA)
        endif
    enddo
    end subroutine

