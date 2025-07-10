subroutine load_ion_database()
    !根据damage_type的类型，选择离子辐照数据库的输入文件
    use typ
    use intf
    implicit none
    integer*4 i,j,sum_sia,sum_vac,sum_ion
    real*8 sum_depth_ion,sum_depth_sia,sum_depth_vac
    type(ion_damage),pointer::cascade_pt(:)
        
    if(irr_status==0)then
        write(10,*)'    Implantation database files incomplete, disabling irradiation function.....'
        return
    endif
    
    if(damage_type=='txt')then
        if(n_beam>1)then
            write(10,*)'    damage_type=txt is not supported under multi-beam irradiation, please use cfg style'
            stop
        endif
        
        write(10,*)'    Loading ion implantation database from ION/VAC/SIA.txt files...'
        call load_ion_database_txt()
    elseif(damage_type=='cfg')then    
        write(10,*)'    Loading ion implantation database from aiv.xyz.cfg file...'
        if(n_beam>=1)  call load_ion_database_cfg(cascade,1)
        if(n_beam>=2)  call load_ion_database_cfg(cascade2,2)
        if(n_beam>=3)  call load_ion_database_cfg(cascade3,3)
    else
        write(10,*)'    Error, damage_type not supported, please choose txt or cfg.'
        stop
    endif
    
    do i=1,n_beam    
        !统计并输出每个数据库的基本信息
        selectcase(i)       
        case(1)
            cascade_pt=>cascade
        case(2)
            cascade_pt=>cascade2
        case(3)
            cascade_pt=>cascade3
        end select
        
        sum_ion=0
        sum_sia=0
        sum_vac=0
        sum_depth_ion=0
        sum_depth_sia=0
        sum_depth_vac=0
        do j=1,ion_database_size(i)
            if(cascade_pt(j)%ion_coord(1)>0)sum_ion=sum_ion+1
            sum_sia=sum_sia+cascade_pt(j)%ipi
            sum_vac=sum_vac+cascade_pt(j)%vpi
            
            !注意这里的implant diirection已经与damage direction对齐
            if(cascade_pt(j)%ion_coord(1)>0)sum_depth_ion=sum_depth_ion+cascade_pt(j)%ion_coord(implant_direction)
            sum_depth_sia=sum_depth_sia+sum(cascade_pt(j)%vac_coord(implant_direction,:))
            sum_depth_vac=sum_depth_vac+sum(cascade_pt(j)%SIA_coord(implant_direction,:))
        enddo
        
        write(10,*)'    Information for damage database',i
        write(10,*)'        Ion type:',ion_type(i)
        write(10,*)'        Number of Ions loaded:',ion_database_size(i)
        write(10,*)'        Number of Ions reflected:',ion_database_size(i)-sum_ion
        write(10,*)'        Number of SIA  loaded:',sum_sia
        write(10,*)'        Number of VAC  loaded:',sum_vac
        write(10,*)'        Number of SIA per ION:',sum_sia*1.0/ion_database_size(i)
        write(10,*)'        Number of VAC per ION:',sum_vac*1.0/ion_database_size(i)
        write(10,*)'        Mean ION depth:',sum_depth_ion*1.0/sum_ion
        write(10,*)'        Mean SIA depth:',sum_depth_sia*1.0/sum_sia
        write(10,*)'        Mean VAC depth:',sum_depth_vac*1.0/sum_vac
        write(10,*)
    enddo

end subroutine load_ion_database

subroutine load_ion_database_cfg(cascade_in,i_beam)    
    !载入离子注入数据库
    use typ
    implicit none
    integer n_ion,i_beam
    integer*4 GetFileN,i,j,k,ierr,filelines,cfg_data_size
    real*4 box_scale,box(3)
    real*4,allocatable::coord(:,:)
    integer*4,allocatable::defect_type(:),ion_index(:)
    character*160 path,content_string,makepath,c_dummy,filename
    type(ion_damage),allocatable,intent(inout):: cascade_in(:)
    
    selectcase(i_beam)       
    case(1)
        open(1000,file='aiv.xyz.cfg',STATUS='OLD')                       !打开原文件
    case(2)
        open(1000,file='aiv.xyz2.cfg',STATUS='OLD')                       !打开原文件
    case(3)
        open(1000,file='aiv.xyz3.cfg',STATUS='OLD')                       !打开原文件
    end select
    
    
    filelines=GetFileN(1000) 
    cfg_data_size=filelines-19
    allocate(coord(cfg_data_size,3))
    allocate(defect_type(cfg_data_size))
    allocate(ion_index(cfg_data_size))
    do i=1,19                                           
        !读取前19行的盒子参数
        read(1000,'(A160)') content_string
        if(i==2) read(content_string,*)c_dummy,c_dummy,box_scale
        if(i==3) read(content_string(11:),*)box(1)
        if(i==7) read(content_string(11:),*)box(2)
        if(i==11)read(content_string(11:),*)box(3)
    enddo
    box=box*box_scale
    
    do  i=1,filelines-19
        !读取所有cascade
        read(1000,*)coord(i,:),defect_type(i),ion_index(i)
    enddo
    
    defect_type=defect_type+1
    !1/2/3分别代表 ion sia vac
    ion_index=ion_index+1
    ion_database_size(i_beam)=maxval(ion_index)
    allocate(cascade_in(ion_database_size(i_beam)))
    
    do i=1,3
        !计算绝对坐标
        coord(:,i)=coord(:,i)*box(i)
    enddo

    cascade_in(:)%ipi=0
    cascade_in(:)%vpi=0
    do i=1,cfg_data_size
        !统计ipi和vpi，以便下一步分配内存
        select case(defect_type(i))
        case(2)
            cascade_in(ion_index(i))%ipi=cascade_in(ion_index(i))%ipi+1
        case(3)
            cascade_in(ion_index(i))%vpi=cascade_in(ion_index(i))%vpi+1
        end select
    enddo
    
    do i=1,ion_database_size(i_beam)
        !分配内存,并设置默认坐标
        allocate(cascade_in(i)%vac_coord(3,cascade_in(i)%vpi))
        allocate(cascade_in(i)%SIA_coord(3,cascade_in(i)%ipi))
        cascade_in(i)%ipi=0
        cascade_in(i)%vpi=0
        cascade_in(i)%ion_coord=-10000.0
        cascade_in(i)%vac_coord=-10000.0
        cascade_in(i)%SIA_coord=-10000.0
    enddo
    
    do i=1,cfg_data_size
        !读取坐标
        select case(defect_type(i))
        case(1)
            cascade_in(ion_index(i))%ion_coord=coord(i,:)  
        case(2)
            cascade_in(ion_index(i))%ipi=cascade_in(ion_index(i))%ipi+1
            cascade_in(ion_index(i))%sia_coord(:,cascade_in(ion_index(i))%ipi)=coord(i,:)
        case(3)
            cascade_in(ion_index(i))%vpi=cascade_in(ion_index(i))%vpi+1
            cascade_in(ion_index(i))%vac_coord(:,cascade_in(ion_index(i))%vpi)=coord(i,:)
        end select
    enddo
    
    if (implant_direction /= damage_direction)then
         !交换implant_direction和damage_direction的坐标数据
        do i=1,ion_database_size(i_beam)
            cascade_in(i)%ion_coord([implant_direction,damage_direction])  =cascade_in(i)%ion_coord([damage_direction,implant_direction])
            cascade_in(i)%vac_coord([implant_direction,damage_direction],:)=cascade_in(i)%vac_coord([damage_direction,implant_direction],:)
            cascade_in(i)%SIA_coord([implant_direction,damage_direction],:)=cascade_in(i)%SIA_coord([damage_direction,implant_direction],:)
        enddo
    endif

end subroutine load_ion_database_cfg
    
subroutine load_ion_database_txt()
    !载入离子注入数据库
    use typ
    implicit none
    integer*4 GetFileN,i,j,int_dummy,ion_index,string_length,ion_file_lines,VAC_file_lines,SIA_file_lines,note_location,vpi_max,ipi_max
    real*8 pair_radius,ran1,ran2,ran3,alpha,beta,rad
    real*8 coord(3)
    character*300 dummy_string


    vpi_max=0
    ipi_max=0

    
    open(1000,file='ION.txt',STATUS='OLD')
    open(2000,file='VAC.txt',STATUS='OLD')
    open(3000,file='SIA.txt',STATUS='OLD')
    
    ion_file_lines=GetFileN(1000)                               !查询文件大小
    VAC_file_lines=GetFileN(2000)
    SIA_file_lines=GetFileN(3000)
    allocate(cascade(ion_file_lines))                           !分配内存
    ion_database_size(1)=0                                         !数据库大小计数器归零
    
    do i=1,ion_file_lines                                       !载入离子坐标
        read(1000,'(A300)')dummy_string
        call de_note(dummy_string)                                                  !提取有效内容
        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   !该行为无效行
        endif
    
        read(dummy_string,*)int_dummy,coord                   
        ion_database_size(1)=ion_database_size(1)+1
        
        cascade(ion_database_size(1))%ion_coord=coord  
        cascade(ion_database_size(1))%vpi=0                                            !损伤数归零
        cascade(ion_database_size(1))%ipi=0
        cascade(ion_database_size(1))%vac_coord=-10000.0
        cascade(ion_database_size(1))%SIA_coord=-10000.0
    enddo
    
    do i=1,VAC_file_lines                                                           !统计最大vpi值，确定内存分配空间
        read(2000,'(A300)')dummy_string
        call de_note(dummy_string)                                                  
        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   
        endif
    
        read(dummy_string,*)ion_index,coord
        if(coord(implant_direction)<0)cycle                                                         
        cascade(ion_index)%vpi=cascade(ion_index)%vpi+1
        if(cascade(ion_index)%vpi>vpi_max)vpi_max=cascade(ion_index)%vpi
    enddo
    
    do i=1,SIA_file_lines                                                           !统计最大ipi值，确定内存分配空间
        read(3000,'(A300)')dummy_string
        call de_note(dummy_string)                                                  
        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   
        endif
    
        read(dummy_string,*)ion_index,coord
        if(coord(implant_direction)<0)cycle
        cascade(ion_index)%ipi=cascade(ion_index)%ipi+1
        if(cascade(ion_index)%ipi>ipi_max)ipi_max=cascade(ion_index)%ipi
    enddo
    
    do i=1,ion_database_size(1)
        allocate(cascade(i)%vac_coord(3,vpi_max))
        allocate(cascade(i)%SIA_coord(3,ipi_max))
        cascade(i)%vpi=0
        cascade(i)%ipi=0
        cascade(i)%vac_coord=-10000.0
        cascade(i)%SIA_coord=-10000.0
    enddo
    
    rewind(2000)
    do i=1,VAC_file_lines                                                           !载入vac坐标
        read(2000,'(A300)')dummy_string
        call de_note(dummy_string)                                                  !提取有效内容
        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   !该行为无效行
        endif
    
        read(dummy_string,*)ion_index,coord
        if(coord(implant_direction)<0)cycle                                                         !小于0代表该离子没有产生损伤
        cascade(ion_index)%vpi=cascade(ion_index)%vpi+1
        cascade(ion_index)%vac_coord(:,cascade(ion_index)%vpi)=coord
    enddo
    
    rewind(3000)
    do i=1,SIA_file_lines                                                           !载入SIA坐标
        read(3000,'(A300)')dummy_string
        call de_note(dummy_string)                                                  !提取有效内容
        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   !该行为无效行
        endif
    
        read(dummy_string,*)ion_index,coord
        if(coord(implant_direction)<0)cycle
        cascade(ion_index)%ipi=cascade(ion_index)%ipi+1
        cascade(ion_index)%SIA_coord(:,cascade(ion_index)%ipi)=coord
    enddo
    
    if (implant_direction /= damage_direction)then
         !交换implant_direction和damage_direction的坐标数据
        do i=1,ion_database_size(1)
            cascade(i)%ion_coord([implant_direction,damage_direction])  =cascade(i)%ion_coord([damage_direction,implant_direction])
            cascade(i)%vac_coord([implant_direction,damage_direction],:)=cascade(i)%vac_coord([damage_direction,implant_direction],:)
            cascade(i)%SIA_coord([implant_direction,damage_direction],:)=cascade(i)%SIA_coord([damage_direction,implant_direction],:)
        enddo
    endif
    
    close(1000)
    close(2000)
    close(3000)  
    end subroutine load_ion_database_txt