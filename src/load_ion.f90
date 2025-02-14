subroutine load_ion_database()
    !根据damage_type的类型，选择离子辐照数据库的输入文件
    use typ
    implicit none
        
    if(irr_status==0)then
        write(10,*)'    Implantation database files incomplete, disabling irradiation function.....'
        return
    endif
    
    if(damage_type=='txt')then
        write(10,*)'    Loading ion implantation database from ION/VAC/SIA.txt files...'
        call load_ion_database_txt()
    elseif(damage_type=='cfg')then    
        write(10,*)'    Converting aiv.xyz.cfg file to ION/VAC/SIA.txt files...'
        call load_ion_database_cfg()
        !call cfg2okmc()
        !call load_ion_database_txt()
        !write(10,*)'Deleting ION/VAC/SIA.txt files...'
    else
        write(10,*)'    Error, damage_type not supported, please choose txt or cfg.'
        stop
    endif
end subroutine load_ion_database

subroutine load_ion_database_cfg()    
    !载入离子注入数据库
    use typ
    implicit none
    integer n_ion
    integer*4 GetFileN,i,j,k,ierr,filelines,cfg_data_size
    real*4 box_scale,box(3)
    real*4,allocatable::coord(:,:)
    integer*4,allocatable::defect_type(:),ion_index(:)
    character*160 path,content_string,makepath,c_dummy
    
    open(1000,file='aiv.xyz.cfg',STATUS='OLD')                       !打开原文件
    
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
    ion_database_size=maxval(ion_index)
    allocate(cascade(ion_database_size))
    
    do i=1,3
        !计算绝对坐标
        coord(:,i)=coord(:,i)*box(i)
    enddo

    cascade(:)%ipi=0
    cascade(:)%vpi=0
    do i=1,cfg_data_size
        !统计ipi和vpi，以便下一步分配内存
        select case(defect_type(i))
        case(2)
            cascade(ion_index(i))%ipi=cascade(ion_index(i))%ipi+1
        case(3)
            cascade(ion_index(i))%vpi=cascade(ion_index(i))%vpi+1
        end select
    enddo
    
    do i=1,ion_database_size
        !分配内存,并设置默认坐标
        allocate(cascade(i)%vac_coord(3,cascade(i)%vpi))
        allocate(cascade(i)%SIA_coord(3,cascade(i)%ipi))
        cascade(i)%ipi=0
        cascade(i)%vpi=0
        cascade(i)%ion_coord=-10000.0
        cascade(i)%vac_coord=-10000.0
        cascade(i)%SIA_coord=-10000.0
    enddo
    
    do i=1,cfg_data_size
        !读取坐标
        select case(defect_type(i))
        case(1)
            cascade(ion_index(i))%ion_coord=coord(i,:)  
        case(2)
            cascade(ion_index(i))%ipi=cascade(ion_index(i))%ipi+1
            cascade(ion_index(i))%sia_coord(:,cascade(ion_index(i))%ipi)=coord(i,:)
        case(3)
            cascade(ion_index(i))%vpi=cascade(ion_index(i))%vpi+1
            cascade(ion_index(i))%vac_coord(:,cascade(ion_index(i))%vpi)=coord(i,:)
        end select
    enddo
    
end subroutine load_ion_database_cfg
    
subroutine cfg2okmc()
!将aiv.xyz.cfg 转换为 ION/VAC/SIA.txt 
    use typ
    implicit none
    integer n_ion
    integer*4 GetFileN,i,j,k,ierr,filelines,ion_previous(3),ion_miss(3)
    real*4 box_scale,box(3)
    real*4,allocatable::coord(:,:)
    integer*4,allocatable::defect_type(:),ion_index(:)
    character*160 path,content_string,makepath,c_dummy
    
    open(1000,file='aiv.xyz.cfg',STATUS='OLD')                       !打开原文件
    open(1001,file='ION.txt')
    open(1002,file='SIA.txt')
    open(1003,file='VAC.txt')
    
    filelines=GetFileN(1000) 
    allocate(coord(filelines-19,3))
    allocate(defect_type(filelines-19))
    allocate(ion_index(filelines-19))
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
        !读取所有参数
        read(1000,*)coord(i,:),defect_type(i),ion_index(i)
    enddo
    
    do i=1,3
        !计算绝对坐标
        coord(:,i)=coord(:,i)*box(i)
    enddo
    defect_type=defect_type+1
    !1/2/3分别代表 ion sia vac
    ion_index=ion_index+1
    
    ion_previous=0
    do i=1,filelines-19
        if(ion_index(i)==ion_previous(defect_type(i)))then
            !与上一同类缺陷属于同一cascade
            write(1000+defect_type(i),*)ion_index(i),coord(i,:)
        else
            !与上一同类缺陷属于不同cascade
            if(ion_index(i)==ion_previous(defect_type(i))+1)then
                !属于下一cascade产生的缺陷
                write(1000+defect_type(i),*)ion_index(i),coord(i,:)
                ion_previous(defect_type(i))=ion_previous(defect_type(i))+1
            else
                !下一cascade没有产生该类缺陷，自动填入空缺陷
                do while(ion_index(i)>ion_previous(defect_type(i))+1)
                    ion_previous(defect_type(i))=ion_previous(defect_type(i))+1
                    write(1000+defect_type(i),*)ion_previous(defect_type(i)),-10000,-10000,-10000
                enddo
                write(1000+defect_type(i),*)ion_index(i),coord(i,:)
                ion_previous(defect_type(i))=ion_previous(defect_type(i))+1
            endif
        endif
    enddo
    
    !根据最后记录的最大ION,补完三种缺陷，保证数据量相等
    ion_miss=maxval(ion_previous)-ion_previous
    do i=1,3
        do j=1,ion_miss(i)
            write(1000+i,*)ion_previous(i)+j,-10000,-10000,-10000
        enddo
    enddo
        
    close(1000)
    close(1002)
    close(1003)
    close(1004)
end  subroutine cfg2okmc
    
subroutine load_ion_database_txt()
    !载入离子注入数据库
    use typ
    implicit none
    integer*4 GetFileN,i,j,int_dummy,ion_index,string_length,ion_file_lines,VAC_file_lines,SIA_file_lines
    real*8 coord(3)
    character*300 dummy_string
    
    open(1000,file='ION.txt',STATUS='OLD')
    open(2000,file='VAC.txt',STATUS='OLD')
    open(3000,file='SIA.txt',STATUS='OLD')
    
    ion_file_lines=GetFileN(1000)                               !查询文件大小
    VAC_file_lines=GetFileN(2000)
    SIA_file_lines=GetFileN(3000)
    allocate(cascade(ion_file_lines))                           !分配内存
    ion_database_size=0                                         !数据库大小计数器归零
    
    do i=1,ion_file_lines                                       !载入离子坐标
        read(1000,'(A300)')dummy_string
        call de_note(dummy_string)                                                  !提取有效内容
        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   !该行为无效行
        endif
    
        read(dummy_string,*)int_dummy,coord                   
        ion_database_size=ion_database_size+1
        
        cascade(ion_database_size)%ion_coord=coord  
        cascade(ion_database_size)%vpi=0                                            !损伤数归零
        cascade(ion_database_size)%ipi=0
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
    enddo
    
    do i=1,ion_database_size
        allocate(cascade(i)%vac_coord(3,cascade(i)%vpi))
        allocate(cascade(i)%SIA_coord(3,cascade(i)%ipi))
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
        !cascade(ion_index)%ipi=cascade(ion_index)%ipi+1
        cascade(ion_index)%SIA_coord(:,cascade(ion_index)%ipi)=coord
    enddo
    
    close(1000)
    close(2000)
    close(3000)  
end subroutine load_ion_database_txt