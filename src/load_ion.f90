subroutine load_ion_database()
    !����damage_type�����ͣ�ѡ�����ӷ������ݿ�������ļ�
    use typ
    implicit none
        
    write(*,*)'Loading ion implantation database...'
    if(damage_type=='txt')then
        write(10,*)'Loading ion implantation database from ION/VAC/SIA.txt files...'
        call load_ion_database_txt()
    elseif(damage_type=='cfg')then    
        write(10,*)'Converting aiv.xyz.cfg file to ION/VAC/SIA.txt files...'
        call cfg2okmc()
        call load_ion_database_txt()
        !write(10,*)'Deleting ION/VAC/SIA.txt files...'
    else
        write(10,*)'Error, damage_type not supported, please choose txt or cfg.'
        stop
    endif
        
    write(*,*)'Database loaded'    
end subroutine load_ion_database
    
subroutine cfg2okmc()
!��aiv.xyz.cfg ת��Ϊ ION/VAC/SIA.txt 
    use typ
    implicit none
    integer n_ion
    integer*4 GetFileN,i,j,k,ierr,filelines,ion_previous(3)
    real*4 box_scale,box(3)
    real*4,allocatable::coord(:,:)
    integer*4,allocatable::defect_type(:,:)
    character*160 path,content_string,makepath,c_dummy
    
    open(1000,file='aiv.xyz.cfg',STATUS='OLD')                       !��ԭ�ļ�
    open(1001,file='ION.txt')
    open(1002,file='SIA.txt')
    open(1003,file='VAC.txt')
    
    filelines=GetFileN(1000) 
    allocate(coord(filelines-19,3))
    allocate(defect_type(filelines-19,2))
    do i=1,19                                           
        !��ȡǰ19�еĺ��Ӳ���
        read(1000,'(A160)') content_string
        if(i==2) read(content_string,*)c_dummy,c_dummy,box_scale
        if(i==3) read(content_string(11:),*)box(1)
        if(i==7) read(content_string(11:),*)box(2)
        if(i==11)read(content_string(11:),*)box(3)
    enddo
    box=box*box_scale
    
    do  i=20,filelines
        !��ȡ����cascade
        read(1000,*)coord(i-19,:),defect_type(i-19,:)
    enddo
    
    do i=1,3
        !�����������
        coord(:,i)=coord(:,i)*box(i)
    enddo
    defect_type=defect_type+1
    !1/2/3�ֱ���� ion sia vac
    
    ion_previous=0
    !if(defect_type(1,2)>1)then
    !    write(1001,*)1,-10000,-10000,-10000
    !    write(1002,*)1,-10000,-10000,-10000
    !    write(1003,*)1,-10000,-10000,-10000
    !endif
    
    do i=1,filelines-19
        if(defect_type(i,2)==ion_previous(defect_type(i,1)))then
            !����һͬ��ȱ������ͬһcascade
            write(1000+defect_type(i,1),*)defect_type(i,2),coord(i,3),coord(i,1),coord(i,2)
        else
            !����һͬ��ȱ�����ڲ�ͬcascade
            if(defect_type(i,2)==ion_previous(defect_type(i,1))+1)then
                !������һcascade������ȱ��
                write(1000+defect_type(i,1),*)defect_type(i,2),coord(i,3),coord(i,1),coord(i,2)
                ion_previous(defect_type(i,1))=ion_previous(defect_type(i,1))+1
            else
                !��һcascadeû�в�������ȱ�ݣ��Զ������ȱ��
                do while(defect_type(i,2)>ion_previous(defect_type(i,1))+1)
                    ion_previous(defect_type(i,1))=ion_previous(defect_type(i,1))+1
                    write(1000+defect_type(i,1),*)ion_previous(defect_type(i,1)),-10000,-10000,-10000
                enddo
                write(1000+defect_type(i,1),*)defect_type(i,2),coord(i,3),coord(i,1),coord(i,2)
                ion_previous(defect_type(i,1))=ion_previous(defect_type(i,1))+1
            endif
        endif
    enddo
    

    close(1000)
    close(1002)
    close(1003)
    close(1004)
end  subroutine cfg2okmc
    
subroutine load_ion_database_txt()
    !��������ע�����ݿ�
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
    
    ion_file_lines=GetFileN(1000)                               !��ѯ�ļ���С
    VAC_file_lines=GetFileN(2000)
    SIA_file_lines=GetFileN(3000)
    allocate(cascade(ion_file_lines))                           !�����ڴ�
    ion_database_size=0                                         !���ݿ��С����������
    
    do i=1,ion_file_lines                                       !������������
        read(1000,'(A300)')dummy_string
        call de_note(dummy_string)                                                  !��ȡ��Ч����
        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   !����Ϊ��Ч��
        endif
    
        read(dummy_string,*)int_dummy,coord                   
        ion_database_size=ion_database_size+1
        
        cascade(ion_database_size)%ion_coord=coord  
        cascade(ion_database_size)%vpi=0                                            !����������
        cascade(ion_database_size)%ipi=0
        cascade(ion_database_size)%vac_coord=-10000.0
        cascade(ion_database_size)%SIA_coord=-10000.0
    enddo
    
    do i=1,VAC_file_lines                                                           !ͳ�����vpiֵ��ȷ���ڴ����ռ�
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
    
    do i=1,SIA_file_lines                                                           !ͳ�����ipiֵ��ȷ���ڴ����ռ�
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
    
    do i=1,ion_database_size
        allocate(cascade(i)%vac_coord(3,vpi_max))
        allocate(cascade(i)%SIA_coord(3,ipi_max))
        cascade(i)%vpi=0
        cascade(i)%ipi=0
        cascade(i)%vac_coord=-10000.0
        cascade(i)%SIA_coord=-10000.0
    enddo
    
    rewind(2000)
    do i=1,VAC_file_lines                                                           !����vac����
        read(2000,'(A300)')dummy_string
        call de_note(dummy_string)                                                  !��ȡ��Ч����
        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   !����Ϊ��Ч��
        endif
    
        read(dummy_string,*)ion_index,coord
        if(coord(implant_direction)<0)cycle                                                         !С��0���������û�в�������
        cascade(ion_index)%vpi=cascade(ion_index)%vpi+1
        cascade(ion_index)%vac_coord(:,cascade(ion_index)%vpi)=coord
    enddo
    
    rewind(3000)
    do i=1,SIA_file_lines                                                           !����SIA����
        read(3000,'(A300)')dummy_string
        call de_note(dummy_string)                                                  !��ȡ��Ч����
        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   !����Ϊ��Ч��
        endif
    
        read(dummy_string,*)ion_index,coord
        if(coord(implant_direction)<0)cycle
        cascade(ion_index)%ipi=cascade(ion_index)%ipi+1
        cascade(ion_index)%SIA_coord(:,cascade(ion_index)%ipi)=coord
    enddo
    
    close(1000)
    close(2000)
    close(3000)  
end subroutine load_ion_database_txt