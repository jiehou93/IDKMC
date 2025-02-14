    subroutine initiate()
    !��ʼ��������������ϵ�м���ָ��Ũ�ȵĳ�ʼȱ�ݣ����ߴ������ļ��ж�ȡ��ʼȱ�ݹ���
    use typ
    implicit none
    integer*4 i,j,n,orien,formula(element),num,GetFileN,file_lines,string_length,note_location,natom_in_lattice
    integer*4, allocatable :: seed(:)
    real*8 distance,ran1,ran2,ran3,ran4,alpha,beta,coord(3)
    character*300 dummy_string

    call pre_calcul()                   !Ԥ���㳣�ñ���
    
    !�������������
    if(rd_seed(1)==0)then
        write(10,*)'    Using default random seeds...'
        call RANDOM_SEED()
        call random_seed(size = n)
        allocate(seed(n))
        call random_seed(get=seed)
        rd_seed(1:n)=seed
    else            
        write(10,*)'    Using customized random seeds...'
        call random_seed(size = n)
        allocate(seed(n))
        if (n==2)then
            seed=rd_seed(1:2)
        elseif(n==4)then
            seed=rd_seed
        else
            write(10,*)'    Random seed size not supported, please set as default!'
        endif
        
        call RANDOM_SEED(put=seed)
        call random_seed(get=seed)
        
    endif
    write (10, *) '    Random seeds set to:',seed
    

    if(lattice_type=='bcc')then
        !ע��bcc�����ں�2��ԭ�ӣ�fcc��4��
        natom_in_lattice=2
        ndirection=8
        allocate(move(3,ndirection))
        move(:,1)=(/ 1, 1, 1/)                         !�����ƶ���ʸ��
        move(:,2)=(/-1,-1,-1/)
        move(:,3)=(/ 1,-1, 1/)
        move(:,4)=(/-1, 1,-1/)
        move(:,5)=(/ 1,-1,-1/)
        move(:,6)=(/-1, 1, 1/)
        move(:,7)=(/-1,-1, 1/)
        move(:,8)=(/ 1, 1,-1/)
		move=move/sqrt(3.0d0)                       !��һ���ƶ���ʸ
    elseif(lattice_type=='fcc')then
        natom_in_lattice=4
        ndirection=12
        allocate(move(3,ndirection))
        move(:,1) =(/ 1, 1, 0/)
        move(:,2) =(/-1,-1, 0/)
        move(:,3) =(/ 1,-1, 0/)
        move(:,4) =(/-1, 1, 0/)
        move(:,5) =(/ 1, 0, 1/)
        move(:,6) =(/-1, 0,-1/)
        move(:,7) =(/ 1, 0,-1/)
        move(:,8) =(/-1, 0, 1/)
        move(:,9) =(/ 0, 1, 1/)
        move(:,10)=(/ 0,-1,-1/)
        move(:,11)=(/ 0, 1,-1/)
        move(:,12)=(/ 0,-1, 1/)
		move=move/sqrt(2.0d0)                       !��һ���ƶ���ʸ
    else
        write(10,*)'    Error! Only bcc or fcc lattice_types are supportted!'
        stop
    endif
   
    
    ndef=0
    do i=1,element                                                                  !Ԥ��ÿ������ȱ�ݵĳ�ʼŨ���Լ�ȱ������
        nformula(i)=(length(1)*length(2)*length(3)*10**(-6.0d0)/(a0**3))*concen(i)  !ע������˳�򣬱����м�������ֵ����4�ֽ��������ķ�Χ
        nformula(i)=nformula(i)*natom_in_lattice
        
        if(i==1.and.intrinsic_type==0)then
            ndef=ndef+2*nformula(i)             !i=1������ȱ�ݣ�����ȱ������frenkel����ʽ��˫����ġ�
        else
            ndef=ndef+nformula(i)
        endif
    enddo

    allocate(clu(0:ndef))                       !�����ж�������ڴ�ռ�
    clu(0)%sn=0                                 !0�Ŷ�������ⲿ�¼������������ղ���ȱ�ݣ�
    nullify(clu(0)%index)
    clu(0)%rate(3)=damage_rate                  !�ⲿ�¼���Ӧ���ʣ�Ĭ��ֵΪ0��
    do i=0,ndef                               !��ÿ�����������                           
        clu(i)%sn=i
        nullify(clu(i)%up)                      !upָ���ָ
    enddo

    allocate(clu(0)%up)                         !�½����ڵ㣬������clu(0)��
    root=>clu(0)%up
    root%obj=>clu(0)
    nullify(root%up,root%left,root%right)
    height=1

    ndef=0                                      !ȱ�ݼ���������
    nclu=0                                      !�Ŵؼ���������

    call load_cfg()           !��ָ���ļ������ʼ����z

    !������ȱ��
    do i=element,1,-1
        do j=1,nformula(i)
            formula=0

            if(i==1)then
                select case(intrinsic_type)                                         !�ж������SIA��VAC����frenkel��
                case(-1)
                    formula(i)=-1
                    call random_add(formula)
                case(0)
                    formula(i)=-1
                    call random_add(formula) 
                    formula(i)=1
                    call random_add(formula)
                case(1)
                    formula(i)=1
                    call random_add(formula) 
                end select
            else                                                                    !�Ǳ���ȱ��
                formula(i)=1                    
                call random_add(formula) 
            endif
        enddo  
    enddo
    write(10,*)'    Configuration initiated'
    end subroutine initiate