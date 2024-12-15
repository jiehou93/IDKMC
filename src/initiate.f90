    subroutine initiate()
    !��ʼ��������������ϵ�м���ָ��Ũ�ȵĳ�ʼȱ�ݣ����ߴ������ļ��ж�ȡ��ʼȱ�ݹ���
    use typ
    implicit none
    integer*4 i,j,n,orien,formula(element),num,GetFileN,file_lines,string_length,note_location
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
    

    ndef=0
    do i=1,element                                                                  !Ԥ��ÿ������ȱ�ݵĳ�ʼŨ���Լ�ȱ������
        nformula(i)=(cell_number(1)*cell_number(2)*cell_number(3)*10**(-6.0d0)*cell_size(1)*cell_size(2)*cell_size(3)/(a0**3/2.0d0))*concen(i)!ע������˳�򣬱����м�������ֵ����4�ֽ��������ķ�Χ
        if(concen(i)<0) nformula(i)=1                                               !concen(i)<0��ʾ���һ��ȱ��
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