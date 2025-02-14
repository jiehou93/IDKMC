    subroutine initiate()
    !初始化速率树，往体系中加入指定浓度的初始缺陷，或者从输入文件中读取初始缺陷构型
    use typ
    implicit none
    integer*4 i,j,n,orien,formula(element),num,GetFileN,file_lines,string_length,note_location,natom_in_lattice
    integer*4, allocatable :: seed(:)
    real*8 distance,ran1,ran2,ran3,ran4,alpha,beta,coord(3)
    character*300 dummy_string

    call pre_calcul()                   !预计算常用变量
    
    !设置随机数种子
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
        !注意bcc单胞内含2个原子，fcc含4个
        natom_in_lattice=2
        ndirection=8
        allocate(move(3,ndirection))
        move(:,1)=(/ 1, 1, 1/)                         !设置移动基矢，
        move(:,2)=(/-1,-1,-1/)
        move(:,3)=(/ 1,-1, 1/)
        move(:,4)=(/-1, 1,-1/)
        move(:,5)=(/ 1,-1,-1/)
        move(:,6)=(/-1, 1, 1/)
        move(:,7)=(/-1,-1, 1/)
        move(:,8)=(/ 1, 1,-1/)
		move=move/sqrt(3.0d0)                       !归一化移动基矢
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
		move=move/sqrt(2.0d0)                       !归一化移动基矢
    else
        write(10,*)'    Error! Only bcc or fcc lattice_types are supportted!'
        stop
    endif
   
    
    ndef=0
    do i=1,element                                                                  !预算每种类型缺陷的初始浓度以及缺陷总数
        nformula(i)=(length(1)*length(2)*length(3)*10**(-6.0d0)/(a0**3))*concen(i)  !注意运算顺序，避免中间运算数值超出4字节整型数的范围
        nformula(i)=nformula(i)*natom_in_lattice
        
        if(i==1.and.intrinsic_type==0)then
            ndef=ndef+2*nformula(i)             !i=1代表本征缺陷，本征缺陷是以frenkel对形式成双加入的。
        else
            ndef=ndef+nformula(i)
        endif
    enddo

    allocate(clu(0:ndef))                       !给所有对象分配内存空间
    clu(0)%sn=0                                 !0号对象代表外部事件被触发（辐照产生缺陷）
    nullify(clu(0)%index)
    clu(0)%rate(3)=damage_rate                  !外部事件反应速率（默认值为0）
    do i=0,ndef                               !给每个对象分配编号                           
        clu(i)%sn=i
        nullify(clu(i)%up)                      !up指针空指
    enddo

    allocate(clu(0)%up)                         !新建根节点，链接至clu(0)上
    root=>clu(0)%up
    root%obj=>clu(0)
    nullify(root%up,root%left,root%right)
    height=1

    ndef=0                                      !缺陷计数器归零
    nclu=0                                      !团簇计数器归零

    call load_cfg()           !从指定文件载入初始构型z

    !添加随机缺陷
    do i=element,1,-1
        do j=1,nformula(i)
            formula=0

            if(i==1)then
                select case(intrinsic_type)                                         !判断是添加SIA，VAC还是frenkel对
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
            else                                                                    !非本征缺陷
                formula(i)=1                    
                call random_add(formula) 
            endif
        enddo  
    enddo
    write(10,*)'    Configuration initiated'
    end subroutine initiate