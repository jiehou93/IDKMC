    module global
    !全局变量定义模块
    use default_setting
    implicit none

    real*8,save,allocatable::para_table(:,:,:,:,:)        !已有参数列表

    real*8,parameter::pi=3.1415926                      !圆周率
    real*8,parameter::kb=8.6173324e-5                   !玻尔兹曼常量

    real*8,save::timer                                  !模拟计时器
    real*8,save::time0                                  !CPU计时器
    real*8,save::time1                                  
    real*8,save::time2                              
    real*8,save::tem                                    !温度
    real*8,save::damage_rate=0                          !损伤速率
    real*8,save,allocatable::move(:,:)                  !迁移矢量
    real*8,save::surface_area=0                         !注入方向表面积
    integer*4,save::nformula(element)                   !缺陷统计数组
    integer*4,save::ndirection                          !可选迁移方向数量，bcc为8，fcc为12
    integer*4,save::para_range(4,2)                     !读入参数组分范围

    !integer*4,allocatable,save::cell(:,:,:,:)           !元胞列表
    !integer*4,allocatable,save::large_cell(:,:,:,:)     !大元胞列表
    integer*4,allocatable,save::neib_cell(:,:,:,:,:)    !元胞近邻列表
    integer*4,allocatable,save::large_neib_cell(:,:,:,:,:)!大元胞近邻列表

    integer*4,save::nctrl                               !控制模块单位总数
    integer*4,save::ndef                                !点缺陷总数
    integer*4,save::nclu                                !团簇总数
    integer*4,save::level                               !二叉树级层
    integer*4,save::height                              !二叉树总高度
    integer*4,save::ion_database_size                   !离子注入数据库大小
    integer*4,save::defect_remain(4)                    !滞留的缺陷数
    integer*4,save::defect_transmitted(4)               !透射的缺陷数
    integer*4,save::defect_released(4)                  !脱附的缺陷数
    integer*4,save::grain_released(4)                   !晶界吸收的缺陷数
    end module global

    module typ
    !自定义数据结构
    use global
    implicit none

    !元胞链表
    type cell_list                                      !链表第一个节点只存储计数器，不存储其他信息
        integer*4 counter                               !计数器
        type(cluster),pointer::clu                      !指向某个cluster
        type(cell_list),pointer::previous
        type(cell_list),pointer::next                   !
    end type cell_list
    type(cell_list),allocatable,target,save::cell(:,:,:)           !元胞链表
    type(cell_list),allocatable,target,save::large_cell(:,:,:)     !大元胞链表
    
    !缺陷团簇
    type cluster                                        
        type(tree),pointer::up                          !与二叉树树叶的指针链接
        type(cell_list),pointer:: index                 !在元胞中的索引指针
        integer*4 sn                                    !团簇编号
        integer*4 n                                     !大小
        integer*4 orien                                 !方位
        integer*4 formula(element)                      !组分
        real*8 coord(3)                                 !相对坐标
        real*8 rate(3)                                  !各反应速率
        real*8 para(8)                                  !缺陷参数，1-8分别为E_mig, E_mig+E_rot, E_trap, radius, v_mig, v_emit, jump_distance, emit_type
    end type cluster
    type(cluster),target,save,allocatable::clu(:)       !团簇列表
    type(cluster),save::clunull

    !二叉查找树
    type tree                                           !
        real*8 rate                                     !该节点的速率
        type(cluster),pointer::obj                      !指向对象的指针，只有树叶会用到
        type(tree),pointer::up                          !指向父节点的指针
        type(tree),pointer::left                        !指向左枝的指针
        type(tree),pointer::right                       !指向右枝的指针
    end type tree
    type(tree),pointer,save::root                       !树根节点
    type(tree),pointer:: current_previous,current       !定义一个临时指针型节点

    !单个离子轰击产生的所有损伤
    type ion_damage                                 
        integer*4 vpi,ipi                               !vacancy per ion,interstitial per ion
        real*4 ion_coord(3)
        real*4,allocatable:: vac_coord(:,:)
        real*4,allocatable:: SIA_coord(:,:)
    end type ion_damage
    type(ion_damage),save,allocatable::cascade(:)

    !模拟控制模块关键参数
    type ctrl_module                                       
        real*8 tem                                      !温度
        real*8 time                                     !时间
        real*8 irr_flux                                 !辐照通量
        character*300 name                              !模块名
        integer*4 outp                                  !输出判据
    end type ctrl_module
    type(ctrl_module),save,allocatable::ctrl_matrix(:)
    contains
    subroutine replace(clu1,clu2)
    !定义一个替换程序，将将clu2中各元素的值赋值给clu1中相应元素，只保留指针链接up和对象编号sn不变
    type(cluster),target:: clu1,clu2
    clu1%n=clu2%n
    clu1%index=>clu2%index
    if(associated(clu1%index))then
        clu1%index%clu=>clu1
    endif
    clu1%orien=clu2%orien
    clu1%formula=clu2%formula
    clu1%coord=clu2%coord
    clu1%rate=clu2%rate
    clu1%para=clu2%para
    end subroutine replace

    subroutine backup(clu1,clu2)
    !定义一个备份函数，将clu2中的信息完全备份到clu1中去(包括指针的互相连接和编号)
    type(cluster),target:: clu1,clu2
    clu1%up=>clu2%up
    clu1%up%obj=>clu1
    clu1%sn=clu2%sn
    clu1%n=clu2%n
    clu1%index=>clu2%index
    if(associated(clu1%index))then
        clu1%index%clu=>clu1
    endif
    clu1%orien=clu2%orien
    clu1%formula=clu2%formula
    clu1%coord=clu2%coord
    clu1%rate=clu2%rate
    clu1%para=clu2%para
    end subroutine backup

    end module typ