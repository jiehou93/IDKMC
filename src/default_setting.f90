    module default_setting 
    !默认参数设置
    implicit none

    character*30,save::cfg_type='txt'
    character*30,save::damage_type='txt'
    character*30,save::lattice_type='bcc'
    
    integer*4,parameter::main_procedure=1                                           !主进程选择0、调试；1、OKMC模拟
    integer*4,parameter::element=4                                                  !考虑的缺陷种数（SIA和空位视为一类）
    integer*4,save::irr_status=1                                                    !辐照功能开关，默认为1表示开启，若未提供辐照损伤文件，自动关闭（设为0），此时所有辐照通量会归0
    
    real*8,save::a0=3.1652                                                          !晶格常数
    integer*4,save::pbc(3)=(/1,1,0/)                                                !默认2维PBC
    real*8,save::length(3)=(/0,0,0/)                                                !盒子的边长，默认为0，若手动设置该参数，会自动生成大小元胞网格，并忽略大小元胞的手动设置  
    real*8,save::cell_size(3)=(/10,10,10/)                                          !元胞边长
    integer*4,save::cell_number(3)=(/0,0,0/)                                        !元胞列表三个维度上的个数
    real*8,save::large_cell_size(3)=(/20,20,20/)                                    !大元胞边长
    integer*4,save::large_cell_number(3)=(/0,0,0/)                                  !大元胞列表三个维度上的个数
    real*8,save::critical_radius=0                                                  !区分大小缺陷的临界半径,默认值为min(cell_size)/2
    real*8,save::surface_depth=3.1652    
    
    real*8,save::concen(element)=(/0,0,0,0/)                                        !默认无初始缺陷                               
    real*8,save::grain_radi=1e15                                                    !球形晶界半径
    integer*4,save::intrinsic_type=0                                                !本征缺陷的类型,SIA/SIA-VAC/VAC   
    integer*4,save::ion_type=2	                                                    !注入离子类型，默认为第二个元素
	integer*4,save::iso_eff=0                                                       !同位素效应，0/1表示关闭/开启
    integer*4,save::uniform_damage(3)=(/1,1,0/)                                     !辐照级联是否在三个维度均匀分布，是为1，否为0，不影响级联内部的空间关联
    integer*4,save::implant_direction=3												!OKMC中辐照注入方向，默认从第3个方向注入
    integer*4,save::damage_direction=3										    !数据库中辐照注入方向，默认从第3个方向注入
    integer*4,save::rd_seed(4)=(/0,0,0,0/)                                          !随机数种子，默认为随机值，手动设置可使模拟结果具有可重复性，种子数量为4，部分编译器只需要2个种子，此时只有前两个种子生效
    integer*4,save::output_rate=0                                                   !缺陷构型输出时是否添加速率信息，默认为0，若设为1，则会额外输出团簇的速率信息
    
    
    end module default_setting