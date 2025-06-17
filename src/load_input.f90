    subroutine load_input()
    !读取边界条。
    implicit none
    integer*4 GetFileN,file_lines,i,j,k,string_length,note_location,equa_location
    character*300 dummy_string,variable_name,variable_value

    open(1001,file='INPUT.txt',STATUS='OLD')
    file_lines=GetFileN(1001)

    do i=1,file_lines                                                               !读取边界参数
        read(1001,'(A300)')dummy_string

        call de_note(dummy_string)                                                  !提取有效内容
        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   !该行为无效行
        endif

        equa_location=index(dummy_string(1:string_length),'=')                      !判断等号的位置，以此提取变量名和变量值
        variable_name=trim(adjustl(dummy_string(1:equa_location-1)))                !去两端空格后的变量名
        variable_value=trim(adjustl(dummy_string(equa_location+1:string_length)))   !去两端空格后的变量值（字符串）
        call assign_variable(variable_name,variable_value)
    enddo
    close(1001)

    end subroutine load_input

    subroutine assign_variable(variable_name,variable_value)
    !将variable_value显示的数值赋值给variable_name对应的变量。
    use default_setting
    implicit none
    character*300 variable_name,variable_value

    select case(trim(variable_name))
    case('rd_seed','RD_SEED')                                               !随机数种子，默认为随机值，手动设置可使模拟结果具有可重复性，种子数量为4，部分编译器只需要2个种子，此时只有前两个种子生效
        read(variable_value,*)rd_seed        
    case('pbc','PBC')                                                       !三个维度上的周期性边界条件，1为采用周期性边界条件，默认值为1 1 0
        read(variable_value,*)pbc                                           
    case('surface_depth','SURFACE_DEPTH')                                   !表面吸附阱离盒子表面的距离，默认值为3.1652
        read(variable_value,*)surface_depth 
    case('box_length','BOX_LENGTH')                                         !模拟盒子尺寸
        read(variable_value,*)length(1:3)    
    case('cell_size','CELL_SIZE')                                           !单个元胞的大小
        read(variable_value,*)cell_size(1:3)                       
    case('cell_number','CELL_NUMBER')                                       !三个维度上的元胞个数
        read(variable_value,*)cell_number(1),cell_number(2),cell_number(3)
    case('critical_radius','CRITICAL_RADIUS')                               !缺陷索引至大小元胞列表的半径判据，默认值为cell_size最小值的一半
        read(variable_value,*)critical_radius                               
    case('large_cell_size','LARGE_CELL_SIZE')                               !单个大元胞的大小
        read(variable_value,*)large_cell_size(1:3)    
    case('large_cell_number','LARGE_CELL_NUMBER')                           !三个维度上的大元胞个数
        read(variable_value,*)large_cell_number(1),large_cell_number(2),large_cell_number(3)
    case('grain_radius','GRAIN_RADIUS')                                     !晶粒半径，默认值为1e15
        read(variable_value,*)grain_radi     
    case('initial_defect','INITIAL_DEFECT')                                 !初始时随机分布在体系中的缺陷，默认值为0 0 0 0
        read(variable_value,*)concen(1:4)     
    case('intrinsic_type','INTRINSIC_TYPE')                                 !初始随机添加本征缺陷的类型，-1/0/1代表仅SIA/frenkel对/仅VAC，默认值为0
        read(variable_value,*)intrinsic_type
    case('uniform_damage','UNIFORM_DAMAGE')                                 !辐照级联是否在三个维度均匀分布，是为1，否为0，不影响级联内部的空间关联，默认值为1 1 0
        read(variable_value,*)uniform_damage
    case('implant_direction','IMPLANT_DIRECTION')                           !辐照注入方向，默认从第3个方向注入
        read(variable_value,*)implant_direction       
	case('ion_type','ION_TYPE')                                             !注入离子类型，默认为第二个元素
        read(variable_value,*)ion_type  
	case('iso_eff','ISO_EFF')                           						!同位素效应，0/1表示关闭/开启
        read(variable_value,*)iso_eff  
    case('cfg_type','CFG_TYPE')                           					!构型输入/输出格式，可设置为txt/lmp，默认为txt
        read(variable_value,*)cfg_type  
    case('damage_type','DAMAGE_TYPE')                           				!辐照损伤文件格式，可设置为txt/cfg，默认为txt
        read(variable_value,*)damage_type 
    case('output_rate','OUTPUT_RATE')                                       !缺陷构型输出时是否添加速率信息
        read(variable_value,*)output_rate
    case('a0','A0')                                                         !晶格常数
        read(variable_value,*)a0
    case('lattice_type','LATTICE_TYPE')                                     !晶格类型
        read(variable_value,*)lattice_type
    case('damage_direction','DAMAGE_DIRECTION')
        read(variable_value,*)damage_direction
    end select


    end subroutine assign_variable