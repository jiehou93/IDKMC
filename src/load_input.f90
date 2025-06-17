    subroutine load_input()
    !��ȡ�߽�����
    implicit none
    integer*4 GetFileN,file_lines,i,j,k,string_length,note_location,equa_location
    character*300 dummy_string,variable_name,variable_value

    open(1001,file='INPUT.txt',STATUS='OLD')
    file_lines=GetFileN(1001)

    do i=1,file_lines                                                               !��ȡ�߽����
        read(1001,'(A300)')dummy_string

        call de_note(dummy_string)                                                  !��ȡ��Ч����
        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   !����Ϊ��Ч��
        endif

        equa_location=index(dummy_string(1:string_length),'=')                      !�жϵȺŵ�λ�ã��Դ���ȡ�������ͱ���ֵ
        variable_name=trim(adjustl(dummy_string(1:equa_location-1)))                !ȥ���˿ո��ı�����
        variable_value=trim(adjustl(dummy_string(equa_location+1:string_length)))   !ȥ���˿ո��ı���ֵ���ַ�����
        call assign_variable(variable_name,variable_value)
    enddo
    close(1001)

    end subroutine load_input

    subroutine assign_variable(variable_name,variable_value)
    !��variable_value��ʾ����ֵ��ֵ��variable_name��Ӧ�ı�����
    use default_setting
    implicit none
    character*300 variable_name,variable_value

    select case(trim(variable_name))
    case('rd_seed','RD_SEED')                                               !��������ӣ�Ĭ��Ϊ���ֵ���ֶ����ÿ�ʹģ�������п��ظ��ԣ���������Ϊ4�����ֱ�����ֻ��Ҫ2�����ӣ���ʱֻ��ǰ����������Ч
        read(variable_value,*)rd_seed        
    case('pbc','PBC')                                                       !����ά���ϵ������Ա߽�������1Ϊ���������Ա߽�������Ĭ��ֵΪ1 1 0
        read(variable_value,*)pbc                                           
    case('surface_depth','SURFACE_DEPTH')                                   !��������������ӱ���ľ��룬Ĭ��ֵΪ3.1652
        read(variable_value,*)surface_depth 
    case('box_length','BOX_LENGTH')                                         !ģ����ӳߴ�
        read(variable_value,*)length(1:3)    
    case('cell_size','CELL_SIZE')                                           !����Ԫ���Ĵ�С
        read(variable_value,*)cell_size(1:3)                       
    case('cell_number','CELL_NUMBER')                                       !����ά���ϵ�Ԫ������
        read(variable_value,*)cell_number(1),cell_number(2),cell_number(3)
    case('critical_radius','CRITICAL_RADIUS')                               !ȱ����������СԪ���б�İ뾶�оݣ�Ĭ��ֵΪcell_size��Сֵ��һ��
        read(variable_value,*)critical_radius                               
    case('large_cell_size','LARGE_CELL_SIZE')                               !������Ԫ���Ĵ�С
        read(variable_value,*)large_cell_size(1:3)    
    case('large_cell_number','LARGE_CELL_NUMBER')                           !����ά���ϵĴ�Ԫ������
        read(variable_value,*)large_cell_number(1),large_cell_number(2),large_cell_number(3)
    case('grain_radius','GRAIN_RADIUS')                                     !�����뾶��Ĭ��ֵΪ1e15
        read(variable_value,*)grain_radi     
    case('initial_defect','INITIAL_DEFECT')                                 !��ʼʱ����ֲ�����ϵ�е�ȱ�ݣ�Ĭ��ֵΪ0 0 0 0
        read(variable_value,*)concen(1:4)     
    case('intrinsic_type','INTRINSIC_TYPE')                                 !��ʼ�����ӱ���ȱ�ݵ����ͣ�-1/0/1�����SIA/frenkel��/��VAC��Ĭ��ֵΪ0
        read(variable_value,*)intrinsic_type
    case('uniform_damage','UNIFORM_DAMAGE')                                 !���ռ����Ƿ�������ά�Ⱦ��ȷֲ�����Ϊ1����Ϊ0����Ӱ�켶���ڲ��Ŀռ������Ĭ��ֵΪ1 1 0
        read(variable_value,*)uniform_damage
    case('implant_direction','IMPLANT_DIRECTION')                           !����ע�뷽��Ĭ�ϴӵ�3������ע��
        read(variable_value,*)implant_direction       
	case('ion_type','ION_TYPE')                                             !ע���������ͣ�Ĭ��Ϊ�ڶ���Ԫ��
        read(variable_value,*)ion_type  
	case('iso_eff','ISO_EFF')                           						!ͬλ��ЧӦ��0/1��ʾ�ر�/����
        read(variable_value,*)iso_eff  
    case('cfg_type','CFG_TYPE')                           					!��������/�����ʽ��������Ϊtxt/lmp��Ĭ��Ϊtxt
        read(variable_value,*)cfg_type  
    case('damage_type','DAMAGE_TYPE')                           				!���������ļ���ʽ��������Ϊtxt/cfg��Ĭ��Ϊtxt
        read(variable_value,*)damage_type 
    case('output_rate','OUTPUT_RATE')                                       !ȱ�ݹ������ʱ�Ƿ����������Ϣ
        read(variable_value,*)output_rate
    case('a0','A0')                                                         !������
        read(variable_value,*)a0
    case('lattice_type','LATTICE_TYPE')                                     !��������
        read(variable_value,*)lattice_type
    case('damage_direction','DAMAGE_DIRECTION')
        read(variable_value,*)damage_direction
    end select


    end subroutine assign_variable