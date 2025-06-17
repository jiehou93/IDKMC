    module default_setting 
    !Ĭ�ϲ�������
    implicit none

    character*30,save::cfg_type='txt'
    character*30,save::damage_type='txt'
    character*30,save::lattice_type='bcc'
    
    integer*4,parameter::main_procedure=1                                           !������ѡ��0�����ԣ�1��OKMCģ��
    integer*4,parameter::element=4                                                  !���ǵ�ȱ��������SIA�Ϳ�λ��Ϊһ�ࣩ
    integer*4,save::irr_status=1                                                    !���չ��ܿ��أ�Ĭ��Ϊ1��ʾ��������δ�ṩ���������ļ����Զ��رգ���Ϊ0������ʱ���з���ͨ�����0
    
    real*8,save::a0=3.1652                                                          !������
    integer*4,save::pbc(3)=(/1,1,0/)                                                !Ĭ��2άPBC
    real*8,save::length(3)=(/0,0,0/)                                                !���ӵı߳���Ĭ��Ϊ0�����ֶ����øò��������Զ����ɴ�СԪ�����񣬲����Դ�СԪ�����ֶ�����  
    real*8,save::cell_size(3)=(/10,10,10/)                                          !Ԫ���߳�
    integer*4,save::cell_number(3)=(/0,0,0/)                                        !Ԫ���б�����ά���ϵĸ���
    real*8,save::large_cell_size(3)=(/20,20,20/)                                    !��Ԫ���߳�
    integer*4,save::large_cell_number(3)=(/0,0,0/)                                  !��Ԫ���б�����ά���ϵĸ���
    real*8,save::critical_radius=0                                                  !���ִ�Сȱ�ݵ��ٽ�뾶,Ĭ��ֵΪmin(cell_size)/2
    real*8,save::surface_depth=3.1652    
    
    real*8,save::concen(element)=(/0,0,0,0/)                                        !Ĭ���޳�ʼȱ��                               
    real*8,save::grain_radi=1e15                                                    !���ξ���뾶
    integer*4,save::intrinsic_type=0                                                !����ȱ�ݵ�����,SIA/SIA-VAC/VAC   
    integer*4,save::ion_type=2	                                                    !ע���������ͣ�Ĭ��Ϊ�ڶ���Ԫ��
	integer*4,save::iso_eff=0                                                       !ͬλ��ЧӦ��0/1��ʾ�ر�/����
    integer*4,save::uniform_damage(3)=(/1,1,0/)                                     !���ռ����Ƿ�������ά�Ⱦ��ȷֲ�����Ϊ1����Ϊ0����Ӱ�켶���ڲ��Ŀռ����
    integer*4,save::implant_direction=3												!OKMC�з���ע�뷽��Ĭ�ϴӵ�3������ע��
    integer*4,save::damage_direction=3										    !���ݿ��з���ע�뷽��Ĭ�ϴӵ�3������ע��
    integer*4,save::rd_seed(4)=(/0,0,0,0/)                                          !��������ӣ�Ĭ��Ϊ���ֵ���ֶ����ÿ�ʹģ�������п��ظ��ԣ���������Ϊ4�����ֱ�����ֻ��Ҫ2�����ӣ���ʱֻ��ǰ����������Ч
    integer*4,save::output_rate=0                                                   !ȱ�ݹ������ʱ�Ƿ����������Ϣ��Ĭ��Ϊ0������Ϊ1������������Ŵص�������Ϣ
    
    
    end module default_setting