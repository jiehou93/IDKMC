    module global
    !ȫ�ֱ�������ģ��
    use default_setting
    implicit none

    real*8,save,allocatable::ion_para(:,:,:,:,:)        !���в����б�

    !�������ϳ���Ӧ��Ϊ����ֵ����
    real*8,parameter::pi=3.1415926                      !Բ����
    real*8,parameter::a0=3.1652                         !������
    real*8,parameter::r1=a0/2                           !�뾧����
    real*8,parameter::r0=2.7411436                      !������ԭ�Ӿ���
    real*8,parameter::v0=6.1e12                         !��ԭ�ӻ���Ƶ��
    real*8,parameter::vh=5.0467e+013                    !��ԭ�ӻ���Ƶ��
    real*8,parameter::kb=8.6173324e-5                   !������������
    

    real*8,save::timer                                  !ģ���ʱ��
    real*8,save::time0                                  !CPU��ʱ��
    real*8,save::time1                                  
    real*8,save::time2                              
    real*8,save::tem                                    !�¶�
    real*8,save::damage_rate=0                          !��������
    real*8,save::move(3,8)                              !8�������Ǩ��ʸ��
    real*8,save::surface_area=0                         !ע�뷽������
    integer*4,save::nformula(element)                   !ȱ��ͳ������
    integer*4,save::para_range(4,2)                     !���������ַ�Χ

    !integer*4,allocatable,save::cell(:,:,:,:)           !Ԫ���б�
    !integer*4,allocatable,save::large_cell(:,:,:,:)     !��Ԫ���б�
    integer*4,allocatable,save::neib_cell(:,:,:,:,:)    !Ԫ�������б�
    integer*4,allocatable,save::large_neib_cell(:,:,:,:,:)!��Ԫ�������б�

    integer*4,save::nctrl                               !����ģ�鵥λ����
    integer*4,save::ndef                                !��ȱ������
    integer*4,save::nclu                                !�Ŵ�����
    integer*4,save::level                               !����������
    integer*4,save::height                              !�������ܸ߶�
    integer*4,save::ion_database_size                   !����ע�����ݿ��С
    integer*4,save::defect_remain(4)                    !������ȱ����
    integer*4,save::defect_transmitted(4)               !͸���ȱ����
    integer*4,save::defect_released(4)                  !�Ѹ���ȱ����
    integer*4,save::grain_released(4)                   !�������յ�ȱ����
    end module global

    module typ
    !�Զ������ݽṹ
    use global
    implicit none

    !Ԫ������
    type cell_list                                      !�����һ���ڵ�ֻ�洢�����������洢������Ϣ
        integer*4 counter                               !������
        type(cluster),pointer::clu                      !ָ��ĳ��cluster
        type(cell_list),pointer::previous
        type(cell_list),pointer::next                   !
    end type cell_list
    type(cell_list),allocatable,target,save::cell(:,:,:)           !Ԫ������
    type(cell_list),allocatable,target,save::large_cell(:,:,:)     !��Ԫ������
    
    !ȱ���Ŵ�
    type cluster                                        
        type(tree),pointer::up                          !���������Ҷ��ָ������
        type(cell_list),pointer:: index                 !��Ԫ���е�����ָ��
        integer*4 sn                                    !�Ŵر��
        integer*4 n                                     !��С
        integer*4 orien                                 !��λ
        integer*4 emit                                  !����ȱ������
        integer*4 formula(element)                      !���
        real*8 coord(3)                                 !�������
        real*8 rate(3)                                  !����Ӧ����
        real*8 em                                       !Ǩ����
        real*8 er                                       !ת����
        real*8 eb                                       !������
        real*8 r                                        !�뾶
        real*8 vm                                       !Ǩ�Ƴ���Ƶ��
        real*8 ve                                       !���볢��Ƶ��
        real*8 step                                     !Ǩ�Ʋ���
    end type cluster
    type(cluster),target,save,allocatable::clu(:)       !�Ŵ��б�
    type(cluster),save::clunull

    !���������
    type tree                                           !
        real*8 rate                                     !�ýڵ������
        type(cluster),pointer::obj                      !ָ������ָ�룬ֻ����Ҷ���õ�
        type(tree),pointer::up                          !ָ�򸸽ڵ��ָ��
        type(tree),pointer::left                        !ָ����֦��ָ��
        type(tree),pointer::right                       !ָ����֦��ָ��
    end type tree
    type(tree),pointer,save::root                       !�����ڵ�
    type(tree),pointer:: current_previous,current       !����һ����ʱָ���ͽڵ�

    !ȱ�������Ӻ����������������
    type ion_damage                                 
        integer*4 vpi,ipi                               !vacancy per ion,interstitial per ion
        real*4 ion_coord(3)
        real*4,allocatable:: vac_coord(:,:)
        real*4,allocatable:: SIA_coord(:,:)
    end type ion_damage
    type(ion_damage),save,allocatable::cascade(:)

    !ģ�����ģ��ؼ�����
    type ctrl_module                                       
        real*8 tem                                      !�¶�
        real*8 time                                     !ʱ��
        real*8 irr_flux                                 !����ͨ��
        character*300 name                              !ģ����
        integer*4 outp                                  !����о�
    end type ctrl_module
    type(ctrl_module),save,allocatable::ctrl_matrix(:)
    contains
    subroutine replace(clu1,clu2)
    !����һ���滻���򣬽���clu2�и�Ԫ�ص�ֵ��ֵ��clu1����ӦԪ�أ�ֻ����ָ������up�Ͷ�����sn����
    type(cluster),target:: clu1,clu2
    clu1%n=clu2%n
    clu1%index=>clu2%index
    if(associated(clu1%index))then
        clu1%index%clu=>clu1
    endif
    clu1%orien=clu2%orien
    clu1%emit=clu2%emit
    clu1%step=clu2%step
    clu1%formula=clu2%formula
    clu1%coord=clu2%coord
    clu1%rate=clu2%rate
    clu1%em=clu2%em                                        
    clu1%er=clu2%er
    clu1%r=clu2%r
    clu1%vm=clu2%vm
    clu1%ve=clu2%ve
    clu1%eb=clu2%eb
    end subroutine replace

    subroutine backup(clu1,clu2)
    !����һ�����ݺ�������clu2�е���Ϣ��ȫ���ݵ�clu1��ȥ(����ָ��Ļ������Ӻͱ��)
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
    clu1%emit=clu2%emit
    clu1%step=clu2%step
    clu1%formula=clu2%formula
    clu1%coord=clu2%coord
    clu1%rate=clu2%rate
    clu1%em=clu2%em                                        
    clu1%er=clu2%er
    clu1%r=clu2%r
    clu1%vm=clu2%vm
    clu1%ve=clu2%ve
    clu1%eb=clu2%eb
    end subroutine backup

    end module typ