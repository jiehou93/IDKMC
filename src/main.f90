    program okmc
    !������
    use typ
    implicit none

    nullify(root)                           !��ն��������ڵ�

    select case(main_procedure)             !ѡ��ģ��������ģ��
    case(0)
        call debug()
    case(1)
        open(10,file='monitor.txt')
        call file_check()
        call load_input()                   !����߽�����
        call load_control()                 !������ƾ���
        call load_ion_database()            !�������ӷ������ݿ�
        call load_para()                    !���岿�ֶ���ѧ����
        call pre_calcul()                   !Ԥ���㳣�ñ���
        call initiate()                     !���ݽṹ�͹��ͳ�ʼ��
        call sys_check()                    !ϵͳ�����Լ��
        call output_settings()              !�����������
        !call para_test()                   !�ֶ��������ԣ�debug���ܣ�
        
        call ctrl_sequence()                !ģ������
        
        write(10,*)
        write(10,*)'All simulation units completed'
        write(10,*)'Total CPU time=',time2-time0
        close(10)
    endselect
    end
