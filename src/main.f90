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
        write(10,*)'Last update of IDKMC: 20250303'
        write(10,*)'Checking files...'
        write(*,*)'Checking files...'
        call file_check()
        write(10,*)'Loading INPUT file...'
        write(*,*)'Loading INPUT file...'
        call load_input()                   !����߽�����
        write(10,*)'Loading CONTROL file...'
        write(*,*)'Loading CONTROL file...'
        call load_control()                 !������ƾ���
        write(10,*)'Loading implantation database...'
        write(*,*)'Loading implantation database...'
        call load_ion_database()            !�������ӷ������ݿ�
        write(10,*)'Loading parameter database...'
        write(*,*)'Loading parameter database...'
        call load_para()                    !���岿�ֶ���ѧ����
        write(10,*)'Initiating system...'
        write(*,*)'Initiating system...'
        call initiate()                     !���ݽṹ�͹��ͳ�ʼ��
        write(10,*)'Running diagnostics...'
        write(*,*)'Running diagnostics...'
        call sys_check()                    !ϵͳ�����Լ��
        write(10,*)'Output settings...'
        write(*,*)'Output settings...'
        call output_settings()              !�����������
        !call para_test()                   !�ֶ��������ԣ�debug���ܣ�
        write(10,*)'All set! Starting simulation...'
        write(*,*)'All set! Starting simulation...'
        call ctrl_sequence()                !ģ������
        
        write(10,*)
        write(10,*)'All simulation units completed'
        write(10,*)'Total CPU time=',time2-time0
        close(10)
    endselect
    
    end
