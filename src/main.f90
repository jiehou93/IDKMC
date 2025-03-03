    program okmc
    !主程序
    use typ
    implicit none

    nullify(root)                           !清空二叉树根节点

    select case(main_procedure)             !选择模拟主控制模块
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
        call load_input()                   !载入边界条件
        write(10,*)'Loading CONTROL file...'
        write(*,*)'Loading CONTROL file...'
        call load_control()                 !载入控制矩阵
        write(10,*)'Loading implantation database...'
        write(*,*)'Loading implantation database...'
        call load_ion_database()            !载入离子辐照数据库
        write(10,*)'Loading parameter database...'
        write(*,*)'Loading parameter database...'
        call load_para()                    !定义部分动力学参数
        write(10,*)'Initiating system...'
        write(*,*)'Initiating system...'
        call initiate()                     !数据结构和构型初始化
        write(10,*)'Running diagnostics...'
        write(*,*)'Running diagnostics...'
        call sys_check()                    !系统参数自检查
        write(10,*)'Output settings...'
        write(*,*)'Output settings...'
        call output_settings()              !输出参数设置
        !call para_test()                   !手动参数测试（debug功能）
        write(10,*)'All set! Starting simulation...'
        write(*,*)'All set! Starting simulation...'
        call ctrl_sequence()                !模拟主体
        
        write(10,*)
        write(10,*)'All simulation units completed'
        write(10,*)'Total CPU time=',time2-time0
        close(10)
    endselect
    
    end
