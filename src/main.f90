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
        call file_check()
        call load_input()                   !载入边界条件
        call load_control()                 !载入控制矩阵
        call load_ion_database()            !载入离子辐照数据库
        call load_para()                    !定义部分动力学参数
        call pre_calcul()                   !预计算常用变量
        call initiate()                     !数据结构和构型初始化
        call sys_check()                    !系统参数自检查
        call output_settings()              !输出参数设置
        !call para_test()                   !手动参数测试（debug功能）
        
        call ctrl_sequence()                !模拟主体
        
        write(10,*)
        write(10,*)'All simulation units completed'
        write(10,*)'Total CPU time=',time2-time0
        close(10)
    endselect
    end
