subroutine ctrl_sequence()
    !模拟氘滞留的TDS实验
    use typ
    implicit none
    integer*4 i,j,k

    open(101,file='defect_statistic.txt')
    write(101,'(18A10,A20)')'Temp','N_cluster','f1_remain','f2_remain','f3_remain','f4_remain',&
                                              &'f1_desorb','f2_desorb','f3_desorb','f4_desorb',&
                                              &'f1_trans' ,'f2_trans' ,'f3_trans' ,'f4_trans' ,&
                                              &'f1_grain' ,'f2_grain' ,'f3_grain' ,'f4_grain','unit_name'
    call cpu_time(time0)                                                                            !cpu计时

    do i=1,nctrl                                                                                    !逐个运行模拟单元
        call run_unit(ctrl_matrix(i)%tem,ctrl_matrix(i)%time,ctrl_matrix(i)%irr_flux,ctrl_matrix(i)%name,ctrl_matrix(i)%outp)
    enddo

    close(101)
end subroutine ctrl_sequence

subroutine run_unit(unit_tem,unit_time,unit_irr_flux,unit_name,unit_outp)
    !辐照及退火模拟单元模块
    use typ
    implicit none
    integer*4 i,j,k,unit_outp
    real*8 unit_tem,unit_time,unit_irr_flux,ran1
    character*300 unit_name,file_name

    write(10,*)'----------simulation unit <'//trim(adjustl(unit_name))//'> started----------'
    write(10,*)'unit temperature=',unit_tem
    write(10,*)'unit time=',unit_time
    write(10,*)'unit irradiation flux=',unit_irr_flux
	call cpu_time(time1)
	
    damage_rate=unit_irr_flux*surface_area*10.0**-20.0                            !根据通量计算损伤速率(电子/中子dpa需要进行换算)
    tem=unit_tem                                                                !设置温度
    call renew_rate_all()
     
    defect_remain=0
    defect_released=0
    defect_transmitted=0
    grain_released=0
    
    timer=0                                                                 !计时器归零
    call RANDOM_NUMBER(ran1)
    timer=timer-1.0d0/root%rate*log(ran1)                                   !若第一次时间增量就超出unit_time，则不执行任何操作
    do while(timer<unit_time)                                               !运行模拟单元
        call choose()     
        call RANDOM_NUMBER(ran1)
        timer=timer-1.0d0/root%rate*log(ran1)                               !计算时间增量
    enddo
    call cpu_time(time2)
    
    !输出结果
    file_name=trim(adjustl(unit_name))

    call write_cfg(file_name,unit_outp)                                
 

    !统计数量    
    do i=1,nclu
        defect_remain=defect_remain+clu(i)%formula
    enddo

    
    write(101,'(F10.3,17I10,A20)')tem,nclu,defect_remain,defect_released,defect_transmitted,grain_released,trim(adjustl(unit_name))                                                
    write(10,'(A40,1I10)')'No. of clusters remained=',nclu
    write(10,'(A40,4I10)')'No. of defects remained=',defect_remain
    write(10,'(A40,4I10)')'No. of defects released=',defect_released
    write(10,'(A40,4I10)')'No. of defects transmitted=',defect_transmitted
    write(10,'(A40,4I10)')'No. of defects absored by grain=',grain_released
    write(10,*)'CPU time comsuption of simulation unit=',time2-time1
    write(10,*)'overall CPU simulation time comsuption=',time2-time0
    write(10,*)'---------simulation unit <'//trim(adjustl(unit_name))//'> finished----------'
    write(10,*)
    write(*,*)'----------simulation unit <'//trim(adjustl(unit_name))//'> finished----------'
end subroutine run_unit


