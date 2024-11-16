subroutine ctrl_sequence()
    !ģ���������TDSʵ��
    use typ
    implicit none

    integer*4 i,j,k


    open(101,file='defect_remain.txt')
    open(102,file='defect_released.txt')
    open(103,file='defect_transmitted.txt')
    open(104,file='grain_released.txt')
    open(105,file='cluster_remain.txt')

    call cpu_time(time0)                                                                            !cpu��ʱ

    do i=1,nctrl                                                                                    !�������ģ�ⵥԪ
        call run_unit(ctrl_matrix(i)%tem,ctrl_matrix(i)%time,ctrl_matrix(i)%irr_flux,ctrl_matrix(i)%name,ctrl_matrix(i)%outp)
    enddo

    close(101)
    close(102)
    close(103)
    close(104)
    close(105)
end subroutine ctrl_sequence

subroutine run_unit(unit_tem,unit_time,unit_irr_flux,unit_name,unit_outp)
    !���ռ��˻�ģ�ⵥԪģ��
    use typ
    implicit none
    integer*4 i,j,k,unit_outp
    real*8 unit_tem,unit_time,unit_irr_flux,ran1
    character*300 unit_name,file_name

    write(10,*)'----------simulation unit <'//trim(adjustl(unit_name))//'> started----------'
    write(10,*)'unit temperature=',unit_tem
    write(10,*)'unit time=',unit_time
    write(10,*)'unit irradiation flux=',unit_irr_flux

    damage_rate=unit_irr_flux*surface_area*10**-20.0                            !����ͨ��������������(����/����dpa��Ҫ���л���)

    tem=unit_tem                                                                !�����¶�
    call renew_rate_all()

    call cpu_time(time1) 
    defect_remain=0
    defect_released=0
    defect_transmitted=0
    grain_released=0
    
    timer=0                                                                 !��ʱ������
    call RANDOM_NUMBER(ran1)
    timer=timer-1.0d0/root%rate*log(ran1)                                   !����һ��ʱ�������ͳ���unit_time����ִ���κβ���
    do while(timer<unit_time)                                               !����ģ�ⵥԪ
        call choose()     
        call RANDOM_NUMBER(ran1)
        timer=timer-1.0d0/root%rate*log(ran1)                               !����ʱ������
    enddo
    call cpu_time(time2)
    
    !������
    file_name=trim(adjustl(unit_name))
    if(unit_outp==1) then
        call write_cfg(file_name)                                
    endif

    !ͳ������    
    do i=1,nclu
        defect_remain=defect_remain+clu(i)%formula
    enddo

    write(101,'(4I10,F12.3,A20)')defect_remain,tem,trim(adjustl(unit_name))                                                  !������ȱ��
    write(102,'(4I10,F12.3,A20)')defect_released,tem,trim(adjustl(unit_name))                                                !�Ѹ���ȱ��
    write(103,'(4I10,F12.3,A20)')defect_transmitted,tem,trim(adjustl(unit_name))                                             !͸���ȱ��
    write(104,'(4I10,F12.3,A20)')grain_released,tem,trim(adjustl(unit_name))                                                 !�������յ�ȱ��
    write(105,'(I10,F12.3,A20)')nclu,tem,trim(adjustl(unit_name))                                                            !�������յ�ȱ��
    

    write(10,*)'CPU time comsuption of simulation unit=',time2-time1
    write(10,*)'overall CPU simulation time comsuption=',time2-time0
    write(10,*)'---------simulation unit <'//trim(adjustl(unit_name))//'> finished----------'
    write(10,*)
    write(*,*)'----------simulation unit <'//trim(adjustl(unit_name))//'> finished----------'
end subroutine run_unit


