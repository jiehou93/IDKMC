    subroutine load_control()
    !读取流程控制矩阵。
    use typ
    implicit none
    integer*4 GetFileN,file_lines,i,j,k,string_length,eff_length,unit_outp,stat
    real*8 unit_tem,unit_time,unit_irr_flux
    character*300 dummy_string,unit_name,msg


    open(1001,file='CONTROL.txt',STATUS='OLD')
    file_lines=GetFileN(1001)

    allocate(ctrl_matrix(file_lines))                                               !分配流程控制矩阵的内存空间
    nctrl=0                                                                         !流程模块计数器归零

    do i=1,file_lines                                                               !读取边界参数
        read(1001,'(A300)')dummy_string
        call de_note(dummy_string)                                                  !提取有效内容

        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   !该行为无效行
        endif

        read(dummy_string,*)unit_tem,unit_time,unit_irr_flux,unit_name,unit_outp

        nctrl=nctrl+1                                                               !新增一个控制单元并赋予读取的参数值
        ctrl_matrix(nctrl)%tem=unit_tem
        ctrl_matrix(nctrl)%time=unit_time
        ctrl_matrix(nctrl)%irr_flux=unit_irr_flux
        ctrl_matrix(nctrl)%name=unit_name
        ctrl_matrix(nctrl)%outp=unit_outp
    enddo
    close(1001)
    end subroutine load_control

