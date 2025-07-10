    subroutine load_control()
    !��ȡ���̿��ƾ���
    use typ
    implicit none
    integer*4 GetFileN,file_lines,i,j,k,string_length,eff_length,unit_outp,stat,n_flux
    real*8 unit_tem,unit_time,unit_irr_flux(3)
    character*300 dummy_string,unit_name,msg


    open(1001,file='CONTROL.txt',STATUS='OLD')
    file_lines=GetFileN(1001)

    allocate(ctrl_matrix(file_lines))                                               !�������̿��ƾ�����ڴ�ռ�
    nctrl=0                                                                         !����ģ�����������

    do i=1,file_lines                                                               !��ȡ�߽����
        read(1001,'(A300)')dummy_string
        call de_note(dummy_string)                                                  !��ȡ��Ч����

        string_length=len_trim(dummy_string)
        if(string_length==0)then
            cycle                                                                   !����Ϊ��Ч��
        endif
        
        unit_irr_flux=0
        read(dummy_string,*, iostat=stat)unit_tem,unit_time,unit_irr_flux(1:n_beam),unit_name,unit_outp
        n_flux=COUNT(unit_irr_flux>1e-10)
        if (stat /= 0) then
            write(10,*)'Error in reading CONTROL, plase check number of coloums carefully... '
            if (n_flux>n_beam) then
                write(10,*),'No. of fluxes in CONTROL must <= No. of ion_type in INPUT'
                write(10,*),'No. of fluxes=',n_flux
                write(10,*),'No. of ion_type=',ion_type
                write(10,*),'Exiting...'
            endif
            stop
        endif
        
        nctrl=nctrl+1                                                               !����һ�����Ƶ�Ԫ�������ȡ�Ĳ���ֵ
        ctrl_matrix(nctrl)%tem=unit_tem
        ctrl_matrix(nctrl)%time=unit_time
        ctrl_matrix(nctrl)%irr_flux=unit_irr_flux*irr_status                        !��δ�ṩ��Ӧ�ļ���������Զ�������ͨ����Ϊ0
        ctrl_matrix(nctrl)%name=unit_name
        ctrl_matrix(nctrl)%outp=unit_outp
    enddo
    close(1001)
    end subroutine load_control

