    subroutine output_settings()
    !在monitor文件中输出读入的参数和控制矩阵
    use typ
    implicit none
    integer*4 i

    write(10,*)
    write(10,*)'########## INPUT PARAMETERS ##########'
    
    write(10,*)'########### BOX ##########'
    write(10,'(A20,3I15)')'PBC=',pbc
    write(10,'(A20,E15.7)')'grain_radius=',grain_radi
    write(10,'(A20,3F15.7)')'box_length=',length(1:3)
    write(10,'(A20,3F15.7)')'cell_size=',cell_size(1:3)
    write(10,'(A20,3I15)')'cell_number=',cell_number(1),cell_number(2),cell_number(3) 
    write(10,'(A20,3F15.7)')'large_cell_size=',large_cell_size(1:3)
    write(10,'(A20,3I15)')'large_cell_number=',large_cell_number(1),large_cell_number(2),large_cell_number(3)
    write(10,'(A20,F15.7)')'surface_depth=',surface_depth
    write(10,'(A20,F15.7)')'critical_radius=',critical_radius
    
    write(10,*)'########### ion and irradiation ##########'
    write(10,'(A20,3I15)')'uniform_damage=',uniform_damage
    write(10,'(A20,I15)')'ion_type=',ion_type
    write(10,'(A20,I15)')'iso_eff=',iso_eff
    write(10,'(A20,I15)')'implant_direction=',implant_direction
    
    write(10,*)'########## initial defects ##########'    
    write(10,'(A20,4F15.7)')'initial_defect=',concen(1:4)
    write(10,'(A20,I15)')'intrinsic_type=',intrinsic_type
    
    write(10,*)'########## others ##########'
    write(10,'(A20,A15)')'cfg_type=',cfg_type
    write(10,'(A20,4I15)')'rd_seed=',rd_seed
    
    write(10,*)'##################################'
    write(10,*)
    write(10,*)'########## CONTROL MATRIX ##########'
    write(10,'(A15,A15,A5,A20,A5)')'#temperature','time','rate','name','outp'
    do i=1,nctrl
        write(10,'(F15.7,E15.7,E15.7,A20,I5)')ctrl_matrix(i)%tem,ctrl_matrix(i)%time,ctrl_matrix(i)%irr_flux,trim(adjustl(ctrl_matrix(i)%name)),ctrl_matrix(i)%outp
    enddo
    write(10,*)'##################################'
    write(10,*)
    end subroutine output_settings
