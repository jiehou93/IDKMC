    subroutine load_cfg()
    !读取构型
    use typ
    implicit none
    integer*4 i,j,orien,formula(element),num,GetFileN,file_lines,string_length,note_location,pbc_check(3),stat
    real*8 distance,ran1,ran2,ran3,ran4,alpha,beta,coord(3),box(3,2)
    character*300 dummy_string,msg
    
    !从指定文件载入初始构型
    write(10,*)'    Loading POSITION file...'
    if(cfg_type=='txt')then
        open(2000,file='POSITION.txt',STATUS='OLD',iostat=stat,iomsg=msg) 
        if (stat /= 0 ) then
            write(10,*)'    POSITION file not found, starting from stracth...'
        else
            file_lines=GetFileN(2000)
            write(10,*)'    loading ',file_lines, 'clusters from txt file'
            
            orien=1
            do i=1,file_lines
                read(2000,'(A300)')dummy_string                                                    !

                call de_note(dummy_string)                                                  !提取有效内容
                string_length=len_trim(dummy_string)
                if(string_length==0)then
                    cycle                                                                   !该行为无效行
                endif

                read(dummy_string(1:string_length),*)coord,formula,orien
                coord=coord+1e-8                                                            !防止正好落在边界上导致报错
                pbc_check=coord/length+1
                if(pbc_check(1)*pbc_check(2)*pbc_check(3)/=1)then
                    write(10,*)'    Warrning, cluster',i,'is not in the simulation box, wrapping with PBC...'
                endif
                if(lattice_type=='bcc' .AND. orien>8)then
                     write(10,*)'    Error! In bcc lattice, orien>8 for cluster',i
                     stop
                endif
                call add(coord,orien,formula)
            enddo
            write(10,*)file_lines, 'clusters successfully loaded'
        endif
        close(2000)
    elseif(cfg_type=='lmp')then
        open(2000,file='POSITION.lmp',STATUS='OLD',iostat=stat,iomsg=msg) 
        if (stat /= 0 ) then
            write(10,*)'    POSITION file not found, starting from stracth...'
        else
            file_lines=GetFileN(2000)
            if(file_lines==0) return
            !读取头文件
            read(2000,*)
            read(2000,*)
            read(2000,*)
            read(2000,*)num
            write(10,*)'    loading ',num, 'clusters from lmp file'
            read(2000,*)
            read(2000,*)box(1,:)
            read(2000,*)box(2,:)
            read(2000,*)box(3,:)
            if(sum(box(:,2)-length)/=0) write(10,*)'    POSITION box size differs with INPUT box size, using INPUT box size'
            read(2000,*)
            
            !读取数据
            orien=1
            do i=1,num
                read(2000,'(A300)')dummy_string                                                    !

                call de_note(dummy_string)                                                  !提取有效内容
                string_length=len_trim(dummy_string)
                if(string_length==0)then
                    cycle                                                                   !该行为无效行
                endif

                read(dummy_string(1:string_length),*)coord,formula,orien
                coord=coord+1e-8                                                            !防止正好落在边界上导致报错
                pbc_check=coord/length+1
                if(pbc_check(1)*pbc_check(2)*pbc_check(3)/=1)then
                    write(10,*)'    Warrning, cluster',i,'is not in the simulation box, wrapping with PBC...'
                endif
                if(lattice_type=='bcc' .AND. orien>8)then
                     write(10,*)'    Error! In bcc lattice, orien>8 for cluster',i
                     stop
                endif
                call add(coord,orien,formula)
            enddo
            write(10,*)file_lines, 'clusters successfully loaded'   
        endif
        close(2000)
    else
        write(10,*)'    Error, cfg_type not supported, please choose txt or lmp.'
        stop
    endif
    
    end subroutine load_cfg
    
    subroutine write_cfg(file_name,unit_outp)
    !读取构型
    use typ
    implicit none
    integer*4 i,j,orien,formula(element),num,GetFileN,file_lines,string_length,note_location,unit_outp,selective_output,n_outp
    real*8 distance,ran1,ran2,ran3,ran4,alpha,beta,coord(3),box(3,2)
    character*300 dummy_string,file_name
    
    if(unit_outp==0) return !不进行输出，直接返回
    
    selective_output=unit_outp-1
    if(cfg_type=='txt')then
        file_name=trim(adjustl(file_name))//'.txt'                                  
        open(110,file=trim(adjustl(file_name)),status='replace')
    elseif(cfg_type=='lmp')then
        file_name=trim(adjustl(file_name))//'.lmp'                                  
        open(110,file=trim(adjustl(file_name)),status='replace')
        !文件头
        write(110,'(A,L)')'ITEM: TIMESTEP'
        write(110,'(A,L)')'0'
        write(110,'(A,L)')'ITEM: NUMBER OF ATOMS'
        if(selective_output==0)then
            n_outp=nclu
        else
            n_outp=0
            do i=1,nclu
                if(clu(i)%formula(selective_output)/=0)  n_outp=n_outp+1
            enddo
        endif
        write(110,*)n_outp
            
        write(110,'(A,L)')'ITEM: BOX BOUNDS pp pp pp'
        write(110,*)0.0,length(1)
        write(110,*)0.0,length(2)
        write(110,*)0.0,length(3)
        if(output_rate==0)then
            write(110,'(A,L)')'ITEM: ATOMS x y z f1 f2 f3 f4 orient radius'
        elseif(output_rate==1)then
            write(110,'(A,L)')'ITEM: ATOMS x y z f1 f2 f3 f4 orient radius rate_mig rate_rot rate_emit'
        else
             write(10,*)'   Error, output_rate must be 0 or 1'
        endif
        
    else    
        write(10,*)'    Error, cfg_type not supported, please choose txt or lmp.'
        stop
    endif
    
    !详细构型输出
    if(output_rate==0)then
        !不输出速率信息
        if(selective_output==0)then
            !输出所有缺陷
            do i=1,nclu
                write(110,'(1x,3F16.6,5I6,F8.3)')clu(i)%coord,clu(i)%formula,clu(i)%orien,clu(i)%r 
            enddo
        else
            !输出指定类型缺陷
            do i=1,nclu
                if((clu(i)%formula(selective_output)/=0))then
                     write(110,'(1x,3F16.6,5I6,F8.3)')clu(i)%coord,clu(i)%formula,clu(i)%orien,clu(i)%r 
                endif
            enddo
        endif
    elseif(output_rate==1)then
        !输出速率信息
        if(selective_output==0)then
            !输出所有缺陷
            do i=1,nclu
                write(110,'(1x,3F16.6,5I6,F8.3,3E10.3)')clu(i)%coord,clu(i)%formula,clu(i)%orien,clu(i)%r,clu(i)%rate(1),clu(i)%rate(2)-clu(i)%rate(1),clu(i)%rate(3)-clu(i)%rate(2) 
            enddo
        else
            !输出指定类型缺陷
            do i=1,nclu
                if((clu(i)%formula(selective_output)/=0))then
                     write(110,'(1x,3F16.6,5I6,F8.3,3E10.3)')clu(i)%coord,clu(i)%formula,clu(i)%orien,clu(i)%r,clu(i)%rate(1),clu(i)%rate(2)-clu(i)%rate(1),clu(i)%rate(3)-clu(i)%rate(2) 
                endif
            enddo
        endif
    else
        write(10,*)'    Error, output_rate must be 0 or 1'
    endif
    
    
    close(110)
        
    end subroutine write_cfg