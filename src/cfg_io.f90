    subroutine load_cfg()
    !读取构型
    use typ
    implicit none
    integer*4 i,j,orien,formula(element),num,GetFileN,file_lines,string_length,note_location
    real*8 distance,ran1,ran2,ran3,ran4,alpha,beta,coord(3),box(3,2)
    character*300 dummy_string
    
    !从指定文件载入初始构型
    if(cfg_type=='txt')then
        open(2000,file='POSITION.txt',STATUS='OLD')             
            file_lines=GetFileN(2000)
            write(10,*)'loading ',file_lines, 'clusters from txt file'
            
            orien=1
            do i=1,file_lines
                read(2000,'(A300)')dummy_string                                                    !

                call de_note(dummy_string)                                                  !提取有效内容
                string_length=len_trim(dummy_string)
                if(string_length==0)then
                    cycle                                                                   !该行为无效行
                endif

                read(dummy_string(1:string_length),*)coord,formula,orien
                call add(coord,orien,formula)
            enddo
            write(10,*)file_lines, 'clusters successfully loaded'
        close(2000)
    elseif(cfg_type=='lmp')then
        open(2000,file='POSITION.lmp',STATUS='OLD')   
            file_lines=GetFileN(2000)
            if(file_lines==0) return
            !读取头文件
            read(2000,*)
            read(2000,*)
            read(2000,*)
            read(2000,*)num
            write(10,*)'loading ',num, 'clusters from lmp file'
            read(2000,*)
            read(2000,*)box(1,:)
            read(2000,*)box(2,:)
            read(2000,*)box(3,:)
            if(sum(box(:,2)-length)/=0) write(10,*)'POSITION box size differs with INPUT box size, using INPUT box size'
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
                call add(coord,orien,formula)
            enddo
            write(10,*)file_lines, 'clusters successfully loaded'          
    else
        write(10,*)'Error, cfg_type not supported, please choose txt or lmp.'
        stop
    endif
    
    end subroutine load_cfg
    
    subroutine write_cfg(file_name)
    !读取构型
    use typ
    implicit none
    integer*4 i,j,orien,formula(element),num,GetFileN,file_lines,string_length,note_location
    real*8 distance,ran1,ran2,ran3,ran4,alpha,beta,coord(3),box(3,2)
    character*300 dummy_string,file_name
    
    if(cfg_type=='txt')then
        file_name=trim(adjustl(file_name))//'.txt'                                  
        open(110,file=trim(adjustl(file_name)),status='replace')
            do i=1,nclu
                write(110,'(1x,3F16.6,5I6)')clu(i)%coord,clu(i)%formula,clu(i)%orien !详细构型
            enddo
        close(110)
    elseif(cfg_type=='lmp')then
        file_name=trim(adjustl(file_name))//'.lmp'                                  
        open(110,file=trim(adjustl(file_name)),status='replace')
        
            !文件头
            write(110,'(A,L)')'ITEM: TIMESTEP'
            write(110,'(A,L)')'0'
            write(110,'(A,L)')'ITEM: NUMBER OF ATOMS'
            write(110,*)nclu
            write(110,'(A,L)')'ITEM: BOX BOUNDS pp pp pp'
            write(110,*)0.0,length(1)
            write(110,*)0.0,length(2)
            write(110,*)0.0,length(3)
            write(110,'(A,L)')'ITEM: ATOMS x y z nV nH f3 f4 orient'
            
            !数据
            do i=1,nclu
                write(110,'(1x,3F16.6,5I6)')clu(i)%coord,clu(i)%formula,clu(i)%orien !详细构型
            enddo
    else    
        write(10,*)'Error, cfg_type not supported, please choose txt or lmp.'
        stop
    endif
    
    end subroutine write_cfg