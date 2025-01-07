    subroutine sys_check()
    !系统自检器，检查是否有明显的数据错误
    use typ
    implicit none
    integer*4 i,j,k,l,GetFileN,file_lines,string_length,num,stat
    REAL*8 dmax,lmin
    logical log1,log2,log3,warning
    character*300 dummy_string,msg

    warning=.false.
    !检查元胞列表是否匹配
    log1=abs(cell_size(1)*cell_number(1)-large_cell_size(1)*large_cell_number(1))>0.01
    log2=abs(cell_size(2)*cell_number(2)-large_cell_size(2)*large_cell_number(2))>0.01
    log3=abs(cell_size(3)*cell_number(3)-large_cell_size(3)*large_cell_number(3))>0.01
    if(log1.or.log2.or.log3)then
        write(10,*)'    Error! The size of small cell list dose not match with the large cell list.'
        warning=.true.
    endif
    !初始浓度警告
    log1=sum(concen)>500000
    if(log1)then
        write(10,*)'    Warning! Initial defect concentration is higher than 500000appm.'
        warning=.true.
    endif
    !多次定义初始构型警告
    log1=.false.
    log2=.false.
    
    if(cfg_type=='txt')then
        open(2000,file='POSITION.txt',STATUS='OLD',iostat=stat,iomsg=msg) 
        if (stat == 0 ) then
            file_lines=GetFileN(2000)
            do i=1,file_lines
                read(2000,'(A300)')dummy_string               

                call de_note(dummy_string)                    
                string_length=len_trim(dummy_string)
                if(file_lines==0)then
                    cycle                                                                   
                else
                    log1=.true.
                    exit
                endif
            enddo
        endif
        close(2000)
    elseif(cfg_type=='lmp')then
        open(2000,file='POSITION.lmp',STATUS='OLD',iostat=stat,iomsg=msg) 
        if (stat == 0 ) then
            file_lines=GetFileN(2000)
            if(file_lines<9)then
                write(10,*)'    Error, error in reading lmp file'
            endif
            
            read(2000,*)dummy_string
            read(2000,*)dummy_string
            read(2000,*)dummy_string
            read(2000,*)num
            if(num>0)then
                log1=.true.
            endif
        endif
        close(2000)
    else
        write(10,*)'    Error, cfg_type not supported, please choose txt or lmp.'
        stop
    endif
    

    log2=sum(concen)/=0
    if(log1 .and. log2)then
        write(10,*)'    Warning! Costomizing initial configuration while randomly adding defects'
        warning=.true.
    endif

    !检查文件载入初始构型的坐标是否都在box内        
    do i=1,nclu
        log1=clu(i)%coord(1)<0.or.clu(i)%coord(1)>length(1)
        log2=clu(i)%coord(2)<0.or.clu(i)%coord(2)>length(2)
        log3=clu(i)%coord(3)<0.or.clu(i)%coord(3)>length(3)
        if(log1.or.log2.or.log3)then
            write(10,*)'    Error! The cluster',i,'in POSITION lie outside of the box'
            warning=.true.
        endif
    enddo

    !检查large cell size边长是否大于最大的缺陷直径
    lmin=minval(large_cell_size)
    dmax=2*maxval(ion_para(4,:,:,:,:))
    if(lmin<dmax)then
        write(10,*)'    Warning! Large_cell_size < max defect diameter. Correct neighbor search is not guaranteed. Two large defects can overlap without clustering together.'
        warning=.true.
    endif
    
    !检查PBC uniform_damage implant_direction是否一致
    log1=SUM(abs(pbc-uniform_damage))/=0
    if(log1)then
        write(10,*)'    Warning! PBC  /= uniform_damage. Make sure you know what you are doing.'
        warning=.true.
    endif
    if(pbc(implant_direction)/=0)then
        write(10,*)'    Warning! PBC=1 along the implant_direction. Usually unphysical for ion irradiation.'
        warning=.true.
    endif
    if(uniform_damage(implant_direction)/=0)then
        write(10,*)'    Warning! uniform_damage=1 along the implant_direction. Usually unphysical for ion irradiation.'
        warning=.true.
    endif
        
    if(warning)then
        write(*,*)'    Warning! I find something wierd, plese check the monitor.txt file. '
    else
        write(*,*)'    Nothing to worry about. At least for now...'
        write(10,*)'    Nothing to worry about. At least for now...'
    endif

    
        
    
    end subroutine sys_check