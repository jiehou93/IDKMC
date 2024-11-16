subroutine file_check()
    integer*4 stat,stat1,stat2,stat3
    character*300 msg,msg1,msg2,msg3
    
    
    open(2000,file='INPUT.txt',STATUS='OLD',iostat=stat,iomsg=msg) 
    if (stat /= 0) then
        write(10,'(A300)')msg
        stop
    endif
    close(2000)
    
    open(2000,file='CONTROL.txt',STATUS='OLD',iostat=stat,iomsg=msg) 
    if (stat /= 0) then
        write(10,'(A300)')msg
        stop
    endif
    close(2000)
    
    
    open(2000,file='aiv.xyz.cfg',STATUS='OLD',iostat=stat,iomsg=msg) 
    open(2001,file='SIA.txt',STATUS='OLD',iostat=stat1,iomsg=msg1) 
    open(2002,file='ION.txt',STATUS='OLD',iostat=stat2,iomsg=msg2) 
    open(2003,file='VAC.txt',STATUS='OLD',iostat=stat3,iomsg=msg3) 
    
    if(stat/=0)then
        if(stat1/=0 .OR. stat2/=0 .OR. stat3/=0)then
            write(10,'(A300)')msg
            write(10,'(A300)')msg1
            write(10,'(A300)')msg2
            write(10,'(A300)')msg3
            stop
        endif
    endif
    close(2000)
    close(2001)
    close(2002)
    close(2003)
    
    open(2000,file='POSITION.txt',STATUS='OLD',iostat=stat,iomsg=msg) 
    open(2001,file='POSITION.lmp',STATUS='OLD',iostat=stat1,iomsg=msg1) 
    if (stat*stat1 /= 0 ) then
        write(10,'(A300)')msg
        write(10,'(A300)')msg1
        stop
    endif
    close(2000)
    close(2001)
    
end subroutine file_check
    