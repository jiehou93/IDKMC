    !全局调试程序，手动添加缺陷，输出构型信息。
    subroutine debug()
    use typ
    use intf
    implicit none

    integer*4 i,j,k,formula(4),orien,v,vicinity,tot_number
    real*8 ran1,ran2,ran3,coord(3),density,timer1,timer2

    call load_para()                                     !定义部分动力学参数
    !call para_test()
    call load_input()   
    call pre_calcul() 
    call initiate()
    

    tem=300
    orien=1

   coord=(/30,30,30/)
   formula=(/32,0,0,0/)

   call add_and_vicinity(coord,orien,formula)
   clu=clu
    coord=coord+1
    formula=(/1,0,0,0/)
   call add_and_vicinity(coord,orien,formula)




    !call renew_rate_all()
    !call CPU_TIME(timer1)
    !call renew_rate_all()
    !do i=1,10000
    !    call choose()
    !enddo
    !call CPU_TIME(timer2)
    !
    !open(1007,file=trim(adjustl(path))//'output_detail.txt')
    !
    !do i=1,nclu
    !    write(1007,'(1x,3F9.2,5I6)')clu(i)%coord,clu(i)%formula,clu(i)%orien
    !enddo
    !close(1007)

    end subroutine debug