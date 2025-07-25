    subroutine choose()
    !二叉树选择程序，随机选择一个反应并执行。
    use typ
    implicit none
    integer*4 i,j,formula(element)
    real*8 dart,ran1
    type(cluster) clu_dart

    call RANDOM_NUMBER(ran1)
    dart=ran1*root%rate                             !根据当前总速率，生成一个随机飞镖
    current=>root
    do i=1,height-1
        if(dart<current%left%rate)then              !飞镖击中左枝
            current=>current%left
        else
            dart=dart-current%left%rate             !飞镖击中右枝
            current=>current%right
        endif                                       
    enddo

    clu_dart=current%obj
    if(clu_dart%sn==0)then                          !辐照选项
        if(dart<clu_dart%rate(1))then               !选中对象后，再选择执行哪种反应
            call ion_implantation(1)
        elseif(dart<clu_dart%rate(2))then
            call ion_implantation(2)
        elseif(dart<clu_dart%rate(3))then           
            call ion_implantation(3)
        endif
    else
        if(dart<clu_dart%rate(1))then               !选中对象后，再选择执行哪种反应
            call migrate(clu_dart%sn,.false.)       !单向迁移(rotation=.false.)
        elseif(dart<clu_dart%rate(2))then
            call migrate(clu_dart%sn,.true.)        !转向迁移
        elseif(dart<clu_dart%rate(3))then           !注意浮点误差导致的零概率事件被选中。
            call emitting(clu_dart%sn)              !解体
        endif
	endif

    end subroutine