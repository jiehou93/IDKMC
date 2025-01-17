    subroutine assign_para(formula_in,parameters)
    !缺陷复合体的参数计算函数，n=1:8分别代表em,er+em,eb+em,r,ve,vm,emit,step。formula数组为复合体的组成成分
    use typ
    implicit none
    integer*4 formula_in(element),formula(element)
    integer*4 i,j,position,error,para_range_new(4,2)
    real*8 parameters(8),para_dummy
    real*8,allocatable::para_table_new(:,:,:,:,:)
    logical in_range
    
    formula=formula_in
    
	if(iso_eff==1)then
		!考虑同位素效应
        formula(2)=formula(2)+formula(3)
        formula(3)=0
    endif
    
    in_range=.true.
    do  i=1,4
        if(formula(i)<para_range(i,1) .or. formula(i)>para_range(i,2)) in_range=.false.
    enddo 
    
    if(in_range)then
        !该团簇组分在参数定义范围内
        if(para_table(1,formula(1),formula(2),formula(3),formula(4))>-10)then
            !判断参数是否已经在PARA中定义，若已经京东，直接读取并返回，否则结束判断，进入后面的未定义参数设置
            parameters=para_table(:,formula(1),formula(2),formula(3),formula(4))
            return
        endif
    else
        !该团簇组分不在参数定义范围内，对定义范围进行扩容
        write(10,'(A18,4I7,A50)')'Warning! cluster ',formula(1),formula(2),formula(3),formula(4),' is out of para_range, extending para_range to:'
        
        do i=1,4
            !确定扩容范围
            para_range_new(i,1)=min(formula(i),para_range(i,1))
            para_range_new(i,2)=max(formula(i),para_range(i,2))
            write(10,*)para_range_new(i,:)
        enddo
        
        !新参数列表分配内存
        allocate(para_table_new(8,para_range_new(1,1):para_range_new(1,2),para_range_new(2,1):para_range_new(2,2),para_range_new(3,1):para_range_new(3,2),para_range_new(4,1):para_range_new(4,2)))
        !预先设置未定义参数
        para_table_new=-100
        !保存已定义参数
        para_table_new(:,para_range(1,1):para_range(1,2),para_range(2,1):para_range(2,2),para_range(3,1):para_range(3,2),para_range(4,1):para_range(4,2))=para_table
        !扩容旧参数列表
        deallocate(para_table)
        allocate(para_table(8,para_range_new(1,1):para_range_new(1,2),para_range_new(2,1):para_range_new(2,2),para_range_new(3,1):para_range_new(3,2),para_range_new(4,1):para_range_new(4,2)))
        !将保存的参数复制回去
        para_table=para_table_new
        para_range=para_range_new
        deallocate(para_table_new)
    endif
    
    !未定义参数，设置为不稳定团簇
    write(10,'(A35,4I7,A25)')'Warning! undefined parameter for ',formula(1),formula(2),formula(3),formula(4),' setting as unstable!'
    if(SUM(abs(formula))==1)then
        write(10,*)'Error! Cannot set point defect as unstable!'
        write(10,*)'Please manually set para for point defects!'
        stop
    endif
        
    parameters(1:2)=100         !不可迁移
    parameters(3:4)=0           !钉扎能为0，半径为0
    parameters(5)=1E15          !很大的v_emit
    parameters(6)=0             !v_mig=0
    para_dummy=maxloc(abs(formula),1)
    parameters(7)=formula(para_dummy)/abs(formula(para_dummy))*para_dummy   !发射最多的组元
    parameters(8)=1             !setp=1
    para_table(:,formula(1),formula(2),formula(3),formula(4))=parameters    !保存该缺陷的不稳定参数，避免后续重复设置
    
    end subroutine assign_para