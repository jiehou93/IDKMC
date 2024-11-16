    function parameters(n,formula_in)
    !缺陷复合体的参数计算函数，n=1:8分别代表em,er+em,eb+em,r,ve,vm,emit,step。formula数组为复合体的组成成分
    use typ
    implicit none
    integer*4 n,formula_in(element),formula(element)
    integer*4 i,j,position,error
    real*8 parameters
    
    formula=formula_in
    
	if(iso_eff==1)then
		!考虑同位素效应
        formula(2)=formula(2)+formula(3)
        formula(3)=0
    endif
    
    if(formula(1)<=para_range(1,2).AND.formula(1)>=para_range(1,1).AND.formula(2)<=para_range(2,2).AND.formula(3)<=para_range(3,2).AND.formula(4)<=para_range(4,2))then
        !假如是已经定义好的部分参数，且是有效参数，则直接读取并返回，否则进入下面的不稳定团簇设置
        parameters=ion_para(n,formula(1),formula(2),formula(3),formula(4))
        if(ion_para(1,formula(1),formula(2),formula(3),formula(4))>-10)then
            !确定读取到了有效参数
            return
        endif

    endif     
    
    !未定义的团簇均为为不稳定团簇
    selectcase(n)
    case(1:2)
        parameters=100
    case(3)
        parameters=0
         write(10,'(A35,4I7,A25)')'Warning! undefined parameter for ',formula(1),formula(2),formula(3),formula(4),' setting to unstable!'
    case(4)
        parameters=0
    case(5)
        parameters=1e15
    case(6)
        parameters=0
    case(7)
        parameters=maxloc(abs(formula(:)),1)
        parameters=formula(parameters)/abs(formula(parameters))*parameters
    case(8)
        parameters=0
    endselect
    
    end function