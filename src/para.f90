    function parameters(n,formula_in)
    !ȱ�ݸ�����Ĳ������㺯����n=1:8�ֱ����em,er+em,eb+em,r,ve,vm,emit,step��formula����Ϊ���������ɳɷ�
    use typ
    implicit none
    integer*4 n,formula_in(element),formula(element)
    integer*4 i,j,position,error
    real*8 parameters
    
    formula=formula_in
    
	if(iso_eff==1)then
		!����ͬλ��ЧӦ
        formula(2)=formula(2)+formula(3)
        formula(3)=0
    endif
    
    if(formula(1)<=para_range(1,2).AND.formula(1)>=para_range(1,1).AND.formula(2)<=para_range(2,2).AND.formula(3)<=para_range(3,2).AND.formula(4)<=para_range(4,2))then
        !�������Ѿ�����õĲ��ֲ�����������Ч��������ֱ�Ӷ�ȡ�����أ������������Ĳ��ȶ��Ŵ�����
        parameters=ion_para(n,formula(1),formula(2),formula(3),formula(4))
        if(ion_para(1,formula(1),formula(2),formula(3),formula(4))>-10)then
            !ȷ����ȡ������Ч����
            return
        endif

    endif     
    
    !δ������Ŵؾ�ΪΪ���ȶ��Ŵ�
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