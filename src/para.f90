    subroutine assign_para(formula_in,parameters)
    !ȱ�ݸ�����Ĳ������㺯����n=1:8�ֱ����em,er+em,eb+em,r,ve,vm,emit,step��formula����Ϊ���������ɳɷ�
    use typ
    implicit none
    integer*4 formula_in(element),formula(element)
    integer*4 i,j,position,error,para_range_new(4,2)
    real*8 parameters(8),para_dummy
    real*8,allocatable::para_table_new(:,:,:,:,:)
    logical in_range
    
    formula=formula_in
    
	if(iso_eff==1)then
		!����ͬλ��ЧӦ
        formula(2)=formula(2)+formula(3)
        formula(3)=0
    endif
    
    in_range=.true.
    do  i=1,4
        if(formula(i)<para_range(i,1) .or. formula(i)>para_range(i,2)) in_range=.false.
    enddo 
    
    if(in_range)then
        !���Ŵ�����ڲ������巶Χ��
        if(para_table(1,formula(1),formula(2),formula(3),formula(4))>-10)then
            !�жϲ����Ƿ��Ѿ���PARA�ж��壬���Ѿ�������ֱ�Ӷ�ȡ�����أ���������жϣ���������δ�����������
            parameters=para_table(:,formula(1),formula(2),formula(3),formula(4))
            return
        endif
    else
        !���Ŵ���ֲ��ڲ������巶Χ�ڣ��Զ��巶Χ��������
        write(10,'(A18,4I7,A50)')'Warning! cluster ',formula(1),formula(2),formula(3),formula(4),' is out of para_range, extending para_range to:'
        
        do i=1,4
            !ȷ�����ݷ�Χ
            para_range_new(i,1)=min(formula(i),para_range(i,1))
            para_range_new(i,2)=max(formula(i),para_range(i,2))
            write(10,*)para_range_new(i,:)
        enddo
        
        !�²����б�����ڴ�
        allocate(para_table_new(8,para_range_new(1,1):para_range_new(1,2),para_range_new(2,1):para_range_new(2,2),para_range_new(3,1):para_range_new(3,2),para_range_new(4,1):para_range_new(4,2)))
        !Ԥ������δ�������
        para_table_new=-100
        !�����Ѷ������
        para_table_new(:,para_range(1,1):para_range(1,2),para_range(2,1):para_range(2,2),para_range(3,1):para_range(3,2),para_range(4,1):para_range(4,2))=para_table
        !���ݾɲ����б�
        deallocate(para_table)
        allocate(para_table(8,para_range_new(1,1):para_range_new(1,2),para_range_new(2,1):para_range_new(2,2),para_range_new(3,1):para_range_new(3,2),para_range_new(4,1):para_range_new(4,2)))
        !������Ĳ������ƻ�ȥ
        para_table=para_table_new
        para_range=para_range_new
        deallocate(para_table_new)
    endif
    
    !δ�������������Ϊ���ȶ��Ŵ�
    write(10,'(A35,4I7,A25)')'Warning! undefined parameter for ',formula(1),formula(2),formula(3),formula(4),' setting as unstable!'
    if(SUM(abs(formula))==1)then
        write(10,*)'Error! Cannot set point defect as unstable!'
        write(10,*)'Please manually set para for point defects!'
        stop
    endif
        
    parameters(1:2)=100         !����Ǩ��
    parameters(3:4)=0           !������Ϊ0���뾶Ϊ0
    parameters(5)=1E15          !�ܴ��v_emit
    parameters(6)=0             !v_mig=0
    para_dummy=maxloc(abs(formula),1)
    parameters(7)=formula(para_dummy)/abs(formula(para_dummy))*para_dummy   !����������Ԫ
    parameters(8)=1             !setp=1
    para_table(:,formula(1),formula(2),formula(3),formula(4))=parameters    !�����ȱ�ݵĲ��ȶ���������������ظ�����
    
    end subroutine assign_para