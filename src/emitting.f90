    subroutine emitting(i)
    !�Ŵؽ����ӳ��򣬸����Ŵص����ͣ����Ŵ��н��봦һ����ȱ�ݣ�������Ծǰһ��
    use typ
    implicit none
    integer*4 i,j,jj,k,n,n1,n2,emit,nemit,nclu1,v,vicinity
    integer*4 formula1(element),formula2(element),orien1,orien2
    real*8 radius1,radius2,ran1,parameters,mig_step
    real*8 x(3),x1(3),x2(3),vector1(3),vector2(3)


    x=clu(i)%coord                              !��ȡ�����Ԫ����Ϣ

    n=clu(i)%n                                  !������Ŵش�С
    n1=n-1
    n2=1
    emit=clu(i)%emit
    
    formula2=0
    if(iso_eff==1 .AND. (emit==2 .OR. emit==3))then
		!����ͬλ��ЧӦ,����2��3����������ѡ�������
        call RANDOM_NUMBER(ran1)
        if(ran1<clu(i)%formula(2)*1.0/(clu(i)%formula(2)+clu(i)%formula(3)))then
            formula2(2)=1
        else
            formula2(3)=1
        endif
    else
        !������ͬλ��ЧӦ��ֱ��ȷ���������
        formula2(abs(emit))=emit/abs(emit)
    endif
    
    formula1=clu(i)%formula-formula2            !���������


    orien1=clu(i)%orien                                     !�������Ŵصķ�λȡԭ�Ŵط�λ
    call RANDOM_NUMBER(ran1)
    orien2=ran1*8+1

    radius1=parameters(4,formula1)                          !�����İ뾶
    radius2=parameters(4,formula2)

    x1=x                                                    !�������Ŵص�����

    mig_step=max(parameters(8,formula2),0.1)                !��ֹmig_step=0�����·�������̾ۺϣ�������ѭ��
    call RANDOM_NUMBER(ran1)
    orien2=ran1*8+1
    x2=x+(radius1+radius2+mig_step)*move(:,orien2)          !���Ŵؽ���������
 

    

    call dele(i)                                            !ɾ����������Ŵ�
    call add_and_vicinity(x1,orien1,formula1)               !������õ��Ĵ��Ŵؼ��뵽��ϵ��(�������ʱ����ȱ�ݶ�������������)

    call add_and_vicinity(x2,orien2,formula2)               !������õ���С�Ŵؼ��뵽��ϵ��  

10  end