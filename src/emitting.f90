    subroutine emitting(i)
    !团簇解离子程序，根据团簇的类型，从团簇中解离处一个点缺陷，并向外跃前一步
    use typ
    implicit none
    integer*4 i,j,jj,k,n,n1,n2,emit,nemit,nclu1,v,vicinity
    integer*4 formula1(element),formula2(element),orien1,orien2
    real*8 radius1,radius2,ran1,parameters,mig_step
    real*8 x(3),x1(3),x2(3),vector1(3),vector2(3)


    x=clu(i)%coord                              !读取坐标和元胞信息

    n=clu(i)%n                                  !解离后团簇大小
    n1=n-1
    n2=1
    emit=clu(i)%emit
    
    formula2=0
    if(iso_eff==1 .AND. (emit==2 .OR. emit==3))then
		!考虑同位素效应,跟据2、3组分数量随机选择发射组分
        call RANDOM_NUMBER(ran1)
        if(ran1<clu(i)%formula(2)*1.0/(clu(i)%formula(2)+clu(i)%formula(3)))then
            formula2(2)=1
        else
            formula2(3)=1
        endif
    else
        !不考虑同位素效应，直接确定发射组分
        formula2(abs(emit))=emit/abs(emit)
    endif
    
    formula1=clu(i)%formula-formula2            !解离后的组分


    orien1=clu(i)%orien                                     !解离后大团簇的方位取原团簇方位
    call RANDOM_NUMBER(ran1)
    orien2=ran1*8+1

    radius1=parameters(4,formula1)                          !解离后的半径
    radius2=parameters(4,formula2)

    x1=x                                                    !解离后大团簇的坐标

    mig_step=max(parameters(8,formula2),0.1)                !防止mig_step=0，导致发射后立刻聚合，陷入死循环
    call RANDOM_NUMBER(ran1)
    orien2=ran1*8+1
    x2=x+(radius1+radius2+mig_step)*move(:,orien2)          !单团簇解离后的坐标
 

    

    call dele(i)                                            !删除被解离的团簇
    call add_and_vicinity(x1,orien1,formula1)               !将解离得到的大团簇加入到体系中(假设解离时两个缺陷都不被晶界吸收)

    call add_and_vicinity(x2,orien2,formula2)               !将解离得到的小团簇加入到体系中  

10  end