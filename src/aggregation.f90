    function vicinity(i)
    !元胞列表法近邻搜索子程序，搜索该团簇周围27个元胞内所有团簇，若满足聚合条件，返回第一个搜索到的团簇编号，若开始位错功能，则额外的进行与位错距离的判断。
    !返回-1代表晶界，返回-2代表位错，返回正值代表某个缺陷团簇，返回-100000代表无近邻对象
    use typ
    implicit none
    integer*4 vicinity
    integer*4 i,j,i1,i0,index,ii,jj,kk,ll,a1,b1,c1
    integer*4 a(3),b(3),large_a(3),large_b(3),large_c(3)
    real*8 x(3),x1(3),x2(3),dis(3),dist,radius
    type(cell_list),pointer::pointer_dummy
    
    vicinity=-100000                                                        !没有近邻对象时，近邻值默认为-100000

    !检查缺陷是否达到x/y/z表面 
    do j=1,3
        if(pbc(j)==0)then
            if(clu(i)%coord(j)-clu(i)%r<surface_depth)then
                vicinity=-1
                defect_released=defect_released+clu(i)%formula
                return
            elseif(clu(i)%coord(j)+clu(i)%r>length(j)-surface_depth)then
                vicinity=-1
                defect_transmitted=defect_transmitted+clu(i)%formula
                return
            endif
        endif  
    enddo


    x=clu(i)%coord      
    a=x/cell_size+1                 !确定元胞编号      

    do ii=1,3                                                           !逐一搜索该27个元胞，返回近邻团簇编号
        do jj=1,3
            do kk=1,3
                pointer_dummy=>cell(neib_cell(a(1),a(2),a(3),1,ii),neib_cell(a(1),a(2),a(3),2,jj),neib_cell(a(1),a(2),a(3),3,kk))
                do index=1,pointer_dummy%counter                        !搜索该元胞中所有团簇
                    pointer_dummy=>pointer_dummy%next
                    i1=pointer_dummy%clu%sn                             !从元胞链表中读出i1的团簇编号
                    if(i1==i)cycle                                      !防止团簇与自身反应

                    dis=abs(clu(i1)%coord-x)                            !计算周期性边界条件下，团簇与新加入团簇的距离
                    dist=sqrt(min(dis(1),length(1)-dis(1))**2+min(dis(2),length(2)-dis(2))**2+min(dis(3),length(3)-dis(3))**2)

                    if(dist<clu(i)%r+clu(i1)%r)then                     !若二者距离小于俘获半径，返回近邻团簇编号
                        vicinity=i1                                     !搜索到近邻缺陷后结束搜索
                        return
                    endif
                enddo
            enddo
        enddo
    enddo


    !以下对大元胞列表进行同样的搜索      
    !引入周期性边界条件，列出该元胞周围的27个大元胞（包括自身所处的元胞）
    
    a=x/large_cell_size+1                                             !确定大元胞编号      
    do ii=1,3                                                           !逐一搜索该27个元胞，若满足聚合条件则执行聚合操作
        do jj=1,3
            do kk=1,3
                pointer_dummy=>large_cell(large_neib_cell(a(1),a(2),a(3),1,ii),large_neib_cell(a(1),a(2),a(3),2,jj),large_neib_cell(a(1),a(2),a(3),3,kk))
                do index=1,pointer_dummy%counter
                    pointer_dummy=>pointer_dummy%next              !从元胞链表中读出i1的团簇编号
                    i1=pointer_dummy%clu%sn                                !防止团簇与自身反应
                    if(i1==i)cycle                         
                    x1=clu(i1)%coord                                    !读出被聚合团簇的坐标
                    dis=abs(x1-x)                                       !计算周期性边界条件下，团簇与新加入团簇的距离
                    dist=sqrt(min(dis(1),length(1)-dis(1))**2+min(dis(2),length(2)-dis(2))**2+min(dis(3),length(3)-dis(3))**2)

                    if(dist<clu(i)%r+clu(i1)%r)then                                 !若二者距离小于俘获半径，执行聚合操作
                        vicinity=i1                                    !搜索到近邻缺陷后结束搜索
                        return
                    endif
                enddo
            enddo
        enddo
    enddo
10  end

    subroutine aggregation(i,i1,aggregated)
    !团簇聚合子程序,聚合i和i1团簇，形成一个行的团簇。
    use typ
    use intf
    implicit none
    integer*4 i,i1,i0,index,ii,jj,kk,ll,a1,b1,c1,orien,n,n1,n2,eigen,eigen1,eigen2
    integer*4 a,b,c,formula(element),vicinity,large_a,large_b,large_c,aggregated
    real*8 x(3),x1(3),x2(3),dis(3),dist,radius,ran1,ran2,ran3,alpha,beta,rad,vector(3)


    !!!!!!!!!!!确定组分!!!!!!!!!!!!!
    eigen=clu(i)%formula(1)                             !计算聚合后的本征缺陷含量
    eigen1=clu(i1)%formula(1)
    eigen2=eigen+eigen1
    formula=clu(i)%formula+clu(i1)%formula              !聚合后组分
    orien=clu(i)%orien
    
    if(eigen2==0)then
        !I-V完全复合，检查是否还有其他产物
        if(sum(abs(formula))==0)then
            !无其他产物，删除两个缺陷
            aggregated=-100000                              
            if(i1==nclu)then                                !若i1在clu列表末端，先删除i0会使得i1位置移动
	            call dele(i1) 
                call dele(i) 
            else
                call dele(i) 
                call dele(i1)
            endif
            return
        else
            !有其他产物，坐标取其中一个（一般i1为大缺陷）
            x2=clu(i1)%coord
        endif
    else
        !I-V不完全复合
        x=clu(i)%coord
        x1=clu(i1)%coord
        dis=abs(x1-x)       
        do ii=1,3
            if(dis(ii)>length(ii)/2) then               !周期性边界条
                if(x1(ii)>x(ii))then
                    x1(ii)=x1(ii)-length(ii)
                else
                    x(ii)=x(ii)-length(ii)
                endif
            endif                    
        enddo
        
        if(eigen*eigen1>0)then
            x2=(x*eigen+x1*eigen1)/(eigen+eigen1)           
            !同号缺陷复合，坐标为本征缺陷的加权平均值
        else
            !异号缺陷复合，小缺陷向大缺陷移动
            if(abs(eigen)>abs(eigen1))then
                x2=x+(x-x1)*abs(eigen1)/(abs(eigen1)+abs(eigen))
            else
                x2=x1+(x1-x)*abs(eigen)/(abs(eigen1)+abs(eigen))
            endif
        endif
    endif
                

    if(i1==nclu)then                                     !若i1在clu列表末端，先删除i0会使得i1位置移动
	    call dele(i1) 
        call dele(i) 
    else
        call dele(i) 
        call dele(i1)
    endif
    
   
    call add(x2,orien,formula)                      !将聚合后的团簇看做一个新的团簇，add到体系中去
    aggregated=nclu

10  end