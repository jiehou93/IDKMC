    subroutine random_add(formula)
    !向体系中随机添加一个指定成分缺陷
    use typ
    implicit none
    integer*4 i,v,k,orien,formula(element),nclu1,vicinity,aggregated
    real*8 ran1,ran2,ran3,ran4,alpha,beta,rad,coord(3)

    !call RANDOM_NUMBER(ran1)                        
    !call RANDOM_NUMBER(ran2)
    !call RANDOM_NUMBER(ran3)
    call RANDOM_NUMBER(ran4)

    call RANDOM_NUMBER(coord)
    coord=coord*length 
    !if(sum(pbc)>=0)then                                 !在盒子里随机生成一个坐标
    !    coord(1)=length(1)*ran1
    !    coord(2)=length(2)*ran2
    !    coord(3)=length(3)*ran3
    !else
    !    alpha=ran1*2*pi
    !    beta=acos(1-2*ran2)                             !注意极角和半径的分布函数！！！
    !    rad=ran3**(1.0/3.0)*grain_radi                  !在球形晶界内随机一个坐标
    !    coord(1)=rad*sin(beta)*cos(alpha)
    !    coord(2)=rad*sin(beta)*sin(alpha)
    !    coord(3)=rad*cos(beta)
    !endif

    orien=ran4*8+1                                      !随机方向

    call add_and_vicinity(coord,orien,formula)          !在该位置加入一个该类型单缺陷团簇
    end subroutine
