    subroutine para_test()
    !�ֶ����Բ��������Ƿ���ȷ
    use typ
    implicit none

    integer*4 formula(element),i,j,k,l
    real*8 parameters

    formula=0
    do
        write(*,*)'������Ҫ��ѯȱ�ݵ����,����4��0�˳���ѯ��'
        read(*,*)formula
        if(formula(1)==0.and.formula(2)==0) exit
        !write(*,*)'em=   ',parameters(1,formula)
        !write(*,*)'em+er=',parameters(2,formula)
        !write(*,*)'em+eb=',parameters(3,formula)
        !write(*,*)'r=    ',parameters(4,formula)
        !write(*,*)'ve=   ',parameters(5,formula)
        !write(*,*)'vm=   ',parameters(6,formula)
        !write(*,*)'emit= ',parameters(7,formula)
        !write(*,*)'step= ',parameters(8,formula)
    enddo
    !open(1005,file='radius.txt')


    !do i=-5000,5000
    !    !do j=0,nint(12*abs(i)**(2.0/3.0))+10
    !        write(1001,'(I5,3F12.4)')i,parameters(4,(/i,0,0,0/)),parameters(4,(/i,0,0,0/))/3.1652
    !    !enddo
    !enddo

    !close(1005)
    stop
    end subroutine