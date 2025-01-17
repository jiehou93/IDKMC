subroutine renew_rate_all()
    !���ʳ�ʼ���ӳ��򣬸��ݵ�ǰ�¶ȣ�����������ϵ������
    use typ
    implicit none
    integer*4 i


    do i=0,nclu                                     !���������������
        current=>clu(i)%up
        do level=height,1,-1
            current%rate=0
            current=>current%up
        enddo
    enddo

    do i=1,nclu                                     !���¶���������Ϊ�µ��¶��µ�����
        clu(i)%rate(1)=1.0/3.0*clu(i)%para(6)*exp(-clu(i)%para(1)/kb/tem)
        clu(i)%rate(2)=clu(i)%rate(1)+2.0/3.0*clu(i)%para(6)*exp(-clu(i)%para(2)/kb/tem)
        clu(i)%rate(3)=clu(i)%rate(2)+clu(i)%para(5)*exp(-clu(i)%para(3)/kb/tem)
        !endif
    enddo

    clu(0)%rate(3)=damage_rate              !�ⲿ�¼�����
    do i=0,nclu
        current=>clu(i)%up
        do level=height,1,-1                !���¶���������
            current%rate=current%rate+clu(i)%rate(3)
            current=>current%up
        enddo
    enddo

end subroutine renew_rate_all

    
subroutine renew_rate(i)
    !���µ�i��ȱ�ݶ���������ڵ������Ӱ��
    use typ
    implicit none
    integer*4 i
    
    current=>clu(i)%up
    current%rate=clu(i)%rate(3)                         !ȱ������
    do                                                  !���¼����������Ӧ�ĸ��ڵ�����
        current=>current%up
        if(associated(current%right))then
            current%rate=current%left%rate+current%right%rate
        else
            current%rate=current%left%rate
        endif
        if(.not.associated(current%up)) exit
    enddo
end subroutine renew_rate
