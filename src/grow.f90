    subroutine grow(i)
    !�޸ģ���ָ����ָ��
    !�������������򣬼���һ�������Ϊ��������һƬ��Ҷ������ǰһ������Ҷ�������ڵ�ı��͹�ϵ��������ν�����Ҷ�������������ϡ�
    use typ
    implicit none
    integer*4 i


    allocate(clu(i)%up)                             !�½�һ���µĶ���ڵ㣨��Ҷ��������������i��
    clu(i)%up%obj=>clu(i)
    clu(i)%up%rate=0                                !��ʼ������Ϊ0������ֻ�Ƿ���ռ䣬��û�����ȱ����Ϣ
    nullify(clu(i)%up%left,clu(i)%up%right)

    current=>clu(i)%up                              !currrenָ��ýڵ�
    current_previous=>clu(i-1)%up                   !current_previousָ��ǰһ������ڵ�
    do
        if(.not.associated(current_previous%up))then            !��current_previous%upΪ���ڵ㣬�����µĸ��ڵ㣬��current��current_previous��������ȥ
            allocate(current_previous%up)                       !                   root
            root=>current_previous%up                           !                   /  \
            nullify(root%up,root%obj)                           !   current_previous    current
            root%left=>current_previous 
            root%right=>current
            root%rate=current_previous%rate
            current%up=>root
            height=height+1
            exit                                                !������ϣ��˳�ѭ�� 
        elseif(.not.associated(current_previous%up%right))then  !��current_previous�ڵ�ͬ�����ҽڵ㻹δ��������current���ӹ�ȥ
            current%up=>current_previous%up                     !                               current_previous%up
            current_previous%up%right=>current                  !                                       /  \
            exit                                                !                       current_previous    current
            !������ϣ��˳�ѭ��
        else                                                    !���ҽڵ��Ѿ���������Ϊcurrent�½�һ�����ڵ㣬����current���ӵ����ڵ����֦
            allocate(current%up)                                !       current_previous%up                 current%up
            nullify(current%up%obj,current%up%up,current%up%right)!           /  \                          /
            current%up%left=>current                            !           ****     current_previous   current
            current=>current%up                                 !����current�������ƶ�һ��
            current_previous=>current_previous%up
            current%rate=0                                      !���ڵ�����ʳ�ʼ��Ϊ0
        endif
    enddo

    end subroutine