    subroutine choose()
    !������ѡ��������ѡ��һ����Ӧ��ִ�С�
    use typ
    implicit none
    integer*4 i,j,formula(element)
    real*8 dart,ran1
    type(cluster) clu_dart

    call RANDOM_NUMBER(ran1)
    dart=ran1*root%rate                             !���ݵ�ǰ�����ʣ�����һ���������
    current=>root
    do i=1,height-1
        if(dart<current%left%rate)then              !���ڻ�����֦
            current=>current%left
        else
            dart=dart-current%left%rate             !���ڻ�����֦
            current=>current%right
        endif                                       !��ʱֱ�ӽ����˲�������Ͷ������
    enddo

    clu_dart=current%obj
    !at_end=nclu
    !at_mid=clu_dart%sn
    !write(104,*)'nclu-pos=',at_end-at_mid
    if(clu_dart%sn==0)then                          !����ѡ��
        call ion_implantation()
    elseif(dart<clu_dart%rate(1))then               !ѡ�ж������ѡ��ִ�����ַ�Ӧ
        call migrate(clu_dart%sn,.false.)           !����Ǩ��(rotation=.false.)
    elseif(dart<clu_dart%rate(2))then
        call migrate(clu_dart%sn,.true.)            !ת��Ǩ��
    elseif(dart<clu_dart%rate(3))then               !ע�⸡�����µ�������¼���ѡ�С�
        call emitting(clu_dart%sn)
11  endif

    end subroutine