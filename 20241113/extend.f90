    subroutine extend()
    !��ȱ������Ĵ�С��չ10%+2��
    use typ
    implicit none
    integer*4 i,j,clu_length
    type(cluster),target,allocatable::clu_tem(:)

    clu_length=nclu+1
    allocate(clu_tem(0:nclu))

    do i=0,nclu                                     !����clu����
        nullify(clu_tem(i)%up)
        call backup(clu_tem(i),clu(i))
    enddo

    deallocate(clu)
    allocate(clu(0:int(clu_length*1.1)+2))              !clu�����С��չ10%+2

    do i=0,nclu                                     !��ԭclu����
        call backup(clu(i),clu_tem(i))
    enddo
    do i=clu_length,size(clu)-1                         !��ն����upָ���ָ
        nullify(clu(i)%up)
        clu(i)%sn=i
    enddo
    deallocate(clu_tem)                             !�ͷ���ʱ������ڴ�
    end subroutine
