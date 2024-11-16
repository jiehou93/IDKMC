    subroutine extend()
    !将缺陷数组的大小扩展10%+2。
    use typ
    implicit none
    integer*4 i,j,clu_length
    type(cluster),target,allocatable::clu_tem(:)

    clu_length=nclu+1
    allocate(clu_tem(0:nclu))

    do i=0,nclu                                     !备份clu数组
        nullify(clu_tem(i)%up)
        call backup(clu_tem(i),clu(i))
    enddo

    deallocate(clu)
    allocate(clu(0:int(clu_length*1.1)+2))              !clu数组大小扩展10%+2

    do i=0,nclu                                     !还原clu数组
        call backup(clu(i),clu_tem(i))
    enddo
    do i=clu_length,size(clu)-1                         !令空对象的up指针空指
        nullify(clu(i)%up)
        clu(i)%sn=i
    enddo
    deallocate(clu_tem)                             !释放临时数组的内存
    end subroutine
