    Integer Function GetFileN(iFileUnit)
    !查询文件行数函数
    Implicit None
    Integer*4,Intent(IN)::iFileUnit
    character(Len=1)::cDummy
    integer*4::ierr
    GetFileN=0
    Rewind(iFileUnit)
    Do
        Read(iFileUnit,'(A1)',ioStat=ierr)cDummy            !计数包括空行
        If(ierr/=0) Exit                                    !最后一行退出
        GetFileN=GetFileN+1
    End Do
    Rewind(iFileUnit)                                       !返回第一行位置
    End Function GetFileN