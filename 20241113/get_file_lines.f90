    Integer Function GetFileN(iFileUnit)
    !��ѯ�ļ���������
    Implicit None
    Integer*4,Intent(IN)::iFileUnit
    character(Len=1)::cDummy
    integer*4::ierr
    GetFileN=0
    Rewind(iFileUnit)
    Do
        Read(iFileUnit,'(A1)',ioStat=ierr)cDummy            !������������
        If(ierr/=0) Exit                                    !���һ���˳�
        GetFileN=GetFileN+1
    End Do
    Rewind(iFileUnit)                                       !���ص�һ��λ��
    End Function GetFileN