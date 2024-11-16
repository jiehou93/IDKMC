    subroutine pointer_move(pointer_dummy,n)
        !��pointer_dummy ָ�������ƶ�n��λ��
        use typ
        implicit none
        type(cell_list),pointer::pointer_dummy
        integer*4 i,j,k,n
        
        do i=1,n
            pointer_dummy=>pointer_dummy%next
        enddo
	end subroutine pointer_move
    
	subroutine add_node(node_dummy)
        !��node_dummy �����һ���½ڵ�
        use typ
        implicit none
        type(cell_list),target::node_dummy
        type(cell_list),pointer::temp_dummy
        
        !����ڵ�                            
        if(associated(node_dummy%next))then
            !��һ���ڵ㲻Ϊ��
            temp_dummy=>node_dummy%next                       !����ԭ��һ�ڵ��ַ
            allocate(node_dummy%next)                         !�����½ڵ�
            node_dummy%next%previous=>node_dummy              !�½����ӵ���ǰ�ڵ��
            node_dummy%next%next=>temp_dummy                  !ԭ��һ�ڵ����ӵ��½ڵ��
            temp_dummy%previous=>node_dummy%next              !
        else
            !��һ���ڵ�Ϊ�գ�ֱ�Ӳ����½ڵ�
            allocate(node_dummy%next)
            node_dummy%next%previous=>node_dummy
            nullify(node_dummy%next%next)
            nullify(temp_dummy)
        endif
        
    end subroutine add_node

	subroutine dele_node(node_dummy)
        !ɾ��node_dummy �ڵ�
        use typ
        implicit none
        type(cell_list),pointer::node_dummy,temp_dummy
        
        if(associated(node_dummy%next))then
            !��һ���ڵ㲻Ϊ�գ������½ڵ��໥����
            node_dummy%next%previous=>node_dummy%previous
		    node_dummy%previous%next=>node_dummy%next
            deallocate(node_dummy)
        else
            !��һ���ڵ�Ϊ�գ�ֱ��ɾ����ǰ�ڵ�
            temp_dummy=>node_dummy%previous
            deallocate(temp_dummy%next)
        endif
        
    end subroutine dele_node