    subroutine pointer_move(pointer_dummy,n)
        !将pointer_dummy 指针往后移动n个位置
        use typ
        implicit none
        type(cell_list),pointer::pointer_dummy
        integer*4 i,j,k,n
        
        do i=1,n
            pointer_dummy=>pointer_dummy%next
        enddo
	end subroutine pointer_move
    
	subroutine add_node(node_dummy)
        !在node_dummy 后插入一个新节点
        use typ
        implicit none
        type(cell_list),target::node_dummy
        type(cell_list),pointer::temp_dummy
        
        !插入节点                            
        if(associated(node_dummy%next))then
            !下一个节点不为空
            temp_dummy=>node_dummy%next                       !保存原下一节点地址
            allocate(node_dummy%next)                         !建立新节点
            node_dummy%next%previous=>node_dummy              !新节连接到当前节点后方
            node_dummy%next%next=>temp_dummy                  !原下一节点连接到新节点后方
            temp_dummy%previous=>node_dummy%next              !
        else
            !下一个节点为空，直接插入新节点
            allocate(node_dummy%next)
            node_dummy%next%previous=>node_dummy
            nullify(node_dummy%next%next)
            nullify(temp_dummy)
        endif
        
    end subroutine add_node

	subroutine dele_node(node_dummy)
        !删除node_dummy 节点
        use typ
        implicit none
        type(cell_list),pointer::node_dummy,temp_dummy
        
        if(associated(node_dummy%next))then
            !下一个节点不为空，将上下节点相互链接
            node_dummy%next%previous=>node_dummy%previous
		    node_dummy%previous%next=>node_dummy%next
            deallocate(node_dummy)
        else
            !下一个节点为空，直接删除当前节点
            temp_dummy=>node_dummy%previous
            deallocate(temp_dummy%next)
        endif
        
    end subroutine dele_node