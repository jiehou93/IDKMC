module intf
    interface
        subroutine add_node(node_dummy)
            use typ
            implicit none
            type(cell_list),target::node_dummy
        end subroutine add_node
        
        subroutine dele_node(node_dummy)
            use typ
            implicit none
            type(cell_list),pointer::node_dummy
        end subroutine dele_node
    end interface
end module intf
    