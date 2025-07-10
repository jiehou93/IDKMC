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
              
        subroutine load_ion_database_cfg(cascade_in,i_beam)    
            use typ
            implicit none
            integer i_beam
            type(ion_damage),allocatable,intent(inout):: cascade_in(:)
        end subroutine load_ion_database_cfg
    end interface
end module intf
    