module TInsert
    use JCloader
    use BTree

contains
    subroutine Insert_Btree()
        integer(8) :: i, dpi_val, setValue_arg
        type(BTreeNode), pointer :: child
        character(len=50) :: cmd, cmd2
        allocate(child)

        do i = 1, size(clients)
            read(clients(i)%dpi, *) dpi_val

            if(setValue(dpi_val, setValue_arg, root, child))then
                root => createNode(setValue_arg, child)
            end if
        end do

        print*, 'Clientes Insertados en el arbol B'
        print*, ' '
        print*, 'Imprimiendo arbol B'
        print*, ' '
        call generateGraphviz(root, 'btree.dot')

        cmd = 'dot -Tpng btree.dot -o btree.png'

        call execute_command_line(cmd)
        print*, 'Arbol generado'

        cmd2 = 'start "" "btree.png"'
        call execute_command_line(trim(cmd2))

        print*, ' '
        print*, '1. Arbol B de usuarios'
        print*, '2. Buscar usuarios'
        print*, '3. Listar usuarios'
        print*, '4. Regresar'
    end subroutine Insert_Btree

    subroutine printBtree()
        call traversal(root)
        print*, ' '

        print*, ' '
        print*, '1. Arbol B de usuarios'
        print*, '2. Buscar usuarios'
        print*, '3. Listar usuarios'
        print*, '4. Regresar'
    end subroutine printBtree
end module TInsert