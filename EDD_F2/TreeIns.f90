module TInsert
    use JCloader
    use BTree

contains
    subroutine Insert_Btree()
        integer(8) :: i, dpi_val, setValue_arg
        type(BTreeNode), pointer :: child
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
        call traversal(root)
        print*, ' '
        print*, ' '
    end subroutine Insert_Btree
end module TInsert