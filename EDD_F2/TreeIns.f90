module TInsert
    use JCloader
    use JCAloader
    use JImgLoader
    use AlbumLoader
    use BTree
    use abb_m
    use avldef
    use linked_list_module
    use matrix_m
    implicit none

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

    subroutine InsertABB()
        type(abb) :: tree
        integer :: i
        character(len=100) :: cmd

        do i = 1, size(capas)
            call tree%insert(capas(i)%id_capa)
        end do

        call tree%graph("inserted")

        print*, "Preorden"
        call tree%preorder()
        print*, ' '
        print*, "Inorden"
        call tree%inorder()
        print*, ''
        print*, "Posorden"
        call tree%posorder()

        cmd = 'start "" "inserted.png"'
        call execute_command_line(cmd)

        print*, ' '
        print*, '1. Arbol de imagenes'
        print*, '2. Arbol de capas'
        print*, '3. Listado de albums'
        print*, '4. Imagen y capa'
        print*, '5. regresar'
    end subroutine InsertABB

    subroutine InsertAVL()
        type(avl) :: t	
        integer :: unit, i
        character(len=100) :: filename, cmd
        filename = 'avl.dot'
        open(unit, file=filename, status='replace')	

        do i = 1, size(capasi)
            call t%add(capasi(i)%id)
        end do
        
        print *, 'Preorder:'
        call t%preorder(t%root)
        print *, ''
        print *, 'Inorder:'
        call t%inorder(t%root)
        print *, ''
        print *, 'Postorder:'
        call t%postorder(t%root)
        print *, ''
        
        print *, 'Generating Dot file...'
        call t%dotgen(t%root, unit)
        close(unit)
        print *, 'Dot file generated:', trim(filename)
        call execute_command_line('dot -Tpng avl.dot > avl.png')

        cmd = 'start "" "avl.png"'
        call execute_command_line(cmd)

        print*, ' '
        print*, '1. Arbol de imagenes'
        print*, '2. Arbol de capas'
        print*, '3. Listado de albums'
        print*, '4. Imagen y capa'
        print*, '5. regresar'
    end subroutine InsertAVL

    subroutine InsertCircularList()
        Type(linked_list) :: list
        integer :: i
        character(len=100) cmd, cmd2

        if(.not. allocated(albums))then
            print*, 'No hay albums'
            return
        end if

        do i = 1, size(albums)
            call list%append(i)
        end do

        print*, ' '

        do i = 1, size(albums)
            print*, 'ID: ', i, ' Nombre del album: ', trim(albums(i)%nombre_album)
        end do

        print*, ' '
        print*, 'Datos de la lista'
        call list%print()
        call list%generate_dot('cl.dot')

        cmd = 'dot -Tpng cl.dot -o cl.png'
        call execute_command_line(cmd)

        cmd2 = 'start "" "cl.png"'
        call execute_command_line(cmd2)

    end subroutine InsertCircularList
end module TInsert