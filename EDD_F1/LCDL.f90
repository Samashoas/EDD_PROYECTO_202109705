module LCD_list
    implicit none
    private

    type, public :: node
        private
        character(100) :: id
        character(100) :: nombre
        character(100) :: img_g
        character(100) :: img_p
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null() 
    end type node

    type, public :: linked_list
        private
        type(node), pointer :: head => null()
    contains
        procedure :: append
        procedure :: print
        procedure :: delete
        procedure :: graficar
    end type linked_list

contains

subroutine append(this, id, nombre, img_g, img_p)
    class(linked_list), intent(inout) :: this
    character(len=*), intent(in) :: id, nombre, img_g, img_p
    
    type(node), pointer :: new

    if (.not. associated(this%head)) then
        ! Create a new node if the list is empty
        allocate(new)
        new%id = id
        new%nombre = nombre
        new%img_g = img_g
        new%img_p = img_p
        new%next => new
        new%prev => new
        this%head => new
    else
        ! Update the existing node with the new information
        this%head%id = id
        this%head%nombre = nombre
        this%head%img_g = img_g
        this%head%img_p = img_p
    end if

    print *, "Elemento ingresado a la lista: ", nombre
end subroutine append

    subroutine print(this)
        class(linked_list), intent(inout) :: this
        type(node), pointer :: current

        if (.not. associated(this%head)) then
            print *, "La lista está vacía."
            return
        end if

        current => this%head

        do while (associated(current))
            print *, current%nombre

            if (associated(current%next, this%head)) exit

            current => current%next       
        end do
    end subroutine print

    subroutine delete(this, nombre)
        class(linked_list), intent(inout) :: this
        character(len=*), intent(in) :: nombre
        type(node), pointer :: current, previous

        if (.not. associated(this%head)) then
            print *, "La lista está vacía. No se puede eliminar el valor: ", nombre
            return
        end if

        current => this%head
        previous => null()

        do while (associated(current) .and. current%nombre /= nombre)
            previous => current
            current => current%next
        end do

        if (associated(current) .and. current%nombre == nombre) then
            if (associated(previous)) then
                previous%next => current%next
                current%next%prev => previous 
            else
                this%head => current%next
                current%next%prev => previous 
            end if
            
            deallocate(current)
            print *, "Se ha eliminado correctamente el valor: ", nombre
        else
            print *, "No se ha encontrado el valor: ", nombre
        end if
    end subroutine delete
    
    subroutine graficar(this, filename)
        class(linked_list), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer :: unit
        type(node), pointer :: current
        integer :: count
        character(len=10) :: count_str
        character(len=10) :: count_next_str

        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph Linked_List {'
        write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=pink];' 

        current => this%head
        count = 0
        do while (associated(current))
            count = count + 1
            write(count_str, '(I0)') count
            write(unit, '(A)') '    "Node' // trim(adjustl(count_str)) // '" [label="ID: ' // trim(current%id) // &
                                '\nNombre: ' // trim(current%nombre) // '\nImg_g: ' // trim(current%img_g) // &
                                '\nImg_p: ' // trim(current%img_p) // '"];'
            if (associated(current%next)) then
                write(count_next_str, '(I0)') count + 1
                write(unit, '(A)') '    "Node' // trim(adjustl(count_str)) // '" -> "Node' // trim(adjustl(count_next_str)) // '";'
            end if
            current => current%next
        end do 

        write(unit, *) '}'
        close(unit)

        call system('dot -Tpng "' // trim(filename) // '" -o "' // trim(filename) // '.png"')

        print *, 'Graphviz file generated: ', trim(filename) // '.png'
    end subroutine graficar    
end module LCD_list
