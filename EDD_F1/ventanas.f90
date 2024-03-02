module ventana_mod
    implicit none

    type, public ::node
        private
        integer :: value
        type(node), pointer :: next     
    end type node

    type, public :: linked_list
        private
        type(node), pointer :: head => null()
    contains
        procedure :: push
        procedure :: append
        procedure :: print
        procedure :: delete
        procedure :: search
        procedure :: graficar
    end type linked_list

    contains

    subroutine push(this, value)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: value

        type(node), pointer :: temp
        allocate(temp)
        temp%value = value
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            temp%next => this%head
            this%head => temp
        end if

        print *, 'pushed ', value
    end subroutine push

    subroutine append(this, value)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: value

        type(node), pointer :: temp
        type(node), pointer :: current

        allocate(temp)
        temp%value = value
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
        else
            current => this%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => temp
        end if

        print *, 'appended ', value
    end subroutine append

    subroutine delete(this, value)
        class(linked_list), intent(inout) :: this
        integer, intent(in) :: value
        type(node), pointer :: current, previous

        current => this%head
        previous => null()

        do while (associated(current) .and. current%value /= value)
            previous => current
            current => current%next
        end do

        if(associated(current) .and. current%value == value) then
            if(associated(previous)) then
                previous%next => current%next
            else
                this%head => current%next
            end if

            deallocate(current)
            print *, 'Se eliminó el valor ', value
        else
            print *, 'No se encontró el valor ', value
        end if

    end subroutine delete

    function search(this, value) result(retval)
        class(linked_list), intent(in) :: this
        integer, intent(in) :: value

        type(node), pointer :: current

        logical :: retval

        current => this%head
        retval = .false.

        do while(associated(current))
            if(current%value == value) then
                retval = .true.
                exit
            end if
            current => current%next
        end do

    end function search

    subroutine print(this)
        class(linked_list), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        do while (associated(current))
            print *, current%value
            current => current%next
        end do 
    end subroutine print

    subroutine numWin()
        integer :: num_ventanas, i
        type(linked_list) :: windows
        do
            write(*, '(A, I0, A)', advance='no') 'Ingrese el numero de ventanas: '
            read(*, *) num_ventanas

            if (num_ventanas > 0) then
                exit
            else
                write(*, *) 'El número de ventanas debe ser mayor a 0'
            end if
        end do

        do i = 1, num_ventanas
            call windows%append(i)
        end do

            call windows%graficar('ventanas.dot', 'Ventanilas')

        if (num_ventanas == 1) then
            write(*, '(A, I0, A)', advance='no') 'Se ha ingresado: ', num_ventanas, ' ventana'
            print*, (' ')
        else
            write(*, '(A, I0, A)', advance='no') 'Se han ingresado: ', num_ventanas, ' ventanas'
            print*, (' ')
        end if

        print*, (' ')
        print*, '1. Cargar clientes'
        print*, '2. Ingresar ventanillas'
        print*, '3. regresar'
    end subroutine numWin

    subroutine graficar(this, filename, title)
        class(linked_list), intent(in) :: this
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: title

        integer :: unit
        type(node), pointer :: current
        integer :: count

        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph Linked_List {'
        write(unit, '(A)') '    labelloc="t";'
        write(unit, '(A)') '    label= "' //trim(title) //'";'
        write(unit, *) '    rankdir=LR;'
        write(unit, *) ' node [shape=box, style=filled, color=blue, fillcolor=pink];'
        
        current => this%head
        count = 0
        do while (associated(current))
            count = count + 1
            write(unit, *) '    "Node', count, '" [label="Ventanilla: ', current%value, '"];'
            if (associated(current%next)) then
                write(unit, *) '    "Node', count, '" -> "Node', count+1, '";'
            end if
            current => current%next
        end do 

        write(unit, *) '}'
        close(unit)

        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')

        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
    end subroutine graficar

end module ventana_mod