module cola_module
    implicit none
    private

    type, public :: node
        private
        character(100) :: id
        character(100) :: nombre
        character(100) :: img_g
        character(100) :: img_p
        type(node), pointer :: next     
    end type node

    type, public :: cola
        private
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: delete
        procedure :: print
        procedure :: graficar
    end type cola

contains
    subroutine append(this, id, nombre, img_g, img_p)
        class(cola), intent(inout) :: this
        character(len=*), intent(in) :: id, nombre, img_g, img_p

        type(node), pointer :: temp
        allocate(temp)
        temp%id = id
        temp%nombre = nombre
        temp%img_g = img_g
        temp%img_p = img_p
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
            this%tail => temp
        else
            this%tail%next => temp
            this%tail => temp
        end if

        print *, 'Append ', nombre
    end subroutine append

    subroutine delete(this)
        class(cola), intent(inout) :: this
        type(node), pointer :: temp

        if (.not. associated(this%head)) then
            print *, 'Cola esta vacia'
            return
        end if

        print *, 'Delete ', this%head%nombre
        temp => this%head
        this%head => this%head%next
        deallocate(temp)
    end subroutine delete

    subroutine print(this)
        class(cola), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        print *, '//-----------------//'
        print *, 'La cola es:'
        print *, '//-----------------//'

        do while (associated(current))
            print *, 'id: ', current%id
            print *, 'nombre: ', current%nombre
            print *, 'Imagenes grandes: ', current%img_g
            print *, 'imagenes pequenas: ', current%img_p
            current => current%next
        end do 
    end subroutine print

    subroutine graficar(this, filename)
        class(cola), intent(in) :: this
        character(len=*), intent(in) :: filename
    
        integer :: unit
        type(node), pointer :: current
        integer :: count
        character(len=10) :: count_str
        character(len=10) :: count_next_str
    
        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph Queue {'
        write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=pink];' 
        current => this%head
        count = 0
        do while (associated(current))
            count = count + 1
            write(count_str, '(I0)') count
            write(unit, '(A)') '    "Node' // trim(adjustl(count_str)) // '" [label="Nombre: ' // &
                                trim(current%nombre) // '\nId: ' // trim(current%id) // &
                                '\nimg_g: ' // trim(current%img_g) // '\nimg_p: ' // trim(current%img_p) // '"];'
            if (associated(current%next)) then
                write(count_next_str, '(I0)') count + 1
                write(unit, '(A)') '    "Node' // trim(adjustl(count_str)) // '" -> "Node' // trim(adjustl(count_next_str)) // '";'
            end if
            current => current%next
        end do 
    
        write(unit, *) '}'
        close(unit)
    
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
    
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
    end subroutine graficar    
end module cola_module
