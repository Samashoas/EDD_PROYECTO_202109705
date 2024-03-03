module pila_module
    implicit none
    private

    type, public :: node
        private
        character(100) :: img_g
        character(100) :: img_p
        character(100) :: nombre
        type(node), pointer :: next     
    end type node

    type, public :: pila
        private
        type(node), pointer :: top => null()
    contains
        procedure :: push
        procedure :: pop
        procedure :: print
        procedure :: graficar
    end type pila

contains

    subroutine push(this, img_g, img_p, nombre)
        class(pila), intent(inout) :: this
        character(len=*), intent(in) :: img_g, img_p, nombre

        type(node), pointer :: temp
        allocate(temp)
        temp%img_g = img_g
        temp%img_p = img_p
        temp%nombre = nombre
        temp%next => this%top
        this%top => temp

        print *, 'Pushed ', img_g
        print *, 'Pushed ', img_p
    end subroutine push

    subroutine pop(this)
        class(pila), intent(inout) :: this
        type(node), pointer :: temp

        if (.not. associated(this%top)) then
            print *, 'Pila esta vacia.'
            return
        end if
        temp => this%top

        print *, 'Popped img grande', temp%img_g
        print *, 'Popped img pequena', temp%img_p

        this%top => this%top%next
        deallocate(temp)
    end subroutine pop

    subroutine print(this)
        class(pila), intent(in) :: this
        type(node), pointer :: current

        current => this%top

        print *, '//-----------------//'
        print *, 'La pila es:'
        print *, '//-----------------//'

        do while (associated(current))
            print *, current%img_g
            print *, current%img_p
            current => current%next
        end do 
    end subroutine print

    subroutine graficar(this, filename)
        class(pila), intent(in) :: this
        character(len=*), intent(in) :: filename

        integer :: unit
        type(node), pointer :: current
        integer :: count

        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph Pile {'
        write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=pink];' ! Apply attributes to all nodes
        
        current => this%top
        count = 0
        write(unit, *) '    subgraph cluster_imgg {'
        write(unit, *) '        label="IMG GRANDE";'
        write(unit, *) '        color=lightgrey;'
        do while (associated(current))
            count = count + 1
            write(unit, *) '        "Node', count, '_imgg" [label="', trim(current%nombre), '\n', trim(current%img_g), '"];'
            
            if (count > 1) then
                write(unit, *) '        "Node', count-1, '_imgg" -> "Node', count, '_imgg";' ! Connect previous img_g node to current img_g node
            end if
            
            current => current%next
        end do 
        write(unit, *) '    }'
        
        current => this%top
        count = 0
        write(unit, *) '    subgraph cluster_imgp {'
        write(unit, *) '        label="IMG PEQUEÑA";'
        write(unit, *) '        color=lightgrey;'
        do while (associated(current))
            count = count + 1
            write(unit, *) '        "Node', count, '_imgp" [label="', trim(current%nombre), '\n', trim(current%img_p), '"];'
            
            if (count > 1) then
                write(unit, *) '        "Node', count-1, '_imgp" -> "Node', count, '_imgp";' ! Connect previous img_p node to current img_p node
            end if
            
            current => current%next
        end do 
        write(unit, *) '    }'
        
        write(unit, *) '}'
        close(unit)

        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')

        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
    end subroutine graficar
end module pila_module