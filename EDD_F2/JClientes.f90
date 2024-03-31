module JCloader
    use json_module
    implicit none

contains
    subroutine LoadJsonC()
        character(len=100) :: filename
        type(json_file) :: json
        type(json_value), pointer :: listPointer, personPointer, attributePointer
        type(json_core) :: jsoncore
        character(len=:), allocatable :: dpi, nombre, password

        integer :: i, size
        logical :: found

        write (*, '(A, I0, A)', advance='no') 'Ingrese la direccion del archivo JSON: '
        read(*, '(A)') filename
        call json%initialize()
        call json%load(filename=trim(filename))
        call json%print()
        print*, (' ')

        call json%info('',n_children=size)
        call json%get_core(jsoncore)
        call json%get('', listPointer, found)

        print*, ('Clientes: ')
        do i = 1, size
            call jsoncore%get_child(listPointer, i, personPointer, found = found)

            call jsoncore%get_child(personPointer, 'dpi', attributePointer, found = found)
            if (found) then
                call jsoncore%get(attributePointer, dpi)
            end if

            call jsoncore%get_child(personPointer, 'nombre_cliente', attributePointer, found = found)
            if (found) then
                call jsoncore%get(attributePointer, nombre)
            end if

            call jsoncore%get_child(personPointer, 'password', attributePointer, found = found)
            if(found) then
                call jsoncore%get(attributePointer, password)
            end if

            if(found) then
                print*, 'DPI: ', trim(dpi)
                print*, 'nombre: ', trim(nombre)
                print*, 'password: ', trim(password)
                print*, ' '
            end if
        end do
    end subroutine
end module JCloader