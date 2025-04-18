module JCloader
    use json_module
    implicit none

    type client
        character(len=:), allocatable :: dpi, nombre, password
    end type client
    
    type(client), dimension(:), allocatable :: clients
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

        dpi = ''
        nombre = ''
        password = ''

        allocate(clients(size))

        print*, ('Clientes: ')
        do i = 1, size
            call jsoncore%get_child(listPointer, i, personPointer, found = found)

            call jsoncore%get_child(personPointer, 'dpi', attributePointer, found = found)
            if (found) then
                call jsoncore%get(attributePointer, dpi)
                clients(i)%dpi = dpi
            end if

            call jsoncore%get_child(personPointer, 'nombre_cliente', attributePointer, found = found)
            if (found) then
                call jsoncore%get(attributePointer, nombre)
                clients(i)%nombre = nombre
            end if

            call jsoncore%get_child(personPointer, 'password', attributePointer, found = found)
            if(found) then
                call jsoncore%get(attributePointer, password)
                clients(i)%password = password
            end if

            if(found) then
                print*, 'DPI: ', trim(clients(i)%dpi)
                print*, 'nombre: ', trim(clients(i)%nombre)
                print*, 'password: ', trim(clients(i)%password)
                print*, ' '
            end if
        end do

        !Utilizar el menu de ModAdmin
        print*, ' '
        print*, '1. Cargar usuarios'
        print*, '2. Operaciones'
        print*, '3. Reportes'
        print*, '4. Logout'
    end subroutine
end module JCloader
!C:\Users\jpsam\OneDrive\Escritorio\Clientes.json
!C:\Users\jpsam\OneDrive\Escritorio\Clientes1.json