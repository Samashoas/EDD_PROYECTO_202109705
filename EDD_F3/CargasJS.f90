module JSLoader
    use json_module
    implicit none
    
    type Sucursal
        integer :: id
        character(len=:), allocatable :: departamento, direccion, password
    end type Sucursal

    type(Sucursal), dimension(:), allocatable :: sucursales

    contains
        subroutine LoadJS()
            character(len=100) :: filename
            type(json_file) :: json
            type(json_value), pointer :: listPointer, SuPointer, attributePointer
            type(json_core) :: jsoncore

            integer :: i, size
            logical :: found

            character(len=:), allocatable :: departamento, direccion, password

            write (*, '(A, I0, A)', advance='no') 'Ingrese la direccion del archivo JSON: '
            read(*, '(A)') filename

            call json%initialize()
            call json%load(filename=trim(filename))
            call json%print
            print*, ''

            call json%info('',n_children=size)
            call json%get_core(jsoncore)
            call json%get('', listPointer, found)

            departamento = ''
            direccion = ''
            password = ''

            allocate(sucursales(size))

            do i = 1, size
                call jsoncore%get_child(listPointer, i, SuPointer, found=found)

                call jsoncore%get_child(SuPointer, 'id', attributePointer, found=found)
                if(found) then
                    call jsoncore%get(attributePointer, sucursales(i)%id)
                end if

                call jsoncore%get_child(SuPointer, 'departamento', attributePointer, found = found)
                if (found) then
                    call jsoncore%get(attributePointer, departamento)
                    sucursales(i)%departamento = departamento
                end if

                call jsoncore%get_child(SuPointer, 'direccion', attributePointer, found = found)
                if (found) then
                    call jsoncore%get(attributePointer, direccion)
                    sucursales(i)%direccion = direccion
                end if

                call jsoncore%get_child(SuPointer, 'password', attributePointer, found = found)
                if (found) then
                    call jsoncore%get(attributePointer, password)
                    sucursales(i)%password = password
                end if

                if(found) then
                    print*, 'id: ', sucursales(i)%id
                    print*, 'departamento: ', trim(sucursales(i)%departamento)
                    print*, 'direccion: ', trim(sucursales(i)%direccion)
                    print*, 'password: ', trim(sucursales(i)%password)
                    print*, ''
                end if
            end do

            print*, '1. Cargar Sucursales'
            print*, '2. Cargar Rutas'
            print*, '3. Salir'
        end subroutine
end module JSLoader