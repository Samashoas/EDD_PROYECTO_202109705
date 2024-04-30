module JTLoader
    use json_module
    implicit none
    
    type Tecnico
        character(len=:), allocatable :: dpi, nombre, apellido, genero, direccion
        integer :: telefono
    end type Tecnico

    type(Tecnico), dimension(:), allocatable :: tecnicos

    contains
        subroutine LoadJT()
            character(len=1000) :: filename
            type(json_file) :: json
            type(json_value), pointer :: listPointer, PersonPointer, attributePointer
            type(json_core) :: jsoncore

            integer :: i, size
            logical :: found

            character(len=:), allocatable :: dpi, nombre, apellido, genero, direccion

            write (*, '(A, I0, A)', advance='no') 'Ingrese la direccion del archivo JSON: '
            read(*, '(A)') filename

            call json%initialize()
            call json%load(filename=trim(filename))
            call json%print()
            print*, ''

            call json%info('',n_children=size)
            call json%get_core(jsoncore)
            call json%get('', listPointer, found)

            dpi = ''
            nombre = ''
            apellido = ''
            genero = ''
            direccion = ''

            allocate(tecnicos(size))

            !print*, 'Tecnicos'
            do i = 1, size
                call jsoncore%get_child(listPointer, i, PersonPointer, found=found)
                
                !DPI
                call jsoncore%get_child(PersonPointer, 'dpi', attributePointer, found=found)
                if(found) then
                    call jsoncore%get(attributePointer, dpi)
                    tecnicos(i)%dpi = dpi
                end if

                !NOMBRE
                call jsoncore%get_child(PersonPointer, 'nombre', attributePointer, found = found)
                if (found) then
                    call jsoncore%get(attributePointer, nombre)
                    tecnicos(i)%nombre = nombre
                end if

                !APELLIDO
                call jsoncore%get_child(PersonPointer, 'apellido', attributePointer, found = found)
                if (found) then
                    call jsoncore%get(attributePointer, apellido)
                    tecnicos(i)%apellido = apellido
                end if

                !GENERO
                call jsoncore%get_child(PersonPointer, 'genero', attributePointer, found = found)
                if (found) then
                    call jsoncore%get(attributePointer, genero)
                    tecnicos(i)%genero = genero
                end if

                !DIRECCION
                call jsoncore%get_child(PersonPointer, 'direccion', attributePointer, found = found)
                if (found) then
                    call jsoncore%get(attributePointer, direccion)
                    tecnicos(i)%direccion = direccion
                end if

                call jsoncore%get_child(PersonPointer, 'telefono', attributePointer, found=found)
                if(found) then
                    call jsoncore%get(attributePointer, tecnicos(i)%telefono)
                end if

                if(found) then
                    print*, 'id: ', trim(tecnicos(i)%dpi)
                    print*, 'nombre: ', trim(tecnicos(i)%nombre)
                    print*, 'apellido: ', trim(tecnicos(i)%apellido)
                    print*, 'genero: ', trim(tecnicos(i)%genero)
                    print*, 'direccion: ', trim(tecnicos(i)%direccion)
                    print*, 'telfono: ', tecnicos(i)%telefono
                    print*, ''
                end if
            end do

            print*, '1. Cargar Sucursales'
            print*, '2. Cargar Rutas'
            print*, '3. Salir'
        end subroutine
end module JTLoader