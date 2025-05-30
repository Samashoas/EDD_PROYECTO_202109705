module JImgLoader
    use json_module
    implicit none

    type capa
        integer :: id
        integer, dimension(:), allocatable :: capasi
    end type capa

    type(capa), dimension(:), allocatable :: capasi

contains
    subroutine LoadJsonImg()
        character(len=100) :: filename
        type(json_file) :: json
        type(json_value), pointer :: listPointer, capaPointer, capasPointer, attributePointer
        type(json_core) :: jsoncore
        integer :: i, j, size, capasSize
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

        allocate(capasi(size))

        do i = 1, size
            call jsoncore%get_child(listPointer, i, capaPointer, found = found)

            call jsoncore%get_child(capaPointer, 'id', attributePointer, found = found)
            if (found) then
                call jsoncore%get(attributePointer, capasi(i)%id)
            end if

            call jsoncore%get_child(capaPointer, 'capas', capasPointer, found = found)
            if (found) then
                call jsoncore%info(capasPointer, n_children=capasSize)
                allocate(capasi(i)%capasi(capasSize))
                do j = 1, capasSize
                    call jsoncore%get_child(capasPointer, j, attributePointer, found = found)
                    if (found) then
                        call jsoncore%get(attributePointer, capasi(i)%capasi(j))
                    end if
                end do
            end if

            ! Imprimir los datos de la capa
            print*, 'id ', capasi(i)%id
            print*, 'capas ', capasi(i)%capasi
        end do


        !Utilizar el menu de ModAdmin
        print*, ' '
        print*, '1. Capas: '
        print*, '2. Imagenes: '
        print*, '3. Albumes: '
        print*, '4. Regresar'
    end subroutine 

end module JImgLoader
