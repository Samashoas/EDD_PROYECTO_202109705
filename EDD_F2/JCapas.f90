module JCAloader
    use json_module
    implicit none

    type pixel
        integer :: fila, columna
        character(len=:), allocatable :: color
    end type pixel

    type capa
        integer :: id_capa
        type(pixel), dimension(:), allocatable :: pixeles
    end type capa

    type(capa), dimension(:), allocatable :: capas

    contains
        subroutine LoadJsonCAP()
            character(len=100) :: filename
            type(json_file) :: json
            type(json_value), pointer :: listPointer, capaPointer, pixelPointer, attributePointer
            type(json_core) :: jsoncore
            integer :: i, j, size, pixelSize 
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

            allocate(capas(size))

            do i =1, size
                call jsoncore%get_child(listPointer, i, capaPointer, found = found)

                call jsoncore%get_child(capaPointer, 'id_capa', attributePointer, found = found)
                if(found)then
                    call jsoncore%get(attributePointer, capas(i)%id_capa)
                    print*, 'Capa: ', capas(i)%id_capa
                end if

                call jsoncore%get_child(capaPointer, 'pixeles', pixelPointer, found = found)
                if(found)then
                    call jsoncore%info(pixelPointer, n_children=pixelSize)
                    allocate(capas(i)%pixeles(pixelSize))
                    do j = 1, pixelSize
                        call jsoncore%get_child(pixelPointer, j, attributePointer, found = found)
                        if(found)then
                            call jsoncore%get_child(attributePointer, 'fila', attributePointer, found = found)
                            if(found) then
                                call jsoncore%get(attributePointer, capas(i)%pixeles(j)%fila)
                            end if
                            call jsoncore%get_child(attributePointer, 'columna', attributePointer, found = found)
                            if(found)then
                                call jsoncore%get(attributePointer, capas(i)%pixeles(j)%columna)
                            end if
                            call jsoncore%get_child(attributePointer, 'color', attributePointer, found = found)
                            if(found)then
                                call jsoncore%get(attributePointer, capas(i)%pixeles(j)%color)
                            end if
                        end if
                    end do
                end if
            end do

        print*, ' '
        print*, '1. Capas: '
        print*, '2. Imagenes: '
        print*, '3. Albumes: '
        print*, '4. Regresar'
        end subroutine
end module JCAloader