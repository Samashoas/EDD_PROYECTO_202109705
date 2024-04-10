module JCAloader
    use json_module
    use matrix_m
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
    type(matrix) :: m
    contains
        subroutine LoadJsonCAP()
            character(len=100) :: filename
            type(json_file) :: json
            type(json_value), pointer :: listPointer, capaPointer, pixelPointer, attributePointer
            type(json_value), pointer :: filaPointer, columnaPointer, colorPointer
            type(json_core) :: jsoncore
            integer :: i, j, size, pixelSize 
            logical :: found
            character(len=:), allocatable :: color

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
                            call jsoncore%get_child(attributePointer, 'fila', filaPointer, found = found)
                            if(found) then
                                call jsoncore%get(filaPointer, capas(i)%pixeles(j)%fila)
                            end if
                            call jsoncore%get_child(attributePointer, 'columna', columnaPointer, found = found)
                            if(found)then
                                call jsoncore%get(columnaPointer, capas(i)%pixeles(j)%columna)
                            end if
                            call jsoncore%get_child(attributePointer, 'color', colorPointer, found = found)
                            if(found)then
                                call jsoncore%get(colorPointer, color)
                                capas(i)%pixeles(j)%color = trim(color)
                            end if
                        end if
                        print*, 'Fila: ', capas(i)%pixeles(j)%fila
                        print*, 'Columna: ', capas(i)%pixeles(j)%columna
                        print*, 'Color: ', trim(capas(i)%pixeles(j)%color)

                        call m%insert(capas(i)%pixeles(j)%fila, capas(i)%pixeles(j)%columna, trim(capas(i)%pixeles(j)%color))
                    end do
                end if
            end do
            call m%print()
            call m%generate_dot('matriz.dot')
            call execute_command_line('dot -Tpng matriz.dot -o matriz.png')
            call execute_command_line('start "" "matriz.png"')

        print*, ' '
        print*, '1. Capas: '
        print*, '2. Imagenes: '
        print*, '3. Albumes: '
        print*, '4. Regresar'
        end subroutine
end module JCAloader