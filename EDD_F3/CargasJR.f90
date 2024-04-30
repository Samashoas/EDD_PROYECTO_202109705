module JRLoader
    use json_module
    implicit none

    type Grafo
        integer :: s1, s2
        real :: distancia, imp_mantenimiento
    end type Grafo

    type(Grafo), dimension(:), allocatable :: grafos
    contains
        subroutine LoadJR()
            character(len=100) :: filename
            type(json_file) :: json
            type(json_value), pointer :: listPointer, grafoPointer, attributePointer
            type(json_value), pointer :: s1Pointer, s2Pointer, distanciaPointer, impMantenimientoPointer
            type(json_core) :: jsoncore
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

            allocate(grafos(size))

            do i = 1, size
                call jsoncore%get_child(listPointer, i, grafoPointer, found = found)

                call jsoncore%get_child(grafoPointer, 's1', s1Pointer, found = found)
                if(found) then
                    call jsoncore%get(s1Pointer, grafos(i)%s1)
                    print*, 's1: ', grafos(i)%s1
                end if

                call jsoncore%get_child(grafoPointer, 's2', s2Pointer, found = found)
                if(found) then
                    call jsoncore%get(s2Pointer, grafos(i)%s2)
                    print*, 's2: ', grafos(i)%s2
                end if

                call jsoncore%get_child(grafoPointer, 'distancia', distanciaPointer, found = found)
                if(found) then
                    call jsoncore%get(distanciaPointer, grafos(i)%distancia)
                    print*, 'distancia: ', grafos(i)%distancia
                end if

                call jsoncore%get_child(grafoPointer, 'imp_mantenimiento', impMantenimientoPointer, found = found)
                if(found) then
                    call jsoncore%get(impMantenimientoPointer, grafos(i)%imp_mantenimiento)
                    print*, 'imp_mantenimiento: ', grafos(i)%imp_mantenimiento
                end if
            end do

            print*, ' '
            print*, '1. Grafo: '
            print*, '2. Imágenes: '
            print*, '3. Álbumes: '
            print*, '4. Regresar'
        end subroutine
end module JRLoader

