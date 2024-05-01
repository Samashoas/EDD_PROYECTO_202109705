module OPSUC
    use JTLoader
    implicit none

    contains

    subroutine buscar()
        character(len=100) :: dpi
        integer :: i, index
        logical :: found

        write(*, '(A)', advance='no') 'Ingrese DPI: '
        read(*,'(A)') dpi

        found = .false.

        do i=1, size(tecnicos)
            if(trim(tecnicos(i)%dpi)==trim(dpi))then
                found = .true.
                index = i
                exit
            end if
        end do

        if(.not. found)then
            print*, 'Cliente no encontrado'
            return
        end if

        print*, ' '
        print *, 'DPI: ', trim(tecnicos(index)%dpi)
        print *, 'Nombre: ', trim(tecnicos(index)%nombre)
        print *, 'Apellido: ', trim(tecnicos(index)%apellido)
        print *, 'genero: ', trim(tecnicos(index)%genero)
        print *, 'telefono: ', tecnicos(index)%telefono

        print*, ' '
        print*, '1. Cargar Tecnicos'
        print*, '2. Generar recorrido optimo'
        print*, '3. Buscar Tecnico'
        print*, '4. Listar Tecnicos'
        print*, '5. Reportes'
        print*, '6. exit'
    end subroutine buscar

    subroutine listar()
        integer :: i

        if(.not. allocated(tecnicos))then
            print*, 'No clients registered'
            return
        end if

        print *, 'Registered clients'
        do i = 1, size(tecnicos)
            print*, ' '
            write(*, '(I0, A, A)', advance='yes') i, '. DPI: ', trim(tecnicos(i)%dpi)
            print *, 'Nombre: ', trim(tecnicos(i)%nombre)
            print *, 'Apellido: ', trim(tecnicos(i)%apellido)
            print *, 'genero: ', trim(tecnicos(i)%genero)
            print *, 'telefono: ', tecnicos(i)%telefono
        end do 

        print*, ' '
        print*, '1. Cargar Tecnicos'
        print*, '2. Generar recorrido optimo'
        print*, '3. Buscar Tecnico'
        print*, '4. Listar Tecnicos'
        print*, '5. Reportes'
        print*, '6. exit'
    end subroutine listar
end module OPSUC