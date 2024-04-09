module OPAD
    use JCloader
    implicit none

    contains

    subroutine registrar()
        type(client) :: new_client
        character(len=100) dpi, nombre, password

        write(*, '(A)', advance='no') 'Ingrese DPI: '
        read(*, '(A)') dpi

        write(*, '(A)', advance='no') 'Ingrese name: '
        read(*, '(A)') nombre

        write(*, '(A)', advance='no') 'Ingrese password: '
        read(*, '(A)') password

        new_client%dpi = dpi
        new_client%nombre = nombre
        new_client%password = password

        if (allocated(clients)) then
            clients = [clients, new_client]
        else
            clients = [new_client]
        end if

        print*, ' '
        print*, 'Cliente registrado exitosamente'
        print*, 'DPI: ', trim(new_client%dpi)
        print*, 'Nombre: ', trim(new_client%nombre)
        print*, 'Password: ', trim(new_client%password)

        print*, ' '
        print*, '1. Clientes registrados'
        print*, '2. Registrar'
        print*, '3. modificar'
        print*, '4. eliminar'
        print*, '5. Regresar'
    end subroutine registrar

    subroutine modificar()
        character(len=100) :: dpi, nombre, password
        integer :: i, index
        logical :: found

        write(*, '(A)', advance='no') 'Ingrese DPI: '
        read(*,'(A)') dpi

        found = .false.

        do i = 1, size(clients)
            if(trim(clients(i)%dpi)==trim(dpi))then
                found = .true.
                index = i
                exit
            end if
        end do

        if (.not. found)then
            print*, 'Cliente no encontrado'
            return
        end if

        write(*, '(A)', advance='no') 'Enter new client name: '
        read(*,'(A)') nombre

        write(*, '(A)', advance='no') 'Enter new client password: '
        read(*,'(A)') password

        clients(index)%nombre = trim(nombre)
        clients(index)%password = trim(password)

        print*, ' '
        print*, 'Client updated'
        print *, 'DPI: ', trim(clients(index)%dpi)
        print *, 'Nombre: ', trim(clients(index)%nombre)
        print *, 'Password: ', trim(clients(index)%password)

        print*, ' '
        print*, '1. Clientes registrados'
        print*, '2. Registrar'
        print*, '3. modificar'
        print*, '4. eliminar'
        print*, '5. Regresar'
    end subroutine modificar

    subroutine eliminar()
        character(len=100) :: dpi
        integer :: i, index
        logical :: found
        type(client), dimension(:), allocatable :: temp_client

        write(*, '(A)', advance='no') 'Enter Client DPI: '
        read(*,'(A)') dpi

        found = .false.
        do i =i, size(clients)
            if(trim(clients(i)%dpi) == trim(dpi))then
                found = .true.
                index = i
                exit
            end if
        end do

        if (.not. found)then
            print *, 'Cliente no encontrado'
            return
        end if

        allocate(temp_client(size(clients)-1))
        if(index > 1)then
            temp_client(1:index-1)=clients(1:index-1)
        end if

        if(index < size(clients))then
            temp_client(index:size(temp_client)) = clients(index+1:size(clients))
        end if

        if(allocated(clients))then
            deallocate(clients)
        end if
        clients = temp_client

        print*, 'Cliente eliminado'

        print*, ' '
        print*, '1. Clientes registrados'
        print*, '2. Registrar'
        print*, '3. modificar'
        print*, '4. eliminar'
        print*, '5. Regresar'
    end subroutine eliminar

    subroutine buscar()
        character(len=100) :: dpi
        integer :: i, index
        logical :: found

        write(*, '(A)', advance='no') 'Ingrese DPI: '
        read(*,'(A)') dpi

        found = .false.

        do i=1, size(clients)
            if(trim(clients(i)%dpi)==trim(dpi))then
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
        print *, 'DPI: ', trim(clients(index)%dpi)
        print *, 'Nombre: ', trim(clients(index)%nombre)
        print *, 'Password: ', trim(clients(index)%password)
    end subroutine buscar

    subroutine print_clients
        integer :: i

        if(.not. allocated(clients))then
            print*, 'No clients registered'
            return
        end if

        print *, 'Registered clients'
        do i = 1, size(clients)
            print *, '',i, '. ', 'DPI: ', trim(clients(i)%dpi)
            print *, 'Nombre: ', trim(clients(i)%nombre)
            print *, 'Password: ', trim(clients(i)%password)
            print *, ' '
        end do 

        print*, ' '
        print*, '1. Clientes registrados'
        print*, '2. Registrar'
        print*, '3. modificar'
        print*, '4. eliminar'
        print*, '5. Regresar'
    end subroutine print_clients
end module OPAD