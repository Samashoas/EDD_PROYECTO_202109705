module OPAD
    use JCloader
    implicit none

    contains

    subroutine registrar()
        type(client) :: new_client
        character(len=100) dpi, nombre, password

        write(*, '(A)', advance='no') 'Enter DPI: '
        read(*, '(A)') dpi

        write(*, '(A)', advance='no') 'Enter name: '
        read(*, '(A)') nombre

        write(*, '(A)', advance='no') 'Enter password: '
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
        print*, 'Operaciones de clientes'
        print*, '1. Registrar'
        print*, '2. modificar'
        print*, '3. eliminar'
        print*, '4. Regresar'
    end subroutine registrar

end module OPAD