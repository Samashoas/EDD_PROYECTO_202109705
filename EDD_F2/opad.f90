module OPAD
    use JCloader
    implicit none
    type(client) :: new_client
    character (len=:), allocatable :: dpi, nombre, password

    contains

    subroutine registrar()
        write(*, '(A, I0, A)', advance='no') 'Enter DPI: '
        read*, dpi

        write(*, '(A, I0, A)', advance='no') 'Enter name: '
        read*, nombre

        write(*, '(A, I0, A)', advance='no') 'Enter password: '
        read*, password

        new_client%dpi = dpi
        new_client%nombre = nombre
        new_client%password = password

        if(allocated(clients)) then
            clients = [clients, new_client]
        else
            clients = [new_client]
        end if

        print*, 'cliente registrado exitosamente'
    end subroutine registrar
end module OPAD