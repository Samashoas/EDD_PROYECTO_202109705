module ventana_mod
    implicit none

    contains

    subroutine numWin()
        integer :: num_ventanas
        do
            write(*, '(A, I0, A)', advance='no') 'Ingrese el número de ventanas: '
            read(*, *) num_ventanas

            if (num_ventanas > 0) then
                exit
            else
                write(*, *) 'El número de ventanas debe ser mayor a 0'
            end if
        end do
        write(*, '(A, I0, A)', advance='no') 'Se ingresaron: ', num_ventanas, ' ventanas'
        print*, ' '

        print*, (' ')
        print*, '1. Cargar clientes'
        print*, '2. Ingresar ventanillas'
        print*, '3. regresar'
    end subroutine numWin

end module ventana_mod