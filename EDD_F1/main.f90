program main
    use json_loader
    use cola_module
    use ventana_mod
    implicit none
    integer :: choice
    call MainMenu()

contains
    subroutine MainMenu()
        print*, ' '
        print*, '1. Parametros inciales'
        print*, '2. Ejecutar paso'
        print*, '3. Estado de memoria de la estructura'
        print*, '4. Reportes'
        print*, '5. Acerca de: '
        print*, '6. Registro de operaciones'
        print*, '7. Salir'

        do
            write (*, '(A, I0, A)', advance='no') 'Seleccione una opcion: '
            read*, choice
            print*, ' '

            select case (choice)
                case(1)
                    call menuParam()
                    exit
                case(2)
                    call MenuPasos()
                case(3)
                    call MenuMemoria()
                case(4)
                    call MenuReportes()
                case(5)
                    print*, '---Datos del estudiante---'
                    print*, ' Juan Pablo Samayoa Ruiz '
                    print*, '       202109705         '
                    print*, '--------------------------'
                case(6)
                    call MenuMemoria()
                case(7)
                    print*, 'ADIOS!'
                    exit
                case default
                    print*, 'opcion invalida, ingrese una opcion valida'
            end select
        end do
    end subroutine MainMenu

    subroutine menuParam()
        print*, '1. Cargar clientes'
        print*, '2. Ingresar ventanillas'
        print*, '3. regresar'

        do
            write (*, '(A, I0, A)', advance='no') 'Seleccione una opcion: '
            read*, choice
            print*, ' '

            select case(choice)
                case(1)
                    call LoadJson()
                case(2)
                    call numWin()
                case(3)
                    call MainMenu()
                    exit
                case default
                    print *, 'seleccione una opcion valida'
            end select
        end do
    end subroutine menuParam

    subroutine MenuPasos
        print*, 'Trabajando en ello'
        call MainMenu()
    end subroutine MenuPasos

    subroutine MenuMemoria
        print*, 'Trabajando en ello'
        call MainMenu()
    end subroutine MenuMemoria

    subroutine MenuReportes
        print*, 'Trabajando en ello'
        call MainMenu()
    end subroutine MenuReportes

end program main