program Main
    use JSLoader
    use JRLoader
    use JTLoader

    implicit none
    integer :: choice
    
    Logical :: loggedIn
    character(100) :: username, pass
    character(100) :: Cuser = 'admin'
    character(100) :: Cpass = 'admin'
    call cargasAD()

    contains
    subroutine InitMenu()
        print*, ''
        print*, '1. Login'
        print*, '2. Exit'

        do 
            write(*, '(A, I0, A)', advance='no') 'Seleccione una opcion(INIT): '
            read*, choice
            print*, ' '

            select case(choice)
            case(1)
                call Login()
            case(2)
                exit
            end select
        end do
    end subroutine

    subroutine Login()
        loggedIn = .false.

        write(*, '(A)', advance='no') 'Enter user: '
        read*, username

        write(*, '(A)', advance='no') 'Enter password: '
        read*, pass

        if (username == Cuser .and. pass == Cpass) then
            loggedIn = .true.
            print*, ' '
            print*, 'Bienvenido ', username
            call ModAdmin()
        else
            print*, 'Wrong User or password'
            print*, ''
            print*, '1. Login'
            print*, '2. Extit'
        end if
    end subroutine 

    subroutine ModAdmin()
        print*, ' ' 
        print*, '1. Cargar archivos'
        print*, '2. Sucursales'
        print*, '3. Reportes'
        print*, '4. Salir'

        do 
            write(*, '(A, I0, A)', advance='no') 'Seleccione una opcion(MODADMIN): '
            read*, choice
            print*, ' '

            select case(choice)
            case(1)
                call cargasAD()
            case(2)
                print*, 'Se llama a Sucursales'
            case(3)
                print*, 'Se llama a Reportes'
            case(4)
                print*, ''
                print*, '1. Login'
                print*, '2. Extit'
                exit
            end select
        end do
    end subroutine

    subroutine cargasAD()
        print*, ' '
        print*, '1. Cargar Sucursales'
        print*, '2. Cargar Rutas'
        print*, '3. Salir'

        do 
            write(*, '(A, I0, A)', advance='no') 'Seleccione una opcion(CARGASAD): '
            read*, choice
            print*, ' '

            select case(choice)
            case(1)
                call LoadJS()
            case(2)
                print*, 'Se llama a Cargar Rutas'
            case(3)
                print*, ' ' 
                print*, '1. Cargar archivos'
                print*, '2. Sucursales'
                print*, '3. Reportes'
                print*, '4. Salir'
                exit
            end select
        end do
    end subroutine

end program Main