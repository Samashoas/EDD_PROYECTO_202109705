program  Main 
    use JCloader
    implicit none
    integer :: choice
    logical :: loggedIn
    character(100) :: username, pass
    character(100) :: Cuser = 'Admin'
    character(100) :: Cpass = 'Admin'
    integer :: i
    call InitMenu()

    
contains
    subroutine InitMenu()
        print*, ''
        print*, '1. Login'
        print*, '2. Salir'

        do
            write(*, '(A, I0, A)', advance='no') 'Seleccione una opcion: '
            read*, choice
            print*, ' '

            select case (choice)
                case(1)
                    call Login()
                    exit
                case(2)
                    exit
            end select
        end do
    end subroutine InitMenu

    subroutine Login()
        loggedIn = .false.

        do while (.not. loggedIn)
            write(*, '(A, I0, A)', advance='no') 'Enter User: '
            read *, username

            write(*, '(A, I0, A)', advance='no') 'Enter Password: '
            read *, pass

            if (username == Cuser .and. pass == Cpass) then
                print *, "Welcome, ", username, "!"
                loggedIn = .true.
                call ModAdmin()
            else
                do i = 1,  size(clients)
                    if(username == trim(clients(i)%dpi) .and. pass == trim(clients(i)%password))then
                        print *, "Welcome, Client ", trim(clients(i)%nombre)
                        loggedIn = .true.
                        call ModCliente()
                        exit
                    end if
                end do
                if(.not. loggedIn) then
                    print*, "Wrong username or password, try again"
                end if
            end if
        end do
    end subroutine Login

    subroutine ModAdmin()
        print*, ' '
        print*, '1. Cargar usuarios'
        print*, '2. Operaciones'
        print*, '3. Arbol de usuarios'
        print*, '4. Logout'

        do
            write(*, '(A, I0, A)', advance='no') 'Seleccione una opcion: '
            read*, choice
            print*, ' '

            select case(choice)
                case(1)
                    call LoadJsonC()
                case(2)
                    call operacionesAD()
                case(3)
                    print*, 'Trabajando en el arbol'
                case(4)
                    call InitMenu()
                    exit
            end select
        end do
    end subroutine ModAdmin

    subroutine operacionesAD()
        print*, ' '
        print*, 'Operaciones de clientes'
        print*, '1. Registrar'
        print*, '2. modificar'
        print*, '3. eliminar'
        print*, '4. Regresar'

        do
            write(*, '(A, I0, A)', advance='no') 'Seleccione una opcion: '
            read*, choice
            print*, ' '

            select case(choice)
                case(1)
                    print*, 'Trabajando en el registro'
                case(2)
                    print*, 'Trabajando en la modificacion'
                case(3)
                    print*, 'Trabajando en la eliminacion'
                case(4)
                    call ModAdmin
                    exit
            end select
        end do
    end subroutine operacionesAD

    subroutine ModCliente
        print*, ' '
        print*, '1. Carga masiva'
        print*, '2. Imagenes'
        print*, '3. Reportes'
        print*, '4. Salir'

        do
            write(*, '(A, I0, A)', advance='no') 'Seleccione una opcion: '
            read*, choice
            print*, ''

            select case(choice)
                case(1)
                    call Cargas4C
                case(2)
                    print*, 'Trabajando en las imagenes'
                case(3)
                    print*, 'Trabajando en los reportes'
                case(4)
                    call InitMenu()
                    exit
            end select
        end do
    end subroutine ModCliente

    subroutine Cargas4C()
        print*, ' '
        print*, '1. Capas: '
        print*, '2. Imagenes: '
        print*, '3. Albumes: '
        print*, '4. Regresar'

        do
            write(*, '(A, I0, A)', advance='no') 'Seleccione una opcion: '
            read*, choice
            print*, ' '

            select case(choice) 
                case(1)
                    print*, 'Trabajando en la carga de capas'
                case(2)
                    print*, 'Trabajando en la carga de imagenes'
                case(3)
                    print*, 'Trabajando en la carga de Albumes'
                case(4)
                    call ModCliente
            end select
        end do
    end subroutine Cargas4C
end program  Main 