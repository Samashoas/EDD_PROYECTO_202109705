program Main
    use JSLoader
    use JRLoader
    use JTLoader
    use OPSUC

    implicit none
    integer :: choice, i, userSUC, Long, Code
    character(len=20) :: passCoded
    character(len=1) :: char
    real :: seed
    
    Logical :: loggedIn
    character(100) :: username, pass
    character(100) :: Cuser = 'admin'
    character(100) :: Cpass = 'admin'
    call InitMenu()

    contains
    !Completo
    subroutine InitMenu()
        print*, ''
        print*, '1. Login Admin'
        print*, '2. Login Sucursal'
        print*, '3. Exit'

        do 
            write(*, '(A, I0, A)', advance='no') 'Seleccione una opcion(INIT): '
            read*, choice
            print*, ' '

            select case(choice)
            case(1)
                call LoginAD()
            case(2)
                call LoginSUC()
            case(3)
                exit
            end select
        end do
    end subroutine

    !Completo
    subroutine LoginAD()
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
            print*, 'wrong user or password'
            call InitMenu()
        end if
    end subroutine 

    !Completo
    subroutine LoginSUC()
        loggedIn = .false.

        write(*, '(A)', advance='no') 'Enter user: '
        read*, userSUC

        write(*, '(A)', advance='no') 'Enter password: '
        read*, pass

        seed = 2
        Long = len_trim(pass)
        do i = 1, Long
            char = pass(i:i)
            Code = ichar(char) + nint(seed)
            passCoded = trim(passCoded) // achar(mod(Code, 127))
        end do

        print*, 'Encrypted pass: ', trim(passCoded)

        do i = 1, size(sucursales)
            if(userSUC == sucursales(i)%id .and. pass == sucursales(i)%password)then
                print*, 'Bienvenido sucursal #', sucursales(i)%id
                print*, ' '
                loggedIn = .true.
                call ModSuc()
            end if
        end do
        if(.not. loggedIn) then
            print*, 'wrong user or password'
            call InitMenu()
        end if
    end subroutine

    !Falta: Sucursales y reportes
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
                print*, '1. Login Admin'
                print*, '2. Login Sucursal'
                print*, '3. Exit'
                exit
            end select
        end do
    end subroutine

    !Falta: Cargar Rutas
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

    subroutine ModSuc()
        print*, '1. Cargar Tecnicos'
        print*, '2. Generar recorrido optimo'
        print*, '3. Buscar Tecnico'
        print*, '4. Listar Tecnicos'
        print*, '5. Reportes'
        print*, '6. exit'

        do 
            write(*, '(A, I0, A)', advance='no') 'Seleccione una opcion(MODSUC): '
            read*, choice
            print*, ' '

            select case(choice)
            case(1)
                call LoadJT()
            case(2)
                print*, 'Se llama a Generar recorrido optimo'
            case(3)
                call buscar()
            case(4)
                call listar()
            case(5)
                print*, 'Se llama a Reportes'
            case(6)
                print*, ''
                print*, '1. Login Admin'
                print*, '2. Login Sucursal'
                print*, '3. Exit'
                exit
            end select
        end do
    end subroutine
end program Main