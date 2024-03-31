module Logs
    implicit none
    character(100) :: username, password
    character(100) :: Cuser = 'Admin'
    character(100) :: Cpass = 'Admin'

contains

    subroutine Login()
        logical :: loggedIn

        loggedIn = .false.

        do while (.not. loggedIn)
            write(*, '(A, I0, A)', advance='no') 'Enter User: '
            read *, username

            write(*, '(A, I0, A)', advance='no') 'Enter Password: '
            read *, password

            if (username == Cuser .and. password == Cpass) then
                print *, "Welcome, ", username, "!"
                loggedIn = .true.
            else
                print *, "Wrong password, try again"
            end if
        end do
    end subroutine Login
end module Logs