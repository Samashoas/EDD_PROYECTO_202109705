module cola_module
    implicit none
    private

    type, public :: node
        private
        character(100) :: id
        character(100) :: nombre
        character(100) :: img_g
        character(100) :: img_p
        type(node), pointer :: next     
    end type node

    type, public :: cola
        private
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: delete
        procedure :: print
    end type cola

contains
    subroutine append(this, id, nombre, img_g, img_p)
        class(cola), intent(inout) :: this
        character(len =*), intent(in) :: id, nombre, img_g, img_p

        type(node), pointer :: temp
        allocate(temp)
        temp%id = id
        temp%nombre = nombre
        temp%img_g = img_g
        temp%img_p = img_p
        temp%next => null()

        if (.not. associated(this%head)) then
            this%head => temp
            this%tail => temp
        else
            this%tail%next => temp
            this%tail => temp
        end if

        print *, 'Append ', nombre
    end subroutine append

    subroutine delete(this)
        class(cola), intent(inout) :: this
        type(node), pointer :: temp

        if (.not. associated(this%head)) then
            print *, 'Cola esta vacia'
            return
        end if

        print *, 'Delete ', this%head%nombre
        temp => this%head
        this%head => this%head%next
        deallocate(temp)
    end subroutine delete

    subroutine print(this)
        class(cola), intent(in) :: this
        type(node), pointer :: current

        current => this%head

        print *, '//-----------------//'
        print *, 'La cola es:'
        print *, '//-----------------//'

        do while (associated(current))
            print*, current%id
            print *, current%nombre
            print *, current%img_g
            print *, current%img_p
            current => current%next
        end do 
    end subroutine print
end module cola_module