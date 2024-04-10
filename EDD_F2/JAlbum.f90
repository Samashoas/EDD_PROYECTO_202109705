module AlbumLoader
    use json_module
    implicit none

    type album
        character(len=:), allocatable :: nombre_album
        integer, dimension(:), allocatable :: imgs
    end type album
    
    type(album), dimension(:), allocatable :: albums
contains
    subroutine LoadJsonA()
        character(len=100) :: filename
        type(json_file) :: json
        type(json_value), pointer :: listPointer, albumPointer, attributePointer
        type(json_core) :: jsoncore
        character(len=:), allocatable :: nombre_album
        integer, dimension(:), allocatable :: imgs

        integer :: i, j, size, img_size
        logical :: found

        write (*, '(A, I0, A)', advance='no') 'Ingrese la direccion del archivo JSON: '
        read(*, '(A)') filename
        call json%initialize()
        call json%load(filename=trim(filename))
        call json%print()
        print*, (' ')

        call json%info('',n_children=size)
        call json%get_core(jsoncore)
        call json%get('', listPointer, found)

        allocate(albums(size))

        print*, ('Albums: ')
        do i = 1, size
            call jsoncore%get_child(listPointer, i, albumPointer, found = found)

            call jsoncore%get_child(albumPointer, 'nombre_album', attributePointer, found = found)
            if (found) then
                call jsoncore%get(attributePointer, nombre_album)
                albums(i)%nombre_album = nombre_album
            end if

            call jsoncore%get_child(albumPointer, 'imgs', attributePointer, found = found)
            if (found) then
                call jsoncore%get(attributePointer, imgs)
                albums(i)%imgs = imgs
            end if

            if(found) then
                print*, 'nombre_album: ', trim(albums(i)%nombre_album)
                print*, 'imgs: ', albums(i)%imgs
                print*, ' '
            end if
        end do

        !Utilizar el menu de ModAdmin
        print*, ' '
        print*, '1. Capas: '
        print*, '2. Imagenes: '
        print*, '3. Albumes: '
        print*, '4. Regresar'
    end subroutine
end module AlbumLoader
