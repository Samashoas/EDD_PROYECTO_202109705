module json_loader
    use json_module
    use cola_module
    implicit none
    
    type(cola) :: my_cola
contains

    subroutine LoadJson()
        character(len=1000) :: filename
        type(json_file) :: json   
        type(json_value), pointer :: listPointer, personPointer, attributePointer  
        type(json_core) :: jsonc  
        character(len=:), allocatable :: id, nombre, img_p, img_g  

        integer :: i, size 
        logical :: found

        write (*, '(A, I0, A)', advance='no') 'Ingrese la direccion del archivo JSON: '
        read(*, '(A)') filename
        call json%initialize()    
        call json%load(filename=trim(filename))  
        call json%print()
        print *, (' ')         
        
        call json%info('',n_children=size)

        call json%get_core(jsonc)               
        call json%get('', listPointer, found)

        print*, ('Clientes: ')
        do i = 1, size                          
            call jsonc%get_child(listPointer, i, personPointer, found = found)  

            call jsonc%get_child(personPointer, 'id', attributePointer, found = found)  
            if (found) then                    
                call jsonc%get(attributePointer, id)        
            end if

            call jsonc%get_child(personPointer, 'nombre', attributePointer, found = found)  
            if (found) then                    
                call jsonc%get(attributePointer, nombre)        
            end if

            call jsonc%get_child(personPointer, 'img_g', attributePointer, found = found)
            if(found) then
                call jsonc%get(attributePointer, img_g)
            end if

            call jsonc%get_child(personPointer, 'img_p', attributePointer, found = found)
            if(found) then
                call jsonc%get(attributePointer, img_p)
            end if

            if (found) then
                print *, 'id: ', trim(id)
                print *, 'nombre: ', trim(nombre)
                print *, 'Imagenes grandes: ', trim(img_g)
                print *, 'imagenes pequenas: ', trim(img_p)
                call my_cola%append(trim(id), trim(nombre), trim(img_g), trim(img_p))
                print *, ' '
            end if
        end do

        print*, (' ')
        print*, '1. Cargar clientes'
        print*, '2. Ingresar ventanillas'
        print*, '3. regresar' 

    end subroutine
end module json_loader