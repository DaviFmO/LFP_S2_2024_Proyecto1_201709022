module moduleError
    use Utils

    implicit none
    !definir el tipo de error
    type :: Error
        !integer :: id
        character(len=100) :: mensaje, tipo
        integer :: linea
        integer :: columna
    end type Error
contains
subroutine initError(mensaje,tipo,linea,columna,e)
    implicit none
    character(len=*), intent(in) :: mensaje
    character(len=*), intent(in) :: tipo
   ! integer, intent(in) :: tipo
    integer, intent(in) :: linea
    integer, intent(in) :: columna
    type(Error), intent(inout) :: e
    !limpiamos el error
    e%mensaje = ""
    !e%id = 0
    e%linea = 0
    e%columna = 0
    !asignamos los valores
    e%mensaje = trim(mensaje)
    e%linea = linea
    e%columna = columna
    e%tipo = trim(tipo)
  
end subroutine initError

subroutine addError(errors,mensaje,buffer_,tipo,linea,columna)
    implicit none
    type(Error), allocatable, intent(inout) :: errors(:) 
    character(len=*), intent(inout) :: buffer_
    character(len=*), intent(in) :: mensaje
    character(len=*), intent(in) :: tipo
    integer, intent(in) :: linea
    integer, intent(in) :: columna
    type(Error):: e
    integer :: i
    !inicializamos el token
    call initError(mensaje,tipo,linea,columna,e)
    !agregamos el token al arreglo
    if(allocated(errors))then
        i = size(errors)
        call extendArray(errors)
    else
        allocate(errors(1))
        i =0

    end if
    errors(i+1) = e
    call clearBuffer(buffer_)
    
end subroutine addError
    subroutine extendArray(errors)
        implicit none
        type(Error), allocatable, intent(inout) :: errors(:)
        type(Error), allocatable :: temp(:)
        integer :: i
        !extendemos el arreglo
        i = size(errors)
        allocate(temp(i+1))
        temp(1:i) = errors
        deallocate(errors)
        errors = temp
        
    end subroutine extendArray

end module moduleError