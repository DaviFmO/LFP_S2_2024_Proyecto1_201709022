module moduleToken
    use Utils
    implicit none
    !definir el tipo de token
    type :: token
        !integer :: id
        character(len=100) :: lexema, tipo
        integer :: linea
        integer :: columna
    end type token
contains
subroutine initToken(buffer_,tipo,linea,columna,token_)
    implicit none
    character(len=*), intent(in) :: buffer_
    character(len=*), intent(in) :: tipo
   ! integer, intent(in) :: tipo
    integer, intent(in) :: linea
    integer, intent(in) :: columna
    type(token), intent(out) :: token_
    !limpiamos el token
    token_%lexema = ""
    !token_%id = 0
    token_%linea = 0
    token_%columna = 0
    !asignamos los valores
    token_%lexema = trim(buffer_)
    token_%linea = (linea)
    token_%columna = (columna)
    token_%tipo = trim(tipo)
  
end subroutine initToken

subroutine addToken(tokens_,buffer_,tipo,linea,columna)
    implicit none
    type(token), allocatable, intent(inout) :: tokens_(:) 
    character(len=100), intent(inout) :: buffer_
    character(len=*), intent(in) :: tipo
    integer, intent(in) :: linea,columna
    type(token):: token_
    integer :: i
    !inicializamos el token
    call initToken(buffer_,tipo,linea,columna,token_)
    !agregamos el token al arreglo
    if(allocated(tokens_))then
        i = size(tokens_)
        call extendArrays(tokens_)
    else
        allocate(tokens_(1))
        i =0

    end if
    tokens_(i+1) = token_
    call clearBuffer(buffer_)
end subroutine addToken
    subroutine extendArrays(tokens_)
        implicit none
        type(token), allocatable, intent(inout) :: tokens_(:)
        type(token), allocatable :: temp(:)
        integer :: i
        !extendemos el arreglo
        i = size(tokens_)
        allocate(temp(i+1))
        temp(1:i) = tokens_
        deallocate(tokens_)
        tokens_ = temp

    end subroutine extendArrays
end module moduleToken