module Utils

contains
!añadir un caracter al buffer
    subroutine addbuffer(current_char,buffer_,columna)
    implicit none
    character(len=1), intent(in) :: current_char
    character(len=100), intent(inout) :: buffer_
    integer, intent(inout) :: columna
    !añadir el caracter al buffer
    buffer_ = trim(buffer_)//current_char
    columna = columna + 1

    end subroutine addbuffer

!verificar si es un caracter especial
   function isspecialChar(current_char) result(special)
    implicit none
    character(len=1), intent(in) :: current_char
    logical :: special
    !verificar si es un caracter especial
    if(current_char == " " .or. current_char == "\t" .or. &
        current_char == "\r" .or. current_char== "\f" .or. &
        current_char == "\0" .or. current_char== "\s".or.current_char=="\n"&
        .or. current_char=="    ".or.current_char==char(9))then
        special = .true.
        print*, "caracter especialjolasod", current_char
        else
     special = .false.
    end if
    end function isspecialChar
!limpiar el buffer
    subroutine clearBuffer(buffer)
    implicit none
    character(len=100), intent(inout) :: buffer
    buffer = ""
    end subroutine clearBuffer

 !cambiar de estado
subroutine irastate(estado,nex_estado)
    implicit none
    integer, intent(inout) :: estado
    integer, intent(in) :: nex_estado
    !cambiar de estado
    estado = nex_estado
    end subroutine irastate
  !salto de linea
subroutine newLine(linea,columna)
    implicit none
    integer, intent(inout) :: linea,columna
    !salto de linea
    linea = linea + 1
    columna = 1
end subroutine newLine
function esPalabraReservada(buffer) result(reservada)
    implicit none
    character(len=*), intent(in) :: buffer
    character(len=100) :: reservada
    if (buffer=='Grafica'.or. buffer=='grafica'.or.buffer=='Nombre'.or.buffer=='nombre'.or.&
    buffer=='Continente'.or. buffer=='continente'.or. buffer=='pais'.or.buffer=='Pais'.or.&
    buffer=='poblacion'.or. buffer=='Poblacion'.or.buffer=='Saturacion'.or.buffer=='Bandera') then
    reservada = 'reservada'
    else
        reservada='identificador'

    end if
    end function esPalabraReservada

    subroutine goBack(i, columna, buffer_)
        implicit none
        integer, intent(inout) :: i
        integer, intent(inout) :: columna
        character(len=100), intent(inout) :: buffer_
        integer :: actual_length

        i = i - 1
        columna = columna - 1

        ! Obtén la longitud real del contenido de buffer_ sin los espacios en blanco finales
        actual_length = len_trim(buffer_)
        if (actual_length > 0) then
            buffer_ = buffer_(:actual_length-1)
        endif
    end subroutine goBack

   


end module Utils