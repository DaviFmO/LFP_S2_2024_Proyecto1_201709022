module states
    use moduleToken
    use moduleError
    use Utils
    implicit none


    contains
    !stado 0
    subroutine state0(current_char,buffer_,tokens_,errors,linea,columna,estado,salto_linea)
    implicit none
    character(len=1), intent(in) :: current_char
    character(len=100), intent(inout) :: buffer_
    type(token), allocatable, intent(inout) :: tokens_(:)
    type(Error), allocatable, intent(inout) :: errors(:)
    integer, intent(inout) :: linea,columna
    integer, intent(inout) :: estado 
    logical, intent(in) :: salto_linea
    !añadir el caracter al buffer

    call addbuffer(current_char,buffer_,columna)
    !verificar si es un caracter del estado 0
    if(current_char == '=') then 
        call addToken(tokens_,buffer_,'IGUAL',linea,columna)
       
    else if(current_char == "(")then
        call addToken(tokens_,buffer_,"parentesis_abierto",linea,columna)

    else if(current_char == ")")then
        call addToken(tokens_,buffer_,"parentesis_cerrado",linea,columna)
        
    else if(current_char == "{")then
        call addToken(tokens_,buffer_,"llave_abierta",linea,columna)
    !else if(current_char == "\n")then
        !call addToken(tokens_,buffer_,"llave_abierta",linea,columna)
    else if(current_char == "}")then
        call addToken(tokens_,buffer_,"llave_cerrada",linea,columna)
    else if(current_char == ":")then
        call addToken(tokens_,buffer_,"dos_puntos",linea,columna)
    else if(current_char == "%")then
        call addToken(tokens_,buffer_,"porcentaje",linea,columna)
    else if(current_char == ";")then
        call addToken(tokens_,buffer_,"punto_y_coma",linea,columna)
    else if(isspecialChar(current_char))then
    print*, "caracter especial", current_char
       call clearBuffer(buffer_)
       call irastate(estado,0)
    !else if(current_char == "   ")then
        !    print*, "tabulador"
       ! call clearBuffer(buffer_)
       ! call irastate(estado,0)
    else if(current_char == "\t")then
        print*, "tabulador"
        call clearBuffer(buffer_)
        call irastate(estado,0)
    
    else if(current_char>='0' .and. current_char<='9')then
        !cambiar de estado si viene un numero
        call irastate(estado,1)
    else if(current_char>='a' .and. current_char<='z' .or.&
         current_char>='A' .and. current_char<='Z')then
        !cambiar de estado si viene una letra
            call irastate(estado,2)
    else if(current_char=='"')then
        !cambiar de estado si viene una comilla
        call irastate(estado,3)
    
        !salto de espacio en blanco
    else if(current_char == " ")then
        call irastate(estado,0)
     !salto de linea
    else if(current_char == "\n")then
      call newLine(linea,columna)
        call irastate(estado,0)

     !si no es ningun caracter valido
    else
        print*, "caracter no valido", current_char
        
        call addError(errors, "Caracter no reconocido: " // current_char, buffer_, "LEXICO", linea, columna)
        call irastate(estado,0)
    end if
    !si es salto de linea
    if(salto_linea)then
        call newLine(linea,columna)
        call clearBuffer(buffer_)
    end if
   
    end subroutine state0
    !estado 1
    subroutine state1(current_char,buffer_,tokens_,linea,columna,estado,i)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(token), allocatable, intent(inout) :: tokens_(:)
        integer, intent(inout) :: linea,columna
        integer, intent(inout) :: estado
        integer, intent(inout) :: i
        !añadir el caracter al buffer
        call addbuffer(current_char,buffer_,columna)
        !verificar si es un numero
        if(current_char >='0'.and. current_char<='9')then
            !seguir en el estado 1
            call irastate(estado,1)
        else
            ! solo retroceder si es un caracter imprimeble
            if(isspecialChar(current_char).eqv. .false.)then
               call goBack(i,columna,buffer_)
            end if
            call addToken(tokens_,buffer_,"numero",linea,columna)
            !cambiar de estado
            call irastate(estado,0)
        end if
        
    
    end subroutine state1
    !estado 2
    subroutine state2(current_char,buffer_,tokens_,linea,columna,estado,i)
        implicit none
        character(len=1), intent(in) :: current_char
        character(len=100), intent(inout) :: buffer_
        type(token), allocatable, intent(inout) :: tokens_(:)
        integer, intent(inout) :: linea,columna
        integer, intent(inout) :: estado
        integer, intent(inout) :: i
        character(len=100) :: Tokentype
        !initializing the token type
        Tokentype = ""
        !añadir el caracter al buffer
        call addbuffer(current_char,buffer_,columna)
        !verificar si es una letra
        if(current_char >='a'.and. current_char<='z' .or.&
            current_char >='A'.and. current_char<='Z')then
            !seguir en el estado 2
            call irastate(estado,2)
        else
            ! solo retroceder si es un caracter imprimeble
            if(isspecialChar(current_char).eqv. .false.)then
               call goBack(i,columna,buffer_)
            end if
            !verificar si es una palabra reservada
            if(esPalabraReservada(buffer_)=="Palabra reservada")then
               call addToken(tokens_,buffer_,"Palabra reservada",linea,columna)
            else
              call addToken(tokens_,buffer_,"Cadena",linea,columna)
              call irastate(estado,0)
            end if
            Tokentype = esPalabraReservada(buffer_)
            call addToken(tokens_,buffer_,Tokentype,linea,columna)
            !cambiar de estado
            call irastate(estado,0)
        end if
    end subroutine state2
    !estado 3
    subroutine state3(current_char,buffer_,tokens_,linea,columna,estado)
       implicit none
       character(len=1), intent(in) :: current_char
         character(len=100), intent(inout) :: buffer_
            type(token), allocatable, intent(inout) :: tokens_(:)
            integer, intent(inout) :: linea,columna
            integer, intent(inout) :: estado
            !añadir el caracter al buffer
            call addbuffer(current_char,buffer_,columna)
            !verificar si es una comilla
            if(current_char == '"')then
                !añadir el token
                call addToken(tokens_,buffer_,"cadena",linea,columna)
                !cambiar de estado
                call irastate(estado,0)
            else
                !seguir en el estado 3
                call irastate(estado,3)
            end if
    end subroutine state3

        

    
    end module states
