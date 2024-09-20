program main
    use moduleToken
    use moduleError
    use states

    implicit none
    
    integer :: i, linea, columna
    character(len=100) :: line
    integer :: ios 
    integer :: estados
    character(len=100) :: buffer_
    type(token), allocatable :: tokens_(:)
    type(Error), allocatable :: errors_(:)
    integer :: line_length
    integer :: t
    integer :: contador
    character(len=300) :: html


    !inicializamos los arreglos
allocate(tokens_(0))
allocate(errors_(0))


    !inicializamos el estado
    estados = 0
    buffer_ = "" 
    ios = 0
    line_length = 0

    !abrir el archivo
    open(unit=10, file="texto.txt", status="old", action="read", iostat=ios)
    !leer el archivo
    do
        i=1
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) then
            exit
        end if
        line_length = len_trim(line)
        !recorremos la linea
        do while(i<=line_length)
            !print *, "linea: ", line(i:i), " estado: ", estados
            if (estados==0)then
                !estado 0
                call state0(line(i:i), buffer_,tokens_,errors_,linea,columna,estados,line_length==i)

            else if (estados==1)then
                !estado 1
                call state1(line(i:i), buffer_,tokens_,linea,columna,estados,i)
            else if (estados==2)then
                call state2(line(i:i), buffer_,tokens_,linea,columna,estados,i)
            else if (estados==3)then
                call state3(line(i:i), buffer_,tokens_,linea,columna,estados)
            
            end if
        columna= columna + 1
          i= i+1
          end do
          linea = linea + 1  
        columna = 1

    end do
    close (10)
    !imprimir los tokens
    print *, "Tokens generados"
    do i=1, size(tokens_)
        print *, "Token: ", tokens_(i)%lexema, " Tipo: ", tokens_(i)%tipo, " Linea: ", tokens_(i)%linea, " Columna: ", tokens_(i)%columna
    end do
    !imprimir los errores
    print *, "Errores generados"
    do i=1, size(errors_)
        print *, "Error:", errors_(i)%mensaje, "Tipo:", errors_(i)%tipo, "Linea:", errors_(i)%linea, "Columna:", errors_(i)%columna
    end do
    
    !abrir el archivo HTML para escribir
    open(unit=10, file="tabla.html", status="replace", action="write", iostat=ios)
    ! Verificar si hubo error al abrir el archivo
    if (ios /= 0) then
        print *, "Error al abrir el archivo HTML."
        stop
    end if
    ! Comenzar el documento HTML
    html = "<html>" // &
           "<head><title>Tabla de Datos</title></head>" // &
           "<body>" // &
           "<h1>Mi Tabla</h1>" // &
           "<table border=""1"">" // &
           !"<tr><th>No</th><th>leema</th><th>tipo</th><th>fila</th><th>calumna</th></tr>"
           "<tr><th>No</th><th>leema</th><th>tipo</th><th>fila</th><th>calumna</th></tr>"
    write(10, '(A)') trim(html)
    ! Agregar las filas a la tabla
    
    do t = 1, size(tokens_)
        contador = t
        html = "<tr><td>" // trim(adjustl(int_to_string(contador))) // "</td>" //&
        " <td>" // trim(tokens_(t)%lexema) // "</td>" // &
               "<td>" // trim(tokens_(t)%tipo) // "</td>" // &
               "<td>" // trim(adjustl(int_to_string(tokens_(t)%linea))) // "</td>" // &
               "<td>" // trim(adjustl(int_to_string(tokens_(t)%columna))) // "</td></tr>"
        write(10, '(A)') trim(html)
    end do
    ! Cerrar la tabla y el documento HTML
    html = "</table></body></html>"
    write(10, '(A)') trim(html)
    ! Cerrar el archivo
    close(10)
    print *, "Archivo HTML creado correctamente."

    !html para errores
    !abrir el archivo HTML para escribir
    open(unit=10, file="Errores.html", status="replace", action="write", iostat=ios)
    ! Verificar si hubo error al abrir el archivo
    if (ios /= 0) then
        print *, "Error al abrir el archivo HTML."
        stop
    end if
    ! Comenzar el documento HTML
    html = "<html>" // &
           "<head><title>Tabla de Datos</title></head>" // &
           "<body>" // &
           "<h1>Mi Tabla</h1>" // &
           "<table border=""1"">" // &
           !"<tr><th>No</th><th>leema</th><th>tipo</th><th>fila</th><th>calumna</th></tr>"
           "<tr><th>No</th><th>leema</th><th>tipo</th><th>fila</th><th>calumna</th></tr>"
    write(10, '(A)') trim(html)
    ! Agregar las filas a la tabla
    
    do t = 1, size(errors_)
        contador = t
        html = "<tr><td>" // trim(adjustl(int_to_string(contador))) // "</td>" //&
        " <td>" // trim(errors_(t)%mensaje) // "</td>" // &
               "<td>" // trim(errors_(t)%tipo) // "</td>" // &
               "<td>" // trim(adjustl(int_to_string(errors_(t)%linea))) // "</td>" // &
               "<td>" // trim(adjustl(int_to_string(errors_(t)%columna))) // "</td></tr>"
        write(10, '(A)') trim(html)
    end do
    ! Cerrar la tabla y el documento HTML
    html = "</table></body></html>"
    write(10, '(A)') trim(html)
    ! Cerrar el archivo
    close(10)
    print *, "Archivo HTML errores creado correctamente."


contains

    function int_to_string(i) result(str)
        integer, intent(in) :: i
        character(len=100) :: str
        write(str, '(I0)') i
        str = adjustl(str)
    end function int_to_string

end program main
