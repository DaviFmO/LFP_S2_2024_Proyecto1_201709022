program main
    use moduleToken
    use moduleError
    use states
    use graficaarbol
    use archivo_mod

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
    character(len=100) :: abrir_archivo
    character(len=100) :: salida_archivo
    character(len=20):: nombre_imagen1
    character(len=20):: nombre_imagen
      !inicializamos los arreglos
allocate(tokens_(0))
allocate(errors_(0))


    !inicializamos el estado
    estados = 0
    buffer_ = "" 
    ios = 0
    line_length = 0
    abrir_archivo = "Texto.txt" 
    salida_archivo = "archivo_salida.txt"
    nombre_imagen = "graficaultimo"
    nombre_imagen1 = "graficaultimo1"
    !abrir_archivo2 = "salidasss.txt"
    !abrir_lectura ="salidasss2.txt"
    
    
call procesar_archivo(abrir_archivo, salida_archivo)
    !abrir el archivo
    open(unit=10, file=abrir_archivo, status="old", action="read", iostat=ios)
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
   
    !print *, "Tokens generados"
   do i=1, size(tokens_)
       !print *, "Token: ", tokens_(i)%lexema, " Tipo: ", tokens_(i)%tipo, " Linea: ", ((tokens_(i)%linea)/2), " Columna: ", ((tokens_(i)%columna)/2)
    end do
    !imprimir los errores

   !print *, "Errores generados"
    !do i=1, size(errors_)
     !  print *, "Error:", errors_(i)%mensaje, "Tipo:", errors_(i)%tipo, "Linea:", errors_(i)%linea, "Columna:", errors_(i)%columna
   ! end do
    !call lectura(abrir_lectura)
    !abrir el archivo HTML para escribir
    if(size(errors_)>0 .or. size(tokens_)>0 )then
        if(size(tokens_)>0)then
    open(unit=10, file="Tokens.html", status="replace", action="write", iostat=ios)
    ! Verificar si hubo error al abrir el archivo
    if (ios /= 0) then
        print *, "Error al abrir el archivo HTML."
        stop
    end if
    ! Comenzar el documento HTML
    html = "<html>" // &
           "<head><title>Tabla de Datos</title></head>" // &
          ! "<body>" // &
           "<h1>TOKENS</h1>" // &
           !"<table border=""1"">" // &
           !"<tr><th>No</th><th>leema</th><th>tipo</th><th>fila</th><th>calumna</th></tr>"
           !color de la tabla
           "<table border=""1"" cellpadding=""5"" cellspacing=""0"" style=""border-collapse: collapse; width: 100%;"">" // &
           "<tr style=""background-color: green; color: white;"">" // &
           "<th>No</th><th>lexema</th><th>tipo</th><th>fila</th><th>calumna</th></tr>"
           
    write(10, '(A)') trim(html)
    ! Agregar las filas a la tabla
    
    do t = 1, size(tokens_)
        contador = t
        html = "<tr style=""background-color: #90EE90;""><td>" // trim(adjustl(int_to_string(contador))) // "</td>" //&
        " <td>" // trim(tokens_(t)%lexema) // "</td>" // &
               "<td>" // trim(tokens_(t)%tipo) // "</td>" // &
               "<td>" // trim(adjustl(int_to_string((tokens_(t)%linea)/2))) // "</td>" // &
               "<td>" // trim(adjustl(int_to_string((tokens_(t)%columna)/2))) // "</td></tr>"
        write(10, '(A)') trim(html)
    end do
    ! Cerrar la tabla y el documento HTML
    html = "</table></body></html>"
    write(10, '(A)') trim(html)
    ! Cerrar el archivo
    close(10)
    !print *, "Archivo HTML creado correctamente."
    end if
    
    !crear html de errores si existen
    if(size(errors_)>0)then
        if(size(errors_)>0)then
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
           !"<body>" // &
           "<h1>Errores Lexicos</h1>" // &
           !"<table border=""1"">" // &
           "<table border=""1"" cellpadding=""5"" cellspacing=""0"" style=""border-collapse: collapse; width: 100%;"">" // &
           "<tr style=""background-color: orange; color: white;"">" // &
           "<th>No</th><th>lexema</th><th>tipo</th><th>fila</th><th>calumna</th></tr>"
    write(10, '(A)') trim(html)
    ! Agregar las filas a la tabla
    
    do t = 1, size(errors_)
        contador = t
        html =  "<tr style=""background-color: #FFA07A;""><td>"// trim(adjustl(int_to_string(contador))) // "</td>" //&
        " <td>" // trim(errors_(t)%mensaje) // "</td>" // &
               "<td>" // trim(errors_(t)%tipo) // "</td>" // &
               "<td>" // trim(adjustl(int_to_string((errors_(t)%linea)/2))) // "</td>" // &
               "<td>" // trim(adjustl(int_to_string((errors_(t)%columna)/2))) // "</td></tr>"
        write(10, '(A)') trim(html)
    end do
    ! Cerrar la tabla y el documento HTML
    html = "</table></body></html>"
    write(10, '(A)') trim(html)
    ! Cerrar el archivo
    close(10)
    open(unit=30, file="error.txt", status="replace", action="write", iostat=ios)
    ! Verificar si hubo error al abrir el archivo
    if (ios /= 0) then
        print *, "Error al abrir el archivo HTML."
        stop
    end if
    print *, "Errores"
    write(30, '(A)') "C:\Users\PC\Desktop\Proyecto 1 Final\error.png"  
    close(30)
    !call graficaarbo(abrir_archivo)
    !abrir_lectura = "salidasss.txt"
    !call lectura(abrir_lectura)
    !nombre_imagen = "graficaultima1.dot"
    !call graficaarbo(abrir_archivo, nombre_imagen1)
    end if
    end if
    endif
    if(size(errors_)==0)then
        call procesar_archivo(abrir_archivo, salida_archivo)
        end if

contains

    function int_to_string(t) result(str)
        integer, intent(in) :: t
        character(len=100) :: str
        write(str, '(I0)') t
        str = adjustl(str)
    end function int_to_string

end program main
