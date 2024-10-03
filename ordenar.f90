module archivo_mod
    use graficaarbol
    implicit none
contains

    subroutine procesar_archivo(archivo_entrada, archivo_salida)
        character(len=*), intent(in) :: archivo_entrada, archivo_salida
        character(len=256) :: linea
        integer :: ios
        character(len=256) :: nombre, continente, pais, poblacion, saturacion, bandera
        logical :: pais_abierto

        pais_abierto = .false.

        ! Abrir el archivo de entrada
        open(unit=10, file=archivo_entrada, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error al abrir el archivo de entrada'
            stop
        end if

        ! Abrir el archivo de salida
        open(unit=20, file=archivo_salida, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            print *, 'Error al abrir el archivo de salida'
            stop
        end if

        ! Leer y escribir el archivo línea por línea
        do
            read(10, '(A)', iostat=ios) linea
            if (ios /= 0) exit

            ! Procesar la línea para extraer datos y escribir en el archivo de salida
        if (index(linea, 'Grafica:{') > 0) then
            write(20, '(A)') trim(linea)
            ! Leer el nombre de la gráfica
            read(10, '(A)', iostat=ios) linea
            call extraer_valor(linea, nombre)
            write(20, '(A)') 'Nombre: ' // trim(nombre) // ''
        else if (index(linea, 'Continente:{') > 0) then
            write(20, '(A)') trim(linea)
            ! Leer el nombre del continente
            read(10, '(A)', iostat=ios) linea
            call extraer_valor(linea, continente)
            write(20, '(A)') '    Nombre: ' // trim(continente) // ';'
        else if (index(linea, 'Pais:{') > 0) then
            write(20, '(A)') trim(linea)
            pais_abierto = .true.
            ! Inicializar variables del país
            pais = ''
            poblacion = ''
            saturacion = ''
            bandera = ''
        else if (index(linea, 'Nombre:') > 0 .and. pais_abierto) then
            call extraer_valor(linea, pais)
        else if (index(linea, 'Poblacion:') > 0 .and. pais_abierto) then
            call extraer_valor(linea, poblacion)
        else if (index(linea, 'Saturacion:') > 0 .and. pais_abierto) then
            call extraer_valor(linea, saturacion)
        else if (index(linea, 'Bandera:') > 0 .and. pais_abierto) then
            call extraer_valor(linea, bandera)
        else if (index(linea, '}') > 0 .and. pais_abierto) then
            ! Escribir los datos del país en el orden correcto
            !write(20, '(A)') '    Pais:{'
            write(20, '(A)') '        Nombre: ' // trim(pais) // ''
            write(20, '(A)') '        Poblacion: ' // trim(poblacion) // ''
            write(20, '(A)') '        Saturacion: ' // trim(saturacion) // ''
            write(20, '(A)') '        Bandera: ' // trim(bandera) // ''
            write(20, '(A)') '    }'
            pais_abierto = .false.
        else
            write(20, '(A)') trim(linea)
            end if
        end do

        ! Cerrar los archivos
        close(10)
        close(20)
    call graficaarbo(archivo_salida)
    
    contains

        subroutine extraer_valor(linea, valor)
            character(len=*), intent(in) :: linea
            character(len=256), intent(out) :: valor
            integer :: pos

            pos = index(linea, ':') + 1
            valor = adjustl(trim(linea(pos:)))
        end subroutine extraer_valor

    end subroutine procesar_archivo

end module archivo_mod
