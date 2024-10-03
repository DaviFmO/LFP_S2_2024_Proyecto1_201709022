module graficaarbol
    implicit none
    contains
    subroutine graficaarbo(entrada_archivo)
               
            implicit none
            character(len=200) :: linea
            character(len=50) :: nombre_grafica, continente, pais, pais_min_saturacion
            character(len=50) :: pais_menor_global, continente_menor_global
            character(len=100) :: bandera, bandera_min_saturacion, bandera_menor_global
            integer :: poblacion, poblacion_menor_global, poblacion_min_saturacion, end_of_file, i, num_paises
            character(len=10) :: saturacion
            logical :: leyendo_continente, leyendo_pais, leyendo_grafica
            real :: suma_saturacion, promedio_saturacion, min_saturacion
            real :: min_saturacion_global, promedio_saturacion_global
            integer :: saturacion_val, promedio_saturacion_entero
            integer :: salida_unit
            integer :: ios
            character(len=100) :: dot_filename, command
            character(len=20) :: cadens
            character(len=20) :: cadena
            character(len=20) :: entrada_archivo
            character(len=20) :: nombre_imagens
            dot_filename="graficaultima3.dot"
            !entrada_archivo="Entrada2.ORG"
            salida_unit = 30
        
            
        
            leyendo_continente = .false.
            leyendo_pais = .false.
            leyendo_grafica = .false.
            suma_saturacion = 0.0
            num_paises = 0
            min_saturacion = 1.0E6  ! Inicializamos con un valor muy alto
            min_saturacion_global = 1.0E6  ! Inicializamos el valor global
            promedio_saturacion_global = 1.0E6  ! Inicializamos el promedio global
        
        
            ! Abrir archivo de salida
            open(unit=20, file="SalidaDavid.txt", status="replace", action="write")
            !crear archivo .dot
            open(unit=11, file=dot_filename, status="replace",iostat=ios)
            if (ios /= 0) then
                print *, "Error al abrir el archivo."
                stop
            end if
            open(unit=10, file=entrada_archivo, status="old", action="read")
        
            ! Leer el archivo línea por línea
            do 
               read(10, '(A)', iostat=end_of_file) linea
               if (end_of_file /= 0) exit
        
               ! Buscar el inicio de la gráfica
               if (index(linea, 'Grafica:{') /= 0) then
                  leyendo_grafica = .true.
               end if
        
               ! Detectar y leer el nombre de la gráfica
               if (index(linea, 'Nombre:') /= 0 .and. leyendo_grafica) then
                  call extraer_valor(linea, nombre_grafica)
                  write(11, '(A)') 'digraph G {'
                  write(11, '(A)') '  "' // trim(nombre_grafica) // '" [shape=box];'
                  
                  write(salida_unit, *) "Nombre de la gráfica: ", nombre_grafica
                  leyendo_grafica = .false.
               end if
        
               ! Detectar y leer el nombre del continente
               if (index(linea, 'Continente:{') /= 0) then
                  ! Imprimir el promedio y el país con menor saturación si hay países
                  if (num_paises > 0) then
                     promedio_saturacion = suma_saturacion / num_paises
                     write(salida_unit, *) "Promedio de saturación en ", continente, ":", promedio_saturacion, "%"
                     !write(11, '(A)') '  "' // trim(continente) // '" [shape=ellipse];'
                     write(salida_unit, *) "País con menor saturación en ", continente, ": ", pais_min_saturacion, " con ", min_saturacion, "%"
        
                     ! Convertir el promedio de saturación a entero para determinar el color
                     promedio_saturacion_entero = nint(promedio_saturacion)
                    ! write(cadens, '(I0)')  promedio_saturacion
                     write(salida_unit, *) "Color del promedio de saturación en ", continente, ": ", determinar_color(promedio_saturacion_entero)
                     write(cadens, '(I0)')  promedio_saturacion_entero 
                     write(11, '(A)') '  "' // trim(continente) // '" [shape=record, label="{' // trim(continente) // '|' // trim(cadens) // '}"];'
                        
                     Write(11, '(A)') '  "' // trim(continente) // '" [style=filled, fillcolor="' // trim(determinar_color(promedio_saturacion_entero)) // '"];'
                     ! Verificar si el promedio del continente es más bajo que el global en caso de empate de saturación
                     if (min_saturacion == min_saturacion_global .and. promedio_saturacion < promedio_saturacion_global) then
                        pais_menor_global = pais_min_saturacion
                        continente_menor_global = continente
                        poblacion_menor_global = poblacion_min_saturacion
                        bandera_menor_global = bandera_min_saturacion
                        promedio_saturacion_global = promedio_saturacion
                     else if (min_saturacion < min_saturacion_global) then
                        min_saturacion_global = min_saturacion
                        pais_menor_global = pais_min_saturacion
                        continente_menor_global = continente
                        poblacion_menor_global = poblacion_min_saturacion
                        bandera_menor_global = bandera_min_saturacion
                        promedio_saturacion_global = promedio_saturacion
                     end if
                  end if
        
                  ! Reiniciar los contadores para el nuevo continente
                  suma_saturacion = 0.0
                  num_paises = 0
                  min_saturacion = 1.0E6
                  leyendo_continente = .true.
               end if
               
               ! Leer el nombre del continente
               if (index(linea, 'Nombre: "') /= 0 .and. leyendo_continente) then
                  call extraer_valor(linea, continente)
                  call eliminar_espacios(continente)  ! Llama a la subrutina para eliminar espacios
                  write(salida_unit, *) "Continente: ", trim(continente)
                    !write(11, '(A)') '  "' // trim(continente) // '" [shape=ellipse];'
                  ! Write(11, '(A)') '  "' // trim(continente) // '" [style=filled, fillcolor="' // trim(determinar_color(promedio_saturacion_entero)) // '"];'
                    write(11, '(A)') '  "' // trim(nombre_grafica) // '" -> "' // trim(continente) // '";'
                  leyendo_continente = .false.
                  leyendo_pais = .true.
               end if
               !detectar y leer el nombre del pais y sus datos
               ! Leer los datos del país
               if (index(linea, 'Pais:{') /= 0) then
                read(10, '(A)', iostat=end_of_file) linea
                call extraer_valor(linea, pais)
                write(salida_unit, *) "   País: ", pais
                
      
                ! Leer población
                read(10, '(A)', iostat=end_of_file) linea
                call limpiar_linea(linea)
                call extraer_entero(linea, poblacion)
                write(salida_unit, *) "      Población: ", poblacion
      
                ! Leer saturación
                read(10, '(A)', iostat=end_of_file) linea
                call limpiar_linea(linea)
                call extraer_saturacion(linea, saturacion)
                write(salida_unit, *) "      Saturación: ", saturacion, "%"
                
      
                ! Convertir saturación a entero y acumular
                read(saturacion, *) saturacion_val
                suma_saturacion = suma_saturacion + saturacion_val
                num_paises = num_paises + 1
      
                ! Determinar el color según la saturación
                select case (saturacion_val)
                    case (0:15)
                        write(salida_unit, *) "White"
                    case (16:30)
                        write(salida_unit, *) "Blue"
                      case (31:45)
                           write(salida_unit, *) "green"
                      case (46:60)
                          write(salida_unit, *) "yellow"
                    case (61:75)
                        write(salida_unit, *) "orange"
                    case (76:100)
                        write(salida_unit, *) "red"
                    case default
                        write(salida_unit, *) "black"
                end select
      
                ! Leer bandera
                read(10, '(A)', iostat=end_of_file) linea
                call limpiar_linea(linea)
                call extraer_valor(linea, bandera)
                write(salida_unit, *) "      Bandera: ", bandera
      
                ! Actualizar el país con la menor saturación en el continente
                if (saturacion_val < min_saturacion) then
                   min_saturacion = saturacion_val
                   pais_min_saturacion = pais
                   poblacion_min_saturacion = poblacion
                   bandera_min_saturacion = bandera   ! Ahora la bandera está correctamente asignada
                end if
                write(11, '(A)') '  "' // trim(pais) // '" [style=filled, fillcolor="' // trim(determinar_color(saturacion_val)) // '"];'
                write(cadena, '(I0)')  saturacion_val
                write(11, '(A)') '  "' // trim(pais) // '" [shape=record, label="{' // trim(pais) // '|' // trim(cadena) // '}"];'
                  write(11, '(A)') '  "' // trim(continente) // '" -> "' // trim(pais) // '";'
              end if
              

               

              
            end do
        
            ! Calcular y mostrar promedio para el último continente
            if (num_paises > 0) then
               promedio_saturacion = suma_saturacion / num_paises
               write(salida_unit, *) "Promedio de saturación en ", continente, ":", promedio_saturacion, "%"
               write(salida_unit, *) "País con menor saturación en ", continente, ": ", pais_min_saturacion, " con ", min_saturacion, "%"
        
               ! Convertir el promedio de saturación a entero para determinar el color
               promedio_saturacion_entero = nint(promedio_saturacion)
               
               write(salida_unit, *) "Color del promedio de saturación en ", continente, ": ", determinar_color(promedio_saturacion_entero)
               write(cadens, '(I0)')  promedio_saturacion_entero
            write(11, '(A)') '  "' // trim(continente) // '" [shape=record, label="{' // trim(continente) // '|' // trim(cadens) // '}"];'
            Write(11, '(A)') '  "' // trim(continente) // '" [style=filled, fillcolor="' // trim(determinar_color(promedio_saturacion_entero)) // '"];'
        
               ! Verificar si el último país tiene la saturación global mínima
               if (min_saturacion == min_saturacion_global .and. promedio_saturacion < promedio_saturacion_global) then
                  pais_menor_global = pais_min_saturacion
                  continente_menor_global = continente
                  poblacion_menor_global = poblacion_min_saturacion
                  bandera_menor_global = bandera_min_saturacion
               else if (min_saturacion < min_saturacion_global) then
                  min_saturacion_global = min_saturacion
                  pais_menor_global = pais_min_saturacion
                  continente_menor_global = continente
                  poblacion_menor_global = poblacion_min_saturacion
                  bandera_menor_global = bandera_min_saturacion
               end if
            end if
        
            ! Imprimir país con menor saturación global
          ! write(20, *) "-------------------------------------------"
            write(20, *) "Pais con menor saturacion global: ", pais_menor_global
            print*, "Pais con menor saturacion global: ", pais_menor_global
            write(20, *) "Continente: ", continente_menor_global
            print*, "Continente: ", continente_menor_global
            write(20, *) "Poblacion: ", poblacion_menor_global
            print*, "Poblacion: ", poblacion_menor_global
            write(20, *) "Bandera: ", bandera_menor_global
            print*, "Bandera: ", bandera_menor_global
            write(20, *) "Saturacion: ", min_saturacion_global, "%"
            print*, "Saturacion: ", min_saturacion_global
           ! write(20, *) "-------------------------------------------"
            
            close(10)
            write(11, '(A)') '}'
            close(11)
            close(20)
            !print*,'archivo generado',dot_filename
            !preparar el comando para generar la imagen
            command = 'dot -Tpng ' // trim(dot_filename) // ' -o graficoultimo.png'
            !ejecutar el comando
            ios=system(command)
            if (ios /= 0) then
                print *, "Error al ejecutar el comando."
                stop
            end if
            !print*, "Archivo DOT generado exitosamente: graficoultimo.dot"
            !close(salida_unit)
        
        contains
            ! Subrutina para extraer valores entre comillas
            subroutine extraer_valor(linea, valor)
                character(len=*), intent(in) :: linea
                character(len=*), intent(out) :: valor
                integer :: inicio, fin
        
                inicio = index(linea, '"') + 1
                fin = index(linea(inicio+1:), '"') + inicio
                valor = linea(inicio:fin-1)
            end subroutine extraer_valor
        
            ! Subrutina para extraer enteros
            subroutine extraer_entero(linea, valor)
                character(len=*), intent(in) :: linea
                integer, intent(out) :: valor
                integer :: pos
                character(len=50) :: sub_linea
        
                pos = index(linea, ':') + 1
                sub_linea = linea(pos:)  ! Extraemos la subcadena que contiene el número
                call limpiar_linea(sub_linea)  ! Limpiar la línea por si tiene caracteres extraños
                read(sub_linea, *) valor  ! Leer el valor entero desde la subcadena
            end subroutine extraer_entero
        
            ! Subrutina para extraer y limpiar el valor de saturación (con símbolo %)
            subroutine extraer_saturacion(linea, valor)
                character(len=*), intent(in) :: linea
                character(len=*), intent(out) :: valor
                integer :: pos
        
                pos = index(linea, ':') + 1
                valor = adjustl(linea(pos:))  ! Extraer el valor de saturación como texto
                ! Eliminar el símbolo % si está presente
                if (index(valor, '%') > 0) then
                    valor = trim(adjustl(valor(1:index(valor, '%')-1)))
                end if
            end subroutine extraer_saturacion
        
            ! Subrutina para limpiar punto y coma y espacios extra
            subroutine limpiar_linea(linea)
                character(len=*), intent(inout) :: linea
                integer :: i
        
                do i = 1, len_trim(linea)
                    if (linea(i:i) == ';') linea(i:i) = ' '  ! Reemplazar punto y coma por espacio
                end do
                linea = adjustl(linea)  ! Ajustar espacios
            end subroutine limpiar_linea
        
            ! Subrutina para eliminar espacios
            subroutine eliminar_espacios(linea)
                character(len=*), intent(inout) :: linea
                character(len=200) :: sin_espacios
                integer :: i, j
        
                j = 1
                do i = 1, len(linea)
                    if (linea(i:i) /= ' ') then
                        sin_espacios(j:j) = linea(i:i)
                        j = j + 1
                    end if
                end do
                sin_espacios(j:) = ''  ! Termina la cadena
                linea = sin_espacios  ! Actualiza la línea original
            end subroutine eliminar_espacios
        
            ! Función para determinar el color según la saturación
            function determinar_color(saturacion_val) result(color)
                integer, intent(in) :: saturacion_val
                character(len=20) :: color
        
                select case (saturacion_val)
                    case (0:15)
                        color = "white"
                    case (16:30)
                        color = "blue"
                    case (31:45)
                        color = "green"
                    case (46:60)
                        color = "yellow"
                    case (61:75)
                        color = "orange"
                    case (76:100)
                        color = "red"
                    case default
                        color = "No definido"
                end select
            end function determinar_color
        
        
        
       
        
        end subroutine graficaarbo
        subroutine lectura(archivo)
            implicit none
            character(len=100) :: archivo
            character(len=200) :: lineas
            integer :: ios
            open(unit=40, file=archivo, status="old", action="read", iostat=ios)
            if (ios /= 0) then
                print *, "Error al abrir el archivo."
                stop
            end if
            do 
                read(40, '(A)', iostat=ios) lineas
                if (ios /= 0) exit
                print *, lineas
            end do
            close(40)
        end subroutine lectura

    end module graficaarbol