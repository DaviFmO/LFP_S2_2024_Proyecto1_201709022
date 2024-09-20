
from tkinter import *
import tkinter as tk 
from tkinter import scrolledtext
from tkinter import messagebox
import subprocess
ventana = Tk()

ventana.title("Programa de Interfaz Grafica")
ventana.geometry("800x700")
ventana.config(bg="lightblue")
etq1 = Label(ventana, text="ANALIZADAR DE MERCADO")
etq1.pack()

archivo_actual = " "  # Variable global para almacenar la ruta del archivo actual
#abrir archivo con bucador de archivos
from tkinter import filedialog
def abrira():
    archivo_ = filedialog.askopenfilename(
        defaultextension=".ORG", 
        filetypes=[("Archivos ORG", "*.ORG"), ("Todos los archivos", "*.*")])
    if archivo_:
        with open(archivo_, "r") as archivo:
            contenido = archivo.read()
        text_area.delete(1.0, tk.END)  # Limpiar el área de texto
        text_area.insert(tk.END, contenido)  # Insertar el contenido en el área de texto
        ventana.title(f"Editor - {archivo_}")  # Mostrar el nombre del archivo en la ventana
        global archivo_actual
        archivo_actual = archivo_  # Actualizar el archivo actual

# archivo = filedialog.askopenfilename(title="Abrir Archivo")
# if archivo != "":
#     fichero = open(archivo, 'r')
#     contenido = fichero.read()
#     text_area.insert(1.0, contenido)
#     fichero.close()
#guardar archivo con buscador de archivos
def guardar_archivo():
    global archivo_actual  # Declaración global al inicio
    if text_area.get(1.0, tk.END) == "\n":  # Si el área de texto está vacía
        messagebox.showwarning("Advertencia", "No hay contenido para guardar")
    
    elif archivo_actual == " ":  # Si no hay un archivo abierto
        guardar_como_archivo  # Si no hay un archivo abierto, llamamos a "Guardar como"
    # limpiar el archivo actual
        #  archivo_actual = None
    else:
        # Asegúrate de que `archivo_actual` sea la ruta del archivo y no el objeto archivo
        with open(archivo_actual, "w") as archivo:
            archivo.write(text_area.get(1.0, tk.END))  # Guardar el contenido actual
        messagebox.showinfo("Guardar", f"Archivo guardado: {archivo_actual}")
         # archivo_actual = None  # Limpiar el archivo actual después de guardar

def guardar_como_archivo():
    global archivo_actual  # Declaración global al inicio
    archivo_ = filedialog.asksaveasfilename(
        defaultextension=".ORG", 
        filetypes=[("Archivos ORG", "*.ORG"), ("Todos los archivos", "*.*")]
    )
    if archivo_:
        with open(archivo_, "w") as archivo:
            archivo.write(text_area.get(1.0, tk.END))  # Guardar el contenido actual
        ventana.title(f"Editor - {archivo_}")  # Mostrar el nombre del archivo en la ventana
        archivo_actual = archivo_  # Actualizar el archivo actual con la ruta
        messagebox.showinfo("Guardar como", f"Archivo guardado como: {archivo_}")
         # archivo_actual=None
        
   #  archivo = filedialog.asksaveasfilename(title="Guardar Archivo")
    # if archivo != "":
      #   fichero = open(archivo, 'w')
        # contenido = text_area.get(1.0, 'end-1c')
        # fichero.write(contenido)
        # fichero.close()



#guradar archivo con el buscador si es la primera vez que se guarda
def guardar2():
    archivo = filedialog.asksaveasfilename(
        defaultextension=".ORG", 
        filetypes=[("Archivos ORG", "*.ORG"), ("Todos los archivos", "*.*")]
    )
    if archivo:
        with open(archivo, "w") as archivo:
            archivo.write(text_area.get(1.0, tk.END))  # Guardar el contenido actual
        ventana.title(f"Editor - {archivo}")  # Mostrar el nombre del archivo en la ventana
        messagebox.showinfo("Guardar", f"Archivo guardado: {archivo}")
    # if archivo != "":
    #     fichero = open(archivo, 'w')
    #     contenido = text_area.get(1.0, 'end-1c')
    #     fichero.write(contenido)
    #     fichero.close()
    # else:
    #     messagebox.showinfo("Información", "No se ha guardado el archivo")
#limpiar area de texto
def limpiar():
    text_area.delete(1.0, 'end')
    text_area2.config(state=NORMAL)
    text_area2.delete(1.0, 'end')
    text_area2.config(state=DISABLED)
    archivo_actual = None
    
def guardar_y_analizar():
    texto=text_area.get(1.0,END)
    with open("texto.txt", "w") as archivo:
        archivo.write(texto)
    subprocess.run(["./lexer"])




#barra de menu
barraMenu = Menu(ventana)
ventana.config(menu=barraMenu)
#menu desplegable
menuArchivo = Menu(barraMenu, tearoff=False)
menuacerca = Menu(barraMenu, tearoff=False)
barraMenu.add_cascade(label="Archivo", menu=menuArchivo)
barraMenu.add_cascade(label="Acerca de", menu=menuacerca)

#salir del programa
def salir():
    ventana.quit()
    ventana.destroy()
#acerca de
def acercade():
    messagebox.showinfo("Acerca de", "Analizador de Mercado\nVersion 1.0\nDesarrollado por: \nDavid Orlando Fuentes Morales\nCARNE: 201709022")


#opciones del menu
menuArchivo.add_command(label="Abrir", command=abrira)
menuArchivo.add_command(label="Guardar", command=guardar_archivo)
menuArchivo.add_command(label="Guardar Como", command=guardar_como_archivo)
menuArchivo.add_command(label="Salir", command=salir)
menuArchivo.add_command(label="Limpiar", command=limpiar)
menuacerca.add_command(label="Acerca de",command=acercade)


#area de texto para codigo de entrada con barra de desplazamiento
text_area = scrolledtext.ScrolledText(ventana, width=35, height=25,bg="lightyellow") 
text_area.place(x=50, y=50)


#area de texto para codigo de salida bloqueado
text_area2 = Text(ventana, width=35, height=25)
text_area2.place(x=500, y=50)
text_area2.config(state=DISABLED)


"""
#barra de desplazamiento para el area de texto
scroll = Scrollbar(ventana, orient="vertical", command=text_area.yview)
scroll.pack(side="right", fill="y")
text_area.config(yscrollcommand=scroll.set)
"""

#frame para los botones
frame = Frame(ventana)
frame.place(x=370, y=200)

#botones dentro del frame
btn1 = Button(frame, text="Analizar", command=guardar_y_analizar)
btn1.grid(row=0, column=0)
btn2 = Button(frame, text="Limpiar")
btn2.grid(row=1, column=0)
btn3 = Button(frame, text="Salir")
btn3.grid(row=2, column=0)

#frame para resultados nombre e imagen
frame2 = Frame(ventana, width=200, height=200, bg="lightyellow")
frame2.pack_propagate(False)
frame2.place(x=330, y=470)

#etiquetas para los resultados
etq2 = Label(frame2, text="Resultado:")
etq2.pack()
etq3 = Label(frame2, text=".")
etq3.place(x=10, y=30)
etq4 = Label(frame2, text=".")
etq4.place(x=10, y=50)





ventana.mainloop()
