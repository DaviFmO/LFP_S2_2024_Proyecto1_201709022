from tkinter import *
import tkinter as tk 
from tkinter import ttk
from tkinter import scrolledtext
from tkinter import messagebox
from tkinter import PhotoImage
from PIL import Image, ImageTk
import subprocess
ventana = Tk()
archivo_abierto = None
considencia=False
ventana.title("Programa de Interfaz Grafica")
ventana.geometry("1200x900")
ventana.config(bg="lightblue")
frame=ttk.Frame(ventana)
frame.pack(fill="both", expand=True)
Scrollbar=ttk.Scrollbar(frame, orient="vertical")
Scrollbar.pack(side="right", fill="y")
etq1 = Label(ventana, text="ANALIZADAR DE MERCADO")
etq1.pack()
etq1.place(x=450, y=10)
etq5=Label(ventana, text="hollaaaaaaaaaaaaaaaaaaaaaaaaaaaa.")
etq5.pack()
#ubicar el label en la ventana
etq5.place(x=450, y=1500)



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
def limpiar_documento():
    archivo = 'Errores.html'  # Especifica la ruta de tu archivo predeterminado
    try:
        with open(archivo, 'w') as file:
            file.truncate(0)
        #print(f"El archivo {archivo} ha sido limpiado.")
    except Exception as e:
        print(f"Error al limpiar el archivo: {e}")

        # aqui es el arreglo de que hice de los guardar

def guardar_archivo():
    global archivo_abierto
    if archivo_abierto:
        with open(archivo_abierto, 'w') as file:
            contenido = text_area.get(1.0, tk.END)
            file.write(contenido)
            messagebox.showinfo("Guardar como", f"Archivo guardado como: {archivo_abierto}")
    elif text_area.get(1.0, tk.END) == "\n":  # Si el área de texto está vacía
        messagebox.showwarning("Advertencia", "No hay contenido para guardar")
    else:
        guardar_como()

def guardar_como():
    global archivo_abierto
    archivo_abierto = filedialog.asksaveasfilename(defaultextension=".txt", filetypes=[("Archivos de texto", "*.txt")])
   
    if archivo_abierto:
        with open(archivo_abierto, 'w') as file:
            contenido = text_area.get(1.0, tk.END)
            file.write(contenido)



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
    #limpiar label
    for widget in frame2.winfo_children():
        widget.destroy()
    etq3 = Label(frame2, text="Resultado:")
    etq3.pack()
    
def guardar_y_analizar():
    texto=text_area.get(1.0,END)
    with open("texto.txt", "w") as archivo:
        archivo.write(texto)
    subprocess.run(["./lexer"])
    resultado = subprocess.run(["./lexer.exe"], capture_output=True, text=True)
        
        # Mostrar la salida en el label
    #etq3.config(text=resultado.stdout)
    lineas = resultado.stdout.split("\n")
    #limpiar label
    for widget in frame2.winfo_children():
        widget.destroy()
    for linea in lineas:
        if 'Bandera:' in linea:
            ruta_imagen = linea.split(":")[1].strip()
            try:
                img=Image.open(ruta_imagen)
                tamaño_img = (100,100)
                img.thumbnail(tamaño_img)
                img. img = img.resize(tamaño_img)
                img_tk = ImageTk.PhotoImage(img)
                etq3=tk.Label(frame2, image=img_tk)
                etq3.image = img_tk
                etq3.pack()
                imprimir_imagen()
                
            except Exception as e:
                etq3_error = tk.Label(frame2, text="Error al cargar la imagen")
                etq3_error.pack()   
        elif 'Errores' in linea:
            imprimir_imagen1()
            #limpiar label
            for widget in frame2.winfo_children():
                widget.destroy()
            etq3=tk.Label(frame2, text=linea)
            etq3.pack()
           
              
        
        else:
            etq3=tk.Label(frame2, text=linea)
            etq3.pack()
           


    
        

#imprimir imagen en area de texto
def imprimir_imagen():
    #limpiar area de texto
    text_area2.config(state=NORMAL)
    text_area2.delete(1.0, END)
    text_area2.config(state=DISABLED)
    
    
    arcchivo_imagen = "graficoultimo.png"
    
    imagen = Image.open(arcchivo_imagen)
    imagen = imagen.resize((1100, 350))
    imagen_tk = ImageTk.PhotoImage(imagen)
    text_area2.delete(1.0, END)
    text_area2.config(state=NORMAL)
    text_area2.image_create(END, image=imagen_tk)
    text_area2.config(state=DISABLED)
    text_area2.image = imagen_tk
    
def imprimir_imagen1():
    #limpiar area de texto
    text_area2.config(state=NORMAL)
    text_area2.delete(1.0, END)
    text_area2.config(state=DISABLED)
    
    
    arcchivo_imagen = "error.png"
    imagen = Image.open(arcchivo_imagen)
    imagen = imagen.resize((300, 300))
    imagen_tk = ImageTk.PhotoImage(imagen)
    text_area2.delete(1.0, END)
    text_area2.config(state=NORMAL)
    text_area2.image_create(END, image=imagen_tk)
    text_area2.config(state=DISABLED)
    text_area2.image = imagen_tk

def funcione():
    #limpiar_area_imagen()
    limpiar_documento()
    limpiar_area_imagen()
    guardar_y_analizar()
   
    
    
   
def limpiar_area_imagen():
    text_area2.config(state=NORMAL)
    text_area2.delete(1.0, 'end')
    text_area2.config(state=DISABLED)
    
    

#limpiar area de texto
    
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
menuArchivo.add_command(label="Guardar Como", command=guardar_como)
menuArchivo.add_command(label="Salir", command=salir)
menuArchivo.add_command(label="Limpiar", command=limpiar)
menuacerca.add_command(label="Acerca de",command=acercade)


#area de texto para codigo de entrada con barra de desplazamiento
text_area = scrolledtext.ScrolledText(ventana, width=60, height=20,bg="lightyellow") 
text_area.place(x=50, y=50)


#area de texto para codigo de salida bloqueado
text_area2 = Text(ventana, width=140, height=25)
text_area2.place(x=50, y=400)
text_area2.config(state=DISABLED)


"""
#barra de desplazamiento para el area de texto
scroll = Scrollbar(ventana, orient="vertical", command=text_area.yview)
scroll.pack(side="right", fill="y")
text_area.config(yscrollcommand=scroll.set)
"""

#frame para los botones
frame = Frame(ventana)
frame.place(x=600, y=300)

#botones dentro del frame
btn1 = Button(frame, text="Analizar", command=funcione)
#btn1= Button(frame, text="Analizar", command=imprimir_imagen)
btn1.grid(row=0, column=0)
btn2 = Button(frame, text="Limpiar", command=limpiar)
btn2.grid(row=1, column=0)
btn3 = Button(frame, text="Salir", command=salir)
btn3.grid(row=2, column=0)

#frame para resultados nombre e imagen
frame2 = Frame(ventana, width=400, height=200, bg="lightyellow")
frame2.pack_propagate(False)
frame2.place(x=600, y=70)

#etiquetas para los resultados
etq2 = Label(frame2, text="Resultado:")
etq2.pack()
etq3 = Label(frame2, text=".")
etq3.place(x=10, y=30)
etq4 = Label(frame2, text=".")
etq4.place(x=10, y=50)

ventana.mainloop()
