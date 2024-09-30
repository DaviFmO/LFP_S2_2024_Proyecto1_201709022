import tkinter as tk
from PIL import Image, ImageTk  # Necesitas instalar Pillow: pip install pillow

def cargar_e_imprimir_imagen():
    # Cargar la imagen predeterminada (debes especificar la ruta de la imagen)
    archivo_imagen = r"C:\Users\PC\Desktop\Proyecto 1 Final\graficoultimo.png"  # Cambia por la ruta de tu imagen
    imagen = Image.open(archivo_imagen)
    
    # Ajustar el tamaño de la imagen
    imagen = imagen.resize((500, 300))  # Ajusta según sea necesario
    imagen_tk = ImageTk.PhotoImage(imagen)
    
    # Mostrar la imagen en el Label
    label_imagen.config(image=imagen_tk)
    label_imagen.image = imagen_tk  # Guardar una referencia para evitar el recolector de basura
    
    # "Imprimir" la imagen abriéndola en el visor de imágenes predeterminado
    #imagen.show()  # Esto abrirá la imagen con el visor predeterminado del sistema

# Crear la ventana principal
ventana = tk.Tk()
ventana.title("Cargar e Imprimir Imagen Predeterminada")

# Crear un botón para cargar e imprimir la imagen
boton_cargar = tk.Button(ventana, text="Cargar e Imprimir Imagen", command=cargar_e_imprimir_imagen)
boton_cargar.pack(pady=10)

# Crear un Label para mostrar la imagen
label_imagen = tk.Label(ventana)
label_imagen.pack()

# Ejecutar la ventana
ventana.mainloop()