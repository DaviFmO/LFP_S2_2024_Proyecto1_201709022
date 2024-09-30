import tkinter as tk
from tkinter import ttk

# Crear la ventana principal
root = tk.Tk()
root.geometry("400x300")
root.title("Ventana con Scroll")

# Crear un frame para organizar el listbox y la scrollbar
frame = ttk.Frame(root)
frame.pack(fill="both", expand=True)

# Crear un Listbox que contendrá varios elementos
listbox = tk.Listbox(frame)
listbox.pack(side="left", fill="both", expand=True)

# Crear una scrollbar y asociarla con el Listbox
scrollbar = ttk.Scrollbar(frame, orient="vertical", command=listbox.yview)
scrollbar.pack(side="right", fill="y")
listbox.configure(yscrollcommand=scrollbar.set)

# Agregar varios elementos al Listbox
for i in range(100):
    listbox.insert(tk.END, f"Elemento {i+1}")

# Ejecutar la aplicación
root.mainloop()