import customtkinter

customtkinter.set_appearance_mode("dark")
customtkinter.set_default_color_theme("green")

class App(customtkinter.CTk):
    def __init__(self):
        super().__init__()
        self.geometry("1920x1080")
        self.title("Temp Title")

        self.rendered = False

        self.frame1 = customtkinter.CTkFrame(self, width=500, height=400)
        customtkinter.CTkLabel(self.frame1, text="wow youve rendered the frame").grid(row=0, column=0)
        customtkinter.CTkButton(self, text="Temp Button", command=print("balls")).grid(row=1, column=0)

        customtkinter.CTkButton(self, text="Toggle render", command=lambda: self.render(self.frame1)).grid()

    def render(self, frame):
        if self.rendered:
            frame.grid_remove()
            self.rendered = False
        else:
            frame.grid()
            self.rendered = True


if __name__ == "__main__":
    app = App()
    app.mainloop()
