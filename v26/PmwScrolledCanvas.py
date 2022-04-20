from tkinter import *
import Pmw

root = Tk()
#root.option_readfile('optionDB')
Pmw.initialise()

sc = Pmw.ScrolledCanvas(root, borderframe=1, labelpos=N,
                        label_text='ScrolledCanvas', usehullsize=1,
                        hull_width=400, hull_height=300)
for i in range(20):
    x = -10 + 3 * i
    y = -10
    for j in range(10):
        sc.create_rectangle(
            '%dc' % x, '%dc' % y, '%dc' % (x+2), '%dc' % (y+2),
            fill='cadetblue', outline='black')
        sc.create_text(
            '%dc' % (x+1), '%dc' % (y+1),
            text='%d,%d' % (i, j),
            anchor=CENTER, fill='white')
        y = y + 3

sc.pack()
sc.resizescrollregion()

def on_frame_configure(event):
    print('on_frame_configure')
    sc.component('hull').itemconfigure('self.frame', width=event.width)

sc.bind('<Configure>', on_frame_configure)

root.mainloop()
