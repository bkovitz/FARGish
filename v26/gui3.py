from tkinter import *
#from tkinter.ttk import *
from tkscrolledframe import ScrolledFrame
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from colorsys import hls_to_rgb

from Grid2 import Canvas as GCanvas, A, as_xy, Addr, two_cs, \
    default_seed_addrs, Model, WorkingSoup, LongTermSoup, make_small_seed_model
from util import dupdate, pts, sstr

def prt(*args, **kwargs):
    print('PRT', args, kwargs)

class App(Frame):

    def __init__(self):
        Frame.__init__(self, highlightbackground='blue', highlightthickness=4)
        self.pack(side=TOP, expand=YES, fill=BOTH, anchor=NW)
        self.master.title('Canvas-and-Painters: Grid')
        #self.make_modelcanvasframe()
        #self.make_painterframe()


        leftframe = Frame(self)
        leftframe.pack(side=LEFT, expand=NO, fill=None, anchor=NW)
        self.gridw = GridWidget(leftframe)
        self.gridw.pack(side=TOP, anchor=NW, padx=3, pady=3)
        Button(leftframe, text='Small seed', command=self.small_seed).pack(side=LEFT, expand=YES)
        Button(leftframe, text='Step', command=self.step).pack(side=LEFT, expand=YES)
        self.bind_all('<s>', self.step)

        self.painterframe = ScrolledFrame(self)
        #self.paintertable = SimpleTable(self.painterframe)
        self.painterframe.display_widget(SimpleTable)
        self.painterframe.pack(side=LEFT, expand=YES, fill=BOTH)

        self.small_seed()

    def key(self, event):
        print(event)

    def small_seed(self):
        self.model = make_small_seed_model()
        self.draw_model()

    def draw_model(self):
        self.gridw.draw_gc(self.model.c)
        for row, (rpainter, activation) in enumerate(
            self.model.ltsoup.rpainters.items()
        ):
            #self.rightframe.set(row, 0, sstr(rpainter))
            #self.rightframe.set(row, 1, str(activation))
            pass
            

    def step(self, event=None):
        print(event)
        self.model.iteration()
        self.draw_model()

class GridWidget(Canvas):

    defaults = dict(bg='white', width=400, height=400)

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **dupdate(self.defaults, kwargs))
        self.draw_outlines()

    def draw_gc(self, gc: GCanvas) -> None:
        '''Updates all the squares in the GridWidget to match 'gc'.'''
        for a in gc.all_addrs():
            self.draw_square(a, gc[a], gc.clarity(a))

    lo_hue = 170   # deep blue
    lo_sat = 153
    hi_hue = 0     # deep red
    hi_sat = 153

    def draw_square(self, a: Addr, v: int, clarity: int) -> None:
        color: str
        if clarity == 0 or v == 0:
            color = 'white'
        else:
            if v < 0:
                #color = '#228'
                h = self.lo_hue
                s = self.lo_sat
            elif v > 0:
                #color = '#822'
                h = self.hi_hue
                s = self.hi_sat
            rgb = hls_to_rgb(h / 255, 1 - (clarity / 9), s / 255)
            color = '#' + ''.join('%02x' % int(x * 255) for x in rgb)
        self.create_rectangle(*self.addr_to_canvasxy(a), fill=color, outline='')

    @classmethod
    def addr_to_canvasxy(cls, a: Addr) -> Tuple[int, int, int, int]:
        gx, gy = as_xy(a)
        x = (gx - 1) * 50 + 1
        y = (8 - gy) * 50 + 1
        return (x, y, x + 49, y + 49)

    def draw_outlines(self):
        for x in range(0, 351, 50):
            for y in range(0, 351, 50):
                self.create_rectangle(x + 50, y + 50, x, y, outline='#eee')

class SimpleTable(Frame):
    def __init__(self, parent, rows=300, columns=2):
        Frame.__init__(self, parent, background='black', highlightbackground='green', highlightthickness=5)
        self._widgets = []
        for row in range(rows):
            current_row = []
            for column in range(columns):
                label = Label(self, text='%s/%s' % (row, column),
                              borderwidth=0, width=25)
                label.grid(row=row, column=column, sticky='n',
                           padx=1, pady=1)
                current_row.append(label)
            self._widgets.append(current_row)

        self.grid_columnconfigure(0, weight=2)
        self.grid_columnconfigure(0, weight=1)
        self.pack(side=LEFT, fill=BOTH, expand=YES)
#        for column in range(columns):
#            self.grid_columnconfigure(column, weight=1)

    def set(self, row, column, value):
        widget = self._widgets[row][column]
        widget.configure(text=value)

if __name__ == '__main__':
#    root = Tk()
#    root.title('Canvas-and-Painters: Grid')
#    root.geometry('800x600')
#
#    gridw = GridWidget(root)
#    '''
#    gridw.draw_square((3, 4), +1, 2)
#    gridw.draw_square((1, 1), -1, 2)
#    gridw.draw_square((1, 2), -1, 2)
#    gridw.draw_square((8, 8), -1, 2)
#    gridw.draw_square((7, 8), -1, 2)
#    for clarity in range(7):
#        gridw.draw_square((clarity + 1, 6), -1, clarity)
#        gridw.draw_square((clarity + 1, 7), +1, clarity)
#    '''
#    gc = GCanvas.from_data(two_cs)
#    ltsoup = LongTermSoup.make_from_canvas(gc)
#    gc.blank_all_but(default_seed_addrs)
#    m = Model(
#        ltsoup,
#        WorkingSoup.make_from_canvas(gc),
#        gc
#    )
#    gridw.draw_gc(gc)
#    root.mainloop()

    App().mainloop()
