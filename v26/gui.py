from __future__ import annotations

from tkinter import *
#from tkinter.ttk import *
from dataclasses import dataclass
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from colorsys import hls_to_rgb

from Grid2 import Canvas as GCanvas, A, as_xy, Addr, two_cs, \
    default_seed_addrs, Model, WorkingSoup, LongTermSoup, make_small_seed_model
from util import dupdate, pts, sstr, Numeric

def prt(*args, **kwargs):
    print('PRT', args, kwargs)

class App(Frame):

    def __init__(self):
        Frame.__init__(self, highlightbackground='blue', highlightthickness=4)
        self.pack(side=TOP, expand=YES, fill=BOTH, anchor=NW)
        self.master.title('Canvas-and-Painters: Grid')
        leftframe = Frame(self)
        leftframe.pack(side=LEFT, expand=NO, fill=None, anchor=NW)
        self.gridw = GridWidget(leftframe)
        self.gridw.pack(side=TOP, anchor=NW, padx=3, pady=3)
        Button(leftframe, text='Small seed', command=self.small_seed).pack(side=LEFT, expand=YES)
        Button(leftframe, text='Step', command=self.step).pack(side=LEFT, expand=YES)
        self.bind_all('<s>', self.step)

#        self.rightcanvas = Canvas(self, borderwidth=0, highlightbackground='red', highlightthickness=4) # will be scrollable
#        self.rightframe = PainterTable(self.rightcanvas)
#        #self.rightframe = Frame(self.rightcanvas, highlightbackground='green', highlightthickness=4)
#        #Label(self.rightframe, text='Look at this nice text').pack(side=LEFT, expand=YES, fill=X)
#        self.rightframe.pack(side=LEFT, expand=YES, fill=BOTH)
#
#        def my_command(*args, **kwargs):
#            #print('my_command:', args, kwargs)
#            self.rightcanvas.yview(*args, **kwargs)
#
#        self.vsb = Scrollbar(
#            self, orient='vertical', command=self.rightcanvas.yview
#            #self, orient='vertical', command=my_command
#        )
#
#        self.rightcanvas.configure(yscrollcommand=self.vsb.set)
#
#        self.vsb.pack(side='right', fill='y')
#        self.rightcanvas.pack(side='left', fill='both', expand=YES)
#        self.rightcanvas.create_window(
#            (4, 4), window=self.rightframe,
#            anchor='nw', tags='self.frame'
#        )
#
#        self.rightframe.bind('<Configure>', self.onFrameConfigure)
#        self.rightcanvas.bind('<Configure>', self.onCanvasConfigure)
    
        self.painter_table = PainterTable(self)
        self.painter_table.pack(side=LEFT, expand=YES, fill=BOTH)

        #self.wsoup = PainterTable(self)
        #self.wsoup.pack(side=RIGHT, fill='both')
        self.small_seed()
        print('HERE1')

    def onFrameConfigure(self, event):
        '''Reset the scroll region to encompass the inner frame'''
        self.rightcanvas.configure(scrollregion=self.rightcanvas.bbox("all"))

    def onCanvasConfigure(self, event):
        self.rightcanvas.itemconfigure('self.frame', width=event.width)

    def key(self, event):
        print(event)

    def small_seed(self):
        self.model = make_small_seed_model()
        self.draw_model()

    def draw_model(self):
        self.gridw.draw_gc(self.model.c)
#        for row, (rpainter, activation) in enumerate(
#            self.model.ltsoup.rpainters.items()
#        ):
#            self.rightframe.set(row, 0, sstr(rpainter))
#            self.rightframe.set(row, 1, '%8.3f' % activation)
#            pass
        self.painter_table.set_painters(
            PainterAndWeight(p, self.model.weight_of(p))
                for p in self.model.ltsoup.all_painters(self.model)
        )

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
        Frame.__init__(self, parent) #, background='black', highlightbackground='green', highlightthickness=5)
        self._widgets = []
        self.painters_button = Button(self, text='Painters')
        self.painters_button.grid(row=0, column=0)
        self.weights_button = Button(self, text='Weights')
        self.weights_button.grid(row=0, column=1)

        for row in range(rows):
            current_row = []
            for column in range(columns):
                label = Label(self, text='%s/%s' % (row, column),
                              borderwidth=0) #, width=25)
                label.grid(row=row+1, column=column, sticky='w' if column == 0 else None,
                           padx=1, pady=1)
                current_row.append(label)
            self._widgets.append(current_row)

        self.grid_columnconfigure(0, weight=1)
        self.grid_columnconfigure(0, weight=1)
#        for column in range(columns):
#            self.grid_columnconfigure(column, weight=1)

    def set(self, row, column, value):
        widget = self._widgets[row][column]
        widget.configure(text=value)

class PainterTable(Frame):
    
    def __init__(self, parent):
        Frame.__init__(self, parent)
        self._pwtmap: Dict[PainterAndWeight, PWtLabels] = {}
        self._sort_order: Literal['by_painters', 'by_weights'] = 'by_painters'

        self._scrollable_canvas = Canvas(self, borderwidth=0)
        self._inner_frame = Frame(self._scrollable_canvas)
        self._inner_frame.pack(side=LEFT, expand=YES, fill=BOTH)
        self._vsb = Scrollbar(
            self, orient='vertical', command=self._scrollable_canvas.yview
        )
        self._scrollable_canvas.configure(yscrollcommand=self._vsb.set)
        self._vsb.pack(side='right', fill='y')
        self._scrollable_canvas.pack(side=LEFT, expand=YES, fill=BOTH)
        self._scrollable_canvas.create_window(
            (4, 4), window=self._inner_frame, anchor='nw', tags='self.frame'
        )
        self._inner_frame.bind('<Configure>', self.onFrameConfigure)

        self._nlabel = Label(self._inner_frame, text='n=')
        self._nlabel.grid(row=0, column=0, columnspan=2)
        Button(
            self._inner_frame, text='Painters', command=self.sort_by_painters
        ).grid(row=1, column=0)
        Button(
            self._inner_frame, text='Weights', command=self.sort_by_weights
        ).grid(row=1, column=1)

    def sort_by_painters(self):
        self._sort_order = 'by_painters'
        self.show_painters()

    def sort_by_weights(self):
        self._sort_order = 'by_weights'
        self.show_painters()

    def onFrameConfigure(self, event):
        '''Reset the scroll region to encompass the inner frame'''
        print('onFrameConfigure1')
        self._scrollable_canvas.configure(
            scrollregion=self._scrollable_canvas.bbox("all")
        )
        print('onFrameConfigure2')

    def set_painters(self, pwts: Iterable[PainterAndWeight]) -> None:
        self.clear()
        for pwt in pwts:
            self._pwtmap[pwt] = PWtLabels.make_from(self._inner_frame, pwt)
        self.show_painters()

    def show_painters(self):
        self._nlabel.configure(text=f'n={len(self._pwtmap)}')
        self.arrange_sorted()

    def clear(self) -> None:
        '''Remove all painters and all their labels in the grid.'''
        for pl in self._pwtmap.values():
            pl.forget()
        self._pwtmap.clear()

    def arrange_sorted(self):
        '''Arrange the labels for all the painters and their weights according
        to the current ._sort_order.'''
        for row, pwt in enumerate(sorted(
            self._pwtmap.keys(),
            key=(
                (lambda pwt: sstr(pwt.painter))
                    if self._sort_order == 'by_painters'
                    else lambda pwt: -pwt.weight
            )
        )):
            self._pwtmap[pwt].place(row + 2)

@dataclass(frozen=True)
class PainterAndWeight:
    painter: Any
    weight: Numeric

@dataclass(frozen=True)
class PWtLabels:
    '''The Labels that show a certain PainterAndWeight.'''
    painter_label: Label
    weight_label: Label

    def place(self, row: int) -> None:
        self.painter_label.grid(row=row, column=0, sticky='w')
        self.weight_label.grid(row=row, column=1)

    def forget(self) -> None:
        self.painter_label.grid_forget()
        self.weight_label.grid_forget()

    @classmethod
    def make_from(self, frame: Frame, pwt: PainterAndWeight) -> PWtLabels:
        return PWtLabels(
            Label(frame, text=sstr(pwt.painter)),
            Label(frame, text='%9.3f' % pwt.weight)
        )

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
