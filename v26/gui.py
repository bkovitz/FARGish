from tkinter import *
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from colorsys import hls_to_rgb

from Grid2 import Canvas as GCanvas, A, as_xy, Addr, two_cs, \
    default_seed_addrs, Model, WorkingSoup, LongTermSoup
from util import dupdate, pts

class GridWidget(Canvas):

    defaults = dict(bg='white', width=400, height=400)

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **dupdate(self.defaults, kwargs))
        self.pack(anchor=W, padx=3, pady=3)
        self.draw_outlines()

#    def key(self, event):
#        print(event)

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

if __name__ == '__main__':
    root = Tk()
    root.title('Canvas-and-Painters: Grid')
    root.geometry('800x600')

    gridw = GridWidget(root)
    '''
    gridw.draw_square((3, 4), +1, 2)
    gridw.draw_square((1, 1), -1, 2)
    gridw.draw_square((1, 2), -1, 2)
    gridw.draw_square((8, 8), -1, 2)
    gridw.draw_square((7, 8), -1, 2)
    for clarity in range(7):
        gridw.draw_square((clarity + 1, 6), -1, clarity)
        gridw.draw_square((clarity + 1, 7), +1, clarity)
    '''
    gc = GCanvas.from_data(two_cs)
    ltsoup = LongTermSoup.make_from_canvas(gc)
    gc.blank_all_but(default_seed_addrs)
    m = Model(
        ltsoup,
        WorkingSoup.make_from_canvas(gc),
        gc
    )
    gridw.draw_gc(gc)
    root.mainloop()
