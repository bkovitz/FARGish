# NumberLine.py -- "Coarse" view of some numbers

from random import choice, random, uniform
from math import exp


class NumberLine:

    def __init__(self):
        self.numbers = set()
        self.eyes = set()

    def add(self, *ns):
        self.numbers.update(ns)

    def make_eye(self):
        eye = Eye(self)
        self.eyes.add(eye)
        return eye

    def choose(self):
        '''Returns a number chosen randomly, with uniform distribution,
        or None if the NumberLine has no numbers.'''
        if not self.numbers:
            return None
        else:
            return choice(list(self.numbers))

    def range(self):
        '''Returns a tuple (min, max) of numbers, or None if the NumberLine
        has no numbers.'''
        if not self.numbers:
            return None
        else:
            return (min(self.numbers), max(self.numbers))

    def width(self):
        '''Returns difference between min and max numbers, or None if the
        NumberLine has no numbers.'''
        if not self.numbers:
            return None
        else:
            return max(self.numbers) - min(self.numbers)


class Eye:

    def __init__(self, number_line):
        self.number_line = number_line
        self.anchor = self.number_line.choose()
        nl_width = self.number_line.width()
        if nl_width is None:
            self.width = None
        else:
            self.width = uniform(0.05, 0.15) * nl_width
        self.joggle_width()

    def joggle_width(self):
        #TODO Adjust width to randomly embrace a "cluster" including
        #self.anchor
        if self.anchor is None:
            return
        prev_look = self.look()
        prev_width = self.width
        prev_anchor = self.anchor
        for i in range(8):
            self.anchor = center(prev_look)
            self.width *= 1 + uniform(0.05, 0.15)
            new_look = self.look()
            if len(new_look) < len(prev_look):
                self.width = prev_width
                self.anchor = prev_anchor
                break
            elif len(new_look) == len(prev_look):
                break

    def look(self):
        '''Returns list of numbers randomly chosen from self.number_line, with
        high probability for numbers close to self.anchor and low probability
        for numbers far from self.anchor. The probability is further influenced
        by self.width.'''
        return [n for n in self.number_line.numbers if self.seen(n)]

    def seen(self, n):
        '''Randomly determines if n is "seen".
        TODO self.width should affect the probability.'''
        return random() <= hump6(n, max_at=self.anchor, width=self.width)

    def __repr__(self):
        return 'Eye(anchor=%1.1f, width=%1.1f)' % (self.anchor, self.width)


def hump(x, max_at=0.0, width=1.0):
    '''A hump-shaped function equal to 1.0 at max_at, and monotonically
    decreasing in both directions from max_at.'''
    x = x - max_at
    if abs(width) <= 0.01:
        c = 100.0
    else:
        c = 1 / (width + 1)
    try:
        return 1/exp(c*x*x)
    except OverflowError:
        return 0

def hump6(x, max_at=0.0, width=1.0):
    '''A hump-shaped function equal to 1.0 at max_at, and monotonically
    decreasing in both directions from max_at. This hump function is pretty
    flat along the top, due to use of the function x^6.'''
    x = x - max_at
    if abs(width) <= 0.01:
        c = 100.0
    else:
        c = 1 / (width + 1)
    try:
        return exp(-(x * c) ** 6)
    except OverflowError:
        print('OverflowError', x, c)
        return 0

def center(xs):
    return (max(xs) + min(xs)) / 2


nl = NumberLine()
nl.add(1, 3, 4, 7, 120, 121)
eyes = [nl.make_eye() for i in range(10)]
looks = [eye.look() for eye in eyes]

for eye in eyes:
    print(eye, eye.look())
