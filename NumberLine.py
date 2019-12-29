# NumberLine.py -- "Coarse" view of some numbers

#TODO "Joggle" width logarithmically
#TODO Notice a big gap between adjacent clusters

from random import choice, random, uniform
from math import exp
from statistics import median, StatisticsError

from util import input_integers, filter_none
import util


class NumberLine:

    def __init__(self, value_func=util.identity):
        '''value_func is function that maps an element of self.numbers to
        its numeric value. util.identity is appropriate if the self.numbers
        are actual numbers. If the self.numbers are nodeids in a graph,
        then you must pass a function that maps a nodeid to a numeric value.
        If value_func(i) returns None, then i will be ignored.'''
        self.value_func = value_func
        self.numbers = set()
        self.eyes = set()

    def add(self, *ns):
        self.numbers.update(ns)

    def clear_numbers(self):
        self.numbers.clear()

    def set_value_func(self, value_func):
        '''value_func: as in __init__().'''
        self.value_func = value_func

    def median(self):
        try:
            return median((self.value_func(n) for n in self.numbers))
        except StatisticsError:
            return None

    def make_eye(self):
        eye = Eye(self)
        self.eyes.add(eye)
        return eye

    def make_eyes(self, num_eyes):
        while len(self.eyes) < num_eyes:
            self.make_eye()

    def reset_eyes(self):
        num_eyes = len(self.eyes)
        self.eyes.clear()
        self.make_eye(num_eyes)

    def looks(self):
        return [eye.look() for eye in self.eyes]

    def joggle_all_eyes(self):
        for eye in self.eyes:
            eye.joggle_width()

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

    def width(self, minimum=1.0):
        '''Returns difference between min and max numbers, or None if the
        NumberLine has no numbers.'''
        if not self.numbers:
            return None
        else:
            ns = list(filter_none(self.value_func, self.numbers))
            if ns:
                return max(max(ns) - min(ns), minimum)
            else:
                return None

    __repr__ = util.nice_object_repr


class Eye:

    def __init__(self, number_line):
        self.number_line = number_line
        self.anchor = self.number_line.choose()
        nl_width = self.number_line.width()
        if nl_width is None:
            self.width = None
        else:
            self.width = uniform(0.05, 0.25) * nl_width
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
            self.width *= 1 + uniform(0.05, 0.25)
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
        if n is None:
            return False
        else:
            return random() <= hump6(
                self.number_line.value_func(n),
                max_at=self.anchor,
                width=self.width
            )

    def __repr__(self):
        if self.anchor is None:
            return 'Eye(anchor=None, width=%1.1f)' % (self.width)
        else:
            return 'Eye(anchor=%1.1f, width=%1.1f)' % (self.anchor, self.width)


def hump(x, max_at=None, width=1.0):
    '''A hump-shaped function equal to 1.0 at max_at, and monotonically
    decreasing in both directions from max_at. max_at defaults to 0.0.'''
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
    flat along the top, due to use of the function x^6. max_at defaults to
    0.0.'''
    if max_at is None:
        max_at = 0.0
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
    if xs:
        return (max(xs) + min(xs)) / 2
    else:
        return None


def test(numbers):
    nl = NumberLine()
    nl.add(*numbers)
    eyes = [nl.make_eye() for i in range(10)]
    looks = [eye.look() for eye in eyes]

    for eye in eyes:
        print(eye, eye.look())


if __name__ == '__main__':
    test([1, 3, 4, 7, 120, 121])
    #test([10, 11, 20, 25, 30, 35])

def run():
    '''A way to try out sets of numbers at the keyboard.'''
    while True:
        ns = input_integers('Numbers: ')
        if not ns:
            break
        test(ns)
