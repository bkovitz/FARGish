# GridResults.py -- Reproducible experiments on Grid.py

from copy import deepcopy

from Grid import Canvas, two_cs, make_ppainters


def blank4_and_regen():
    c = Canvas.from_data(two_cs)
    orig = deepcopy(c)
    pps = set(make_ppainters(c))
    lpps = list(pps)
    c.blank_random()
    print(c)
    print()
    c.regenerate(pps, niters=100)
    print(c)
    print()
    print(c.claritystr())
    print()
    result = c.levdist(orig)
    print(result)
    return result

if __name__ == '__main__':
    blank4_and_regen()
