# hopfield.py -- Simple test of Hopfield update by choosing edges randomly
#                one at a time and not adding edge weights

# Results: Without 'strange' rounding, the partial images lead consistently
# to the full images. 'Strange' rounding, i.e. painting a 1 when the value
# from the edge is 0, results in occasionally going to a wrong image.

from random import randrange, choice


image1 = [1, 1, 1, -1, -1, 1]
image2 = [1, 1, -1, 1, -1, -1]

image1_reversed = [-x for x in image1]
image2_reversed = [-x for x in image2]

edges = [
    (1, 2, 1),    # (node, node, weight)
    (1, 5, -1),
    (2, 5, -1),
    (3, 4, -1),
    (3, 6, 1),
    (4, 6, -1)
]

start1 = [0, 1, 1, 0, 0, 0]
start2 = [0, 1, -1, 0, 0, 0]

canvas = [0] * 6

def one_update(strange=False):
    '''Set strange=True to round off 0 to 1.'''
    echoice = randrange(len(edges))
    dirchoice = randrange(2)
    edge = edges[echoice]
    if dirchoice:
        from_node = edge[0]
        to_node = edge[1]
    else:
        from_node = edge[1]
        to_node = edge[0]
    weight = edge[2]
    raw = canvas[from_node - 1] * weight
    if raw != 0 or strange:
        canvas[to_node - 1] = sgn(raw)
    return echoice, dirchoice, canvas, '*' if raw == 0 else ' '

def sgn(x):
    if x >= 0:
        return 1
    else:
        return -1

def run(start, n=30, strange=False):
    global canvas
    canvas = list(start)
    print(canvas)
    for _ in range(n):
        e, d, c, z = one_update()
        print(e, d, c, z)

    if canvas == image1:
        print('got image1')
    elif canvas == image2:
        print('got image2')
    elif canvas == image1_reversed:
        print('got image1_reversed')
    elif canvas == image2_reversed:
        print('got image2_reversed')
    else:
        print('got no recognized image')


if __name__ == '__main__':
    run(start1, n=50)
