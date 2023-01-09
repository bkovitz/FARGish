# spike2.py -- A quick test of the idea of detecting *everything* all at once
#
# Specifically, this spike tries out running all predicates as detectors,
# on the Cartesian product of all canvas indices × all canvas indices.

from abc import ABC, abstractmethod

from Model import Canvas, I, J, Subst, Variable   # from Model2.py


DetectionResult = bool

class Predicate(ABC):

    @classmethod
    @abstractmethod
    def detect(self, c: Canvas, su: Subst) -> DetectionResult:
        pass


class Apart(Predicate):
    pass


# NEXT Where do we store Apart's parameters? In the class definition or
# in each object?
predicates = [Apart]

if __name__ == '__main__':
    c = Canvas.make_from('ajaqb')
    for i, j in c.all_index_pairs():
        su = Subst.make_from((I, i), (J, j))
        for predicate in predicates:
            if predicate.detect(c, su):
                print(predicate.__name__)
