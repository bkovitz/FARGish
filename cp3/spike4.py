
@dataclass(frozen=True)
class GivenGive:
    given: Arguments
    give: Arguments

    def go(self, su: Subst) -> Subst:
        pass


class Apart:
    anchors = [LeftIndex, RightIndex]

    def c1(self, ws: Workspace, i: LeftIndex, j: RightIndex) -> bool:
        return (
            j - i == 2
            and
            ws.has_index(i) and ws.has_index(j)
        )

    def c2(self, ws: Workspace, i: LeftIndex) -> Optional[RightIndex]:
        j: RightIndex = i + 2
        if ws.has_index(j):
            return j
        else:
            return None

    def c3(self, ws: Workspace, j: RightIndex) -> Optional[LeftIndex]:
        i: LeftIndex = j - 2
        if ws.has_index(i):
            return i
        else:
            return None

    def c4(self, ws: Workspace, i: LeftIndex, j: RightIndex) \
    -> Info[Span]:
        return Info(Span(i, j))

class Same:
    anchors = [AtLeftIndex, AtRightIndex]

    def c1(self, ws: Workspace, at_i: AtLeftIndex, at_j: AtRightIndex) -> bool:
        return at_i == at_j

    def c2(self, ws: Workspace, at_i: AtLeftIndex) -> Optional[AtRightIndex]:
        return at_i

    def c3(self, ws: Workspace, at_j: AtRightIndex) -> Optional[AtLeftIndex]:
        return at_j


'''

p1 = ws.make_painter_from([Apart, Same], [(I, 1), (J, 3)])
p2 = ws.make_painter_from(
    [Inside, FilledWith],
    [(P, p1), (K, 2), (L, Letter('j'))]
)

a____
Loop I through all indices but last:
p1.complete(ws, Subst.make_from([(I, 1)]) should= Paint(3, Letter('a'))
    Same can build from I but needs a J
    Apart, given I, can make a J
    Same, given @I, can make @J
Now put p1 into ws.
Loop P through all painters:
p2.complete(ws, Subst.make_from([(P, p1)])) should= Paint(2, Letter('j'))
    FilledWith can build @K but needs a K
    Inside, given P, can make a K
    FilledWith, given K, can make @K
Now put p2 into ws.
Loop P through all painters:
p5.complete(ws, Subst.make_from([(P, p2)])) should= MakePainter(p4, ...)
    SameSpatialRelation given P, can make Q.spatial_relation
    ConsecutiveSpans given P, can make Q  (but it will lack a span)
Incomplete[p4'] needs a span
    p3.Apart can make a span and p4' needs a p3 mate
    Apart, to make a span, needs I,J
    ConsecutiveSpans can judge whether p2.I,J and p4'.I,J are consecutive
        ^ This is tricky. We need to make fresh variables I1,J1,I2,J2 to
          pass to ConsecutiveSpans to check every possibility.

"What can you make from an X?"
    We call this while painting, or seeing if you can paint.
"Who can make a Y?"
    We call this while painting or completing, once some part of painting
    or completing has begun. If one predicate has produced something, and
    another predicate needs something, then we call this.
"Who can judge X,Y?"
    If you can judge X,Y, then you can produce a valid X,Y combination if we
    pass you all the possible X,Ys.
A condition: every predicate within a painter must contribute something before
the painter can act.

Usually if one predicate produced something, we ask only other predicates of
the same painter if they can use it.

Are there times when we want one painter to supply something that another
painter needs? Possibly yes: when we are constructing or completing painters.
If a painter P<pA needs something that pB can provide, then we might try
building Q<pB as a stage in completing P. E.g. when p4' needs a span, we
build p3' to provide it. Perhaps if one painter needs a snippet that another
painter can paint, we build the other painter and let it paint the snippet.

Apart must have the ability to make a span, but building a span doesn't
count as painting. Perhaps name these 'informational' methods differently,
or wrap their return value in a different type. Completion[RightIndex] vs.
Info[Span].


So, we have multiple types of completion elements:

1. ActionElements, which help specify an Action that will alter the Workspace,
i.e. will alter the Canvas or the set of Painters.

2. InfoElements, which help choose a method or supply arguments to a method
that will produce an ActionElement or another InfoElement.

3. Actions are themselves completions of Painters.


Are there "primal" Painters, to generate new painters from already complete
relations noticed in the Workspace? How would we specify one? If a compound
relation has been spotted in one place, it should be easier to spot it in
another place--suggesting that perhaps the model should make new "primal"
painters dynamically.

Given I,J,Predicate, give Detection.
Given two Detections, give Painter.

Can we make new Predicates dynamically? Apart(D=3)?
