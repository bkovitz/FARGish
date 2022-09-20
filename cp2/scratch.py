
@dataclass(frozen=True)
class AnnotationType:
    name: str

    def __str__(self) -> str:
        return self.name

Anchor = AnnotationType('Anchor')

@dataclass(frozen=True)
class Annotation:
    type: AnnotationType
    name: str

    def __str__(self) -> str:
        return self.name

Start = Annotation(Anchor, 'Start')
End = Annotation(Anchor, 'End')

@dataclass(frozen=True)
class Annotations:
    elems: FrozenSet[Annotation]

    def elems_str(self) -> str:
        return ', '.join(sorted([short(e) for e in self.elems]))

    def __str__(self) -> str:
        return f'Annotations({self.elems_str()})'

@dataclass(frozen=True)
class CellBundle:
    value: CanvasValue
    annotations: Annotations

    def __str__(self) -> str:
        return f'CellBundle({short(self.value)}; {self.annotations.elems_str()})'


CellContent = Union[CanvasValue, Annotation, Annotations, CellBundle]
CellAddr = Union[Index, CellContent]



@dataclass
class ContentAndClarity:
    content: CellContent
    clarity: Numeric

    def paint(self, v: CellContent) -> None:
        # TODO None
        if v is None:
            self.dec_clarity()
        elif v == self.content:
            self.inc_clarity()
        else:
            if self.clarity == 0:
                self.content = v
                self.clarity = 1
            else:
                self.dec_clarity()

    def inc_clarity(self) -> None:
        if self.clarity < MAX_CLARITY:
            self.clarity += 1

    def dec_clarity(self) -> None:
        if self.clarity > 0:
            self.clarity -= 1
        if self.clarity == 0:
            self.content = None

InternalIndex = Union[Index, Tuple[Index, AnnotationType]]

@dataclass
class ContentsAndClarities:
    d: Dict[InternalIndex, ContentAndClarity] = field(default_factory=dict())
    min_index: Index = 1
    max_index: Index = 10

    def __setitem__(self, i: InternalIndex, v: CellContent) -> None:
        if is_index(i):  # WRONG: need to extract index from i
            # check bounds
        if i in self.d:
            self.d[i].paint(v)
        else:
            self.d[i] = ContentAndClarity(v, 1)

    def clarity(self, i) -> Numeric:
        pass # TODO

    def set_clarity(self, i: InternalIndex, clarity: Numeric) -> None:
        pass # TODO

# TODO How do you erase an annotation?



# In Canvas1D:
    def __setitem__(self, i: Index, v: CellContent) -> None:
        for ii, vv in self.as_internal_args(i, v):
            self.d[ii] = vv

    def as_internal_args(self, i: Index, v: CellContent) \
    -> Iterable[Tuple[InternalIndex, Union[CanvasValue, Annotation]]:
        match v:
            case str():
                yield (i, v)
            case Annotation():
                yield ((i, v.type), v)
            case Annotations():
                for elem in v:
                    yield ((i, elem.type), elem)

contents: Dict[Index, ContentsAndClarities]
