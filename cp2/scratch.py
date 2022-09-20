


@dataclass(frozen=True)
class Annotation:
    name: str

    def __str__(self) -> str:
        return self.name

Start = Annotation('Start')
End = Annotation('End')

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


CellContents = Union[CanvasValue, Annotation, Annotations, CellBundle]
CellAddr = Union[Index, CellContents]



@dataclass
class ContentAndClarity:
    content: CellContent
    clarity: Numeric

    def paint(self, v: CellContent) -> None:
        pass # TODO

InternalIndex = Union[Index, Tuple[Index, AnnotationType]]

@dataclass
class ContentsAndClarities:
    d: Dict[InternalIndex, ContentAndClarity]
    min_index: Index
    max_index: Index

    def __setitem__(self, i: InternalIndex, v: CellContent) -> None:
        if is_index(i):
            # check bounds
        if i in self.d:
            self.d[i].paint(v)
        else:
            self.d[i] = ContentAndClarity(v, 1)

    def clarity(self, i) -> Numeric:
        pass # TODO

    def set_clarity(self, i: InternalIndex, clarity: Numeric) -> None:
        pass # TODO


contents: Dict[Index, ContentsAndClarities]
