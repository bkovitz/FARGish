# OLD Workspace code, deleted 21-Mar-2023. Deleted just after starting to change the
# set of definitions from a dict to a Subst.

@dataclass
class Workspace:
    #subst: Dict[Variable, Any] = field(default_factory=dict)
    subst: Subst = empty_subst
        # a "substitution": a map of variable names to values
    #_tags: Dict[Tag, Set[Variable]] = \
    _tags: Dict[Tag, Set[WorkspaceObj]] = \
        field(default_factory=lambda: defaultdict(set))
        # tag to the objects with that tag   TODO multiple tags, multiple objs
    #_tags_of: Dict[Variable, Set[Tag]] = \
    _tags_of: Dict[WorkspaceObj, Set[Tag]] = \
        field(default_factory=lambda: defaultdict(set))
        # maps each elem to its tags
    #letter_var_counter: int = 0
    var_counters: Dict[str, int] = field(default_factory=lambda: defaultdict(int))
    
    def define(
        self, name: Variable, value: Argument, tag: Union[Tag, List[Tag], None]=None
    ) -> None:
#        for t in as_iter(tag):
#            self._tags[t].add(value)
#            self._tags_of[value].add(t)
        v = self[value]
        if isinstance(v, CompoundWorkspaceObj):
            v = v.replace_constants_with_variables(self)
        self.subst[name] = v
        for t in as_iter(tag):
            self._tags[t].add(v)
            self._tags_of[v].add(t)

#        match value:
#            case _ if is_literal(value):
#                self.subst[name] = self.value
#            case CompoundWorkspaceObj():
#                value = 
#        for k, v in 

#    def define_letter(self, letter: str) -> Variable:
#        while True:
#            self.letter_var_counter += 1
#            name = f'L{self.letter_var_counter}'
#            if not name in self.subst:
#                break
#        self.define(name, letter)
#        return name

    def define_and_name(self, obj: WorkspaceObj) -> Variable:
        name_letter: str
        match obj:
            case str():   # Letter
                name_letter = 'L'
            case Seed():
                name_letter = 'D'
            case int():
                name_letter = 'I'
            case _ if is_type_op(obj):
                name_letter = 'F'
            case Repeat():
                name_letter = 'R'
            case _:
                raise NotImplementedError(obj)
        while True:
            self.var_counters[name_letter] += 1
            name = f'{name_letter}{self.var_counters[name_letter]}'
            if not name in self.subst:
                break
        self.define(name, obj)
        return name

    def undefine(self, name: str) -> None:
        '''It is not an error to undefine an undefined variable.'''
        if name in self.subst:
            del self.subst[name]
        # TODO rm tags

    def tags_of(self, obj: Parameter[CompoundWorkspaceObj]) -> Tags:
        return self._tags_of.get(self[obj], None)

    def find_objects_with_tag(self, tags: Tags) -> Set[CompoundWorkspaceObj]:
        return intersection(
            *(self._tags[t] for t in as_iter(tags))
        )

    def find_object_with_tag(self, tags: Tags) -> CompoundWorkspaceObj:
        # Fizzles if there is not exactly one object with the tags
        result = self.find_objects_with_tag(tags)
        if len(result) == 0:
            raise Fizzle()  # TODO indicate reason for fizzle
        return first(result)

    def extract_tag(self, tagtype: Type[Tag], obj: CompoundWorkspaceObj) \
    -> Optional[Tag]:
        return extract_tag(tagtype, self._tags_of.get(self[obj], None))

    def __getitem__(self, obj: Argument) -> Any:
        '''Returns None if 'obj' is not defined. If 'obj' is defined as
        a variable name, returns the value of that other name.'''
        if is_variable(obj):
            value = self.subst.get(obj, None)
        else:
            value = obj
        if is_variable(value):
            return self[value]
        else:
            return value

#    def eval(self, x: Parameter[ CompoundWorkspaceObj | str | Index]) \
#    -> Parameter[CompoundWorkspaceObj | str | Index]:
    def eval(self, x: Parameter[T]) -> Parameter[T]:
        match x:
            case _ if is_variable(x):
                return self[x]
            case CompoundWorkspaceObj():
                return x.eval(self)  # type: ignore[return-value]
            case _:
                return x
        raise NotImplementedError  # Should never reach this line; mypy bug

    def all_letter_defs(self) -> Dict[Variable, str]:
        return dict(
            (k, v)
                for k, v in self.subst.items()
                    if isinstance(v, str)
        )

    def all_seed_defs(self) -> Dict[Variable, Seed]:
        return dict(
            (k, v)
                for k, v in self.subst.items()
                    if isinstance(v, Seed)
        )

    def all_index_defs(self) -> Dict[Variable, int]:
        return dict(
            (k, v)
                for k, v in self.subst.items()
                    if isinstance(v, int)
        )

    # TODO Merge this into run_painter
    def run_repeater(self, p: Parameter[Repeat]) -> None:
        painter = self.get_repeater(p)  # TODO What about None?
        painter.fill(self)

    def run_painter(self, p: Parameter[Painter]) -> None:
        if isinstance(p, OtherSide):
            p.run(self)

    def run_painter_cluster(
        self, painter_cluster: Parameter[PainterCluster], subst_in: VDict
    ) -> VDict:
        match (pc := self[painter_cluster]):
            case PainterCluster():
                return pc.run(self, subst_in)
            case _:
                raise NotImplementedError  # TODO handle missing PainterCluster
#        #painter_cluster = self.get_painter_cluster(pc)
#        #painter_cluster.run(self, subst)
#        subst: VDict = {}
#        for k,v in subst_in.items():
#            subst[k] = self[v]
#        return subst

    def get_index(self, x: Parameter[Index]) -> Index:
        match x:
            case int():
                return x
            case _:
                return self[x]

    def get_canvas(self, x: Parameter[Canvas]) -> Canvas:
        match x:
            case Canvas():
                return x
            case _:
                return self[x]

    def get_seed(self, x: Parameter[Seed]) -> Seed:
        match x:
            case Seed():
                return x
            case _:
                return self[x]

    def get_op(self, x: Parameter[Type[Op]]) -> Type[Op]:
        match x:
            case type():
                return x
            case _:
                return self[x]

    def get_exception(self, x: OptionalParameter[Exception_]) \
    -> Optional[Exception_]:
        match x:
            case Exception_():
                return x
            case None:
                return None
            case _:
                return self[x]

    def get_repeater(self, x: Parameter[Repeat]) -> Repeat:
        match x:
            case Repeat():
                return x
            case _:
                return self[x]

    def get_painter_cluster(self, x: Parameter[PainterCluster]) \
    -> PainterCluster:
        match x:
            case PainterCluster():
                return x
            case _:
                return self[x]

    def get_letter(self, x: Parameter[str]) -> str:
        if is_variable(x):
            return self[x]
        else:
            return x    # type: ignore[return-value]

    def short(self) -> str:
        return self.__class__.__name__
