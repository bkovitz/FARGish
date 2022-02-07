# Harness.py -- A "test harness": conveniences for setting up tests of
#               RMem, inspecting what happens, summarizing results, and
#               generating the same results repeatedly

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, final
from collections import defaultdict
from time import perf_counter
from io import StringIO
from random import choice

from RMem import RMem, RMemAbs, CanvasAble, BaseValue, Value, ValueTup, \
    make_eqns
from Log import lo, trace
from util import pr, ps, psa, union, Numeric, as_tuple, short, as_list, \
    newline, force_setattr, sample_without_replacement, first, reseed, \
    instantiate_dataclass_from_kwargs, dict_str, as_dict

CanvasMaker = Callable[[], Iterable[CanvasAble]]
CueMaker = Callable[[CanvasAble], CanvasAble]

@dataclass(frozen=True)
class EquationMaker:
    operands: Collection[int] = range(1, 11)
    operators: Collection[str] = ('+', '-', 'x', '/')

    def __call__(self) -> Iterable[CanvasAble]:
        return make_eqns(operands=self.operands, operators=self.operators)

    def short(self) -> str:
        return dict_str(as_dict(self))

@dataclass(frozen=True)
class PartialCueMaker:
    '''Returns a cue consisting of a given canvas image with some of its
    cells overwritten with None. 'npartial' is how many cells to retain,
    or, if negative, how many cells to overwrite with None. If 'npartial'
    is itself None, then the PartialCueMaker always returns the original
    full image.'''
    npartial: Optional[int] = -2   # negative for 'all but -npartial'

    def __call__(self, full_image: CanvasAble) -> CanvasAble:
        full_image: ValueTup = as_tuple(full_image)
        if self.npartial is None:
            return full_image
        l = len(full_image)
        npartial = l + self.npartial if self.npartial < 0 else self.npartial
        if npartial <= 0:
            return (None,) * l
        if npartial >= l:
            return full_image
        r = range(l)
        addrs = set(sample_without_replacement(r, k=npartial))
        return tuple(
            full_image[a] if a in addrs else None
                for a in r
        )

    def short(self) -> str:
        return dict_str(as_dict(self))


@dataclass(frozen=True)
class TestSpec:
    cls: Type[RMem] = RMemAbs
    kwargs: Dict[str, Any] = field(default_factory=lambda: {})
    initial_canvases_cls: Type[CanvasMaker] = EquationMaker
    cue_maker_cls: Type[CueMaker] = PartialCueMaker
    nsamples: Optional[int] = None  # How many initial canvases to test
    n_per_sample: int = 50          # Number of cues per sample
    vv: int = 1  # verbosity level
    seed: Optional[int] = None      # random-number seed
    #name: Optional[str] = None      # The name of this TestSpec

    def run(self, vv: Optional[int] = None) -> FidelityTestResult:
        vv: int = self.vv if vv is None else vv
        seed = reseed(self.seed)
        num_tests = 0  # number of tests actually run
        results: Dict[Tuple[BaseValue, ...], int] = defaultdict(int)

        rmem, initial_canvases_f, initial_canvases, cue_maker = \
            self.make_setup()
        if vv >= 1:
            print()
            print(
                f'{short(rmem):40}  niters={rmem.niters}  {short(initial_canvases_f)}  {short(cue_maker)}'
            )
        initial_canvases = set(initial_canvases)
        num_initial_canvases = len(initial_canvases)

        # Run the tests
        start_time = perf_counter()
        for canvas in sample_without_replacement(
            initial_canvases, k=self.nsamples
        ):
            if vv >= 2:
                lo(canvas)
            for _ in range(self.n_per_sample):
                num_tests += 1
                cue = cue_maker(canvas)
                if vv >= 3:
                    lo('  CUE', cue)
                got = as_tuple(self.run1(cue, rmem, vv=vv))
                if vv >= 3:
                    lo('  GOT', got)
                yes = got[-len(canvas):] == canvas
                if yes:
                    results[canvas] += 1
                if vv == 1:
                    print('+' if yes else '.', end='', flush=True)
        duration = perf_counter() - start_time
        if vv == 1:
            print(flush=True)
        
        return FidelityTestResult(
            tspec=self,
            rmem=rmem,
            initial_canvases_f=initial_canvases_f,
            cue_maker=cue_maker,
            results=results,  # type: ignore[arg-type]
            duration=duration,
            num_tests=num_tests,
            num_initial_canvases=num_initial_canvases,
            seed=seed
        )

    def run1(
        self,
        cue: Optional[CanvasAble]=None,
        rmem: Optional[RMem]=None,
        target_canvas: Optional[CanvasAble]=None,
        vv: int=4
    ) -> CanvasAble:
        cue_maker: Any = None
        if rmem is None or cue is None:
            rm, _, initial_canvases, cue_maker = self.make_setup()
            if cue is None:
                if target_canvas is None:
                    target_canvas = choice(initial_canvases)
                    '''
                    raise AttributeError(
                        'must specify target_canvas from which to generate cue.'
                    )
                    '''
                cue_maker = self.make_cue_maker()
                cue = cue_maker(target_canvas)
            if rmem is None:
                rmem = rm
        got = rmem.regenerate(canvas=cue).as_tuple()
        if vv >= 4:
            pr(rmem.lsteps)
        return got

    def make_rmem(self) -> RMem:
        return instantiate_dataclass_from_kwargs(self.cls, self.kwargs)

    def make_cue_maker(self) -> CueMaker:
        return instantiate_dataclass_from_kwargs(
            self.cue_maker_cls, self.kwargs
        )

    def make_setup(self) \
    -> Tuple[RMem, CanvasMaker, Sequence[CanvasAble], CueMaker]:
        cue_maker = self.make_cue_maker()
        initial_canvases_f = instantiate_dataclass_from_kwargs(
            self.initial_canvases_cls, self.kwargs
        )
        initial_canvases = list(initial_canvases_f())
        # TODO Rework the interface to TestSpec so we can call
        # RMem.make_instance() here.
        #print('MKSET1', self.kwargs)
        rmem = instantiate_dataclass_from_kwargs(self.cls, self.kwargs)
        #print('MKSET2', as_dict(rmem))
        rmem.absorb_canvases(initial_canvases)  # This can be slow.
        return rmem, initial_canvases_f, initial_canvases, cue_maker

@dataclass(frozen=True)
class FidelityTestResult:
    tspec: TestSpec
    rmem: RMem
    initial_canvases_f: CanvasMaker
    cue_maker: CueMaker
    results: Dict[CanvasAble, int]
    duration: float  # in seconds
    num_tests: int
    num_initial_canvases: int
    seed: int

    @property
    def num_correct(self) -> int:
        return sum(self.results.values())

    @property
    def prop_correct(self) -> float:
        '''Proportion correct.'''
        return self.num_correct / self.num_tests

    @property
    def kwargs(self) -> Dict[str, Any]:
        return self.tspec.kwargs

    def nstr(self) -> str:
        '''Returns a string containing just the principal numerical results.'''
        return f'{self.num_correct:3} / {self.num_tests} ({100 * self.prop_correct:1.2f}%)     {self.duration:8.3f} sec        seed={self.seed}'

    def __str__(self) -> str:
        sio = StringIO()
        print(file=sio)
        print(short(self.rmem), file=sio)
        #pr(self.tspec.kwargs, file=sio)
        print(self.initial_canvases_f, file=sio)  # type: ignore[misc]
        print(self.cue_maker, file=sio)  # type: ignore[misc]
        print(self.nstr(), file=sio)
        return sio.getvalue().rstrip()
