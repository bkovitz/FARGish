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

from RMem import RMem, CanvasAble, BaseValue, Value, ValueTup, make_eqns
from Experiments import RMemCC, RMemSalt
from Log import lo, trace
from util import pr, ps, psa, union, Numeric, as_tuple, short, as_list, \
    newline, force_setattr, sample_without_replacement, first, reseed, \
    instantiate_dataclass_from_kwargs

CanvasMaker = Callable[[], Iterable[CanvasAble]]
CueMaker = Callable[[CanvasAble], CanvasAble]

@dataclass(frozen=True)
class EquationMaker:
    operands: Collection[int]
    operators: Collection[str]

    def __call__(self) -> Iterable[CanvasAble]:
        return make_eqns(operands=self.operands, operators=self.operators)

"""
def partial_canvas(c: Tuple[BaseValue, ...], k: int=3) -> Tuple[BaseValue, ...]:
    r = range(len(c))
    addrs = set(sample_without_replacement(r, k=k))
    return tuple(
        c[a] if a in addrs else None
            for a in r
    )
"""

@dataclass(frozen=True)
class PartialCueMaker:
    '''Returns a cue consisting of a given canvas image with some of its
    cells overwritten with None. 'npartial' is how many cells to retain,
    or, if negative, how many cells to overwrite with None.'''
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

@dataclass(frozen=True)
class TestSpec:
    cls: Type[RMem]
    kwargs: Dict[str, Any]
    initial_canvases_cls: Type[CanvasMaker]
    cue_maker_cls: Type[CueMaker] = PartialCueMaker
    nsamples: Optional[int] = None  # How many initial canvases to test
    n_per_sample: int = 50          # Number of cues per sample
    #npartial: int = 3               # Number of cells to fill in partial cue
    vv: int = 1  # verbosity level
    seed: Optional[int] = None      # random-number seed
    #name: Optional[str] = None      # The name of this TestSpec

    def run(self, vv: Optional[int] = None) -> FidelityTestResult:
        vv: int = self.vv if vv is None else vv
        seed = reseed(self.seed)
        num_tests = 0  # number of tests actually run
        results: Dict[Tuple[BaseValue, ...], int] = defaultdict(int)
        initial_canvases_f = instantiate_dataclass_from_kwargs(
            self.initial_canvases_cls, self.kwargs
        )
        cue_maker = instantiate_dataclass_from_kwargs(
            self.cue_maker_cls, self.kwargs
        )

        # Create RMem and absorb initial canvases
        rmem = instantiate_dataclass_from_kwargs(self.cls, self.kwargs)
        if vv >= 1:
            print(short(rmem))
        initial_canvases: Collection[CanvasAble] = list(initial_canvases_f())
        num_initial_canvases = len(initial_canvases)
        rmem.absorb_canvases(initial_canvases)
        initial_canvases = set(initial_canvases)

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
                got = rmem.run_gset(canvas=cue).as_tuple()
                if vv >= 3:
                    lo('  GOT', got)
                yes = got[-len(canvas):] == canvas
                if yes:
                    results[canvas] += 1
                if vv == 1:
                    print('+' if yes else '.', end='', flush=True)
        duration = perf_counter() - start_time
        
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

    def __str__(self) -> str:
        sio = StringIO()
        print(file=sio)
        print(f'{short(self.rmem):60s}  seed={self.seed}', file=sio)
        #pr(self.tspec.kwargs, file=sio)
        print(self.initial_canvases_f, file=sio)  # type: ignore[misc]
        print(self.cue_maker, file=sio)  # type: ignore[misc]
        print(f'{self.num_correct} / {self.num_tests} ({100 * self.prop_correct:1.2f}%)     {self.duration:8.3f} sec', file=sio)
        return sio.getvalue().rstrip()

if __name__ == '__main__':
    eqns_params = [
        dict(operands=[1], operators=['+']),
        dict(operands=[1, 2], operators=['+']),
        dict(operands=range(1, 7), operators=['+', '-', 'x', '/']),
    ]
    for eqn_ps in eqns_params:
        #for niters in [20, 60, 100]:
        for niters in [150, 200]:
            for cls in RMem, RMemCC, RMemSalt:
                kwargs = dict(niters=niters) | eqn_ps
                tspec = TestSpec(
                    cls=cls,
                    kwargs=kwargs,
                    initial_canvases_cls=EquationMaker
                )
                result = tspec.run()
                print(result)

                    # How to make class-specific arg sets?
                    # call cartesian_product?
