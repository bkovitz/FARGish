# Tags.py

from dataclasses import dataclass, field, replace
from Tag import TagConjunction, HasAvail
from Canvas import ArithmeticToHere


@dataclass(frozen=True)
class SuccessfulCanvas(TagConjunction):

    def __init__(self, target: int):
        super().__init__((HasAvail(target), ArithmeticToHere))
