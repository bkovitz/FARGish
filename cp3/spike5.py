
@dataclass(frozen=True)
class GivenGive:
    given: Arguments
    give: Arguments

    def go(self, su: Subst) -> Subst:
        pass
