import re
from dataclasses import dataclass, field
from typing import Self


@dataclass(frozen=True)
class Variable:
    name: str
    exponent: int | float = 1

    def __post_init__(self):
        object.__setattr__(self, "exponent", num(self.exponent))

    def __iter__(self):
        yield self.name
        yield self.exponent

    @property
    def tup(self):
        return (self.name, self.exponent)


@dataclass
class Monomial:
    coefficient: int | float = 1
    variables: list[Variable] = field(default_factory=list)

    def __post_init__(self):
        self.coefficient = num(self.coefficient)

    def __mul__(self, other: Self | int | float) -> Self:  # multiplication
        if isinstance(other, int | float):
            return Monomial(self.coefficient * other, self.variables)
        coef = self.coefficient * other.coefficient
        a = dict(self.variables)
        b = dict(other.variables)
        vae: list[Variable] = []
        for i in set(list(a.keys()) + list(b.keys())):
            t = Variable(
                i,
                (a[i] if a.get(i) is not None else 0) + (b[i] if b.get(i) is not None else 0),
            )
            if t.exponent != 0:
                vae.append(t)
        return Monomial(coef, vae)

    def __truediv__(self, other: Self | int) -> Self:  # division
        if isinstance(other, int | float):
            return Monomial(self.coefficient / other, self.variables)
        coef = self.coefficient / other.coefficient
        a = dict(self.variables)
        b = dict(other.variables)
        vae = []
        for i in set(list(a.keys()) + list(b.keys())):
            t = Variable(
                i,
                (a[i] if a.get(i) is not None else 0) - (b[i] if b.get(i) is not None else 0),
            )
            if t.exponent != 0:
                vae.append(t)
        return Monomial(coef, vae)


@dataclass
class Expression:
    monomials: list[Monomial]

    def __post_init__(self):
        if isinstance(self.monomials, Monomial):
            self.monomials = [self.monomials]
        if isinstance(self.monomials, int):
            self.monomials = [Monomial(self.monomials)]

    def __getitem__(self, key):
        return self.monomials[key]

    def __mul__(self, other: Self) -> Self:
        res: Expression = Expression()
        for i in self:
            for j in other:
                res.add(i * j)
        return res

    def __truediv__(self, other: Self) -> Self:
        res: Expression = Expression()
        for i in self:
            for j in other:
                res.add(i / j)
        return res

    @property
    def is_empty(self) -> bool:
        return len(self.monomials) == 0

    def add(self, monomial: Monomial):
        self.monomials.append(monomial)


@dataclass
class Equation:
    left: Expression = Expression()
    right: Expression = Expression()

    @property
    def is_eqaul(self) -> bool:
        return self.left == self.right

    def __getitem__(self, key):
        return getattr(self, key)


def num(v: str | float | int) -> int | float:
    return float(v) if "." in str(v) and int(v) != float(v) else int(v)


def parser(equation: str) -> Equation:
    def _parser(expression: str) -> Expression:
        def monomialParser(monomial: str) -> Monomial:
            coef = re.match(r"(-?\d+|-)?", monomial).group()
            coef = -1 if coef == "-" else (1 if coef == "" else int(coef))
            g = re.findall(r"([a-z](\^-?\d+)?)", monomial)
            vae: list[Variable] = []  # variables and exponents
            for i in g:
                t = Variable(*i[0].split("^"))
                if t.exponent != 0:
                    vae.append(t)
            return Monomial(coef, vae)

        monomials = Expression()
        monomial = ""
        for index, item in enumerate(expression):
            if item not in "+-" or expression[index - 1] == "^" or monomial == "":
                monomial += item
            else:
                monomials.add(monomialParser(monomial))
                monomial = "-" if item == "-" else ""
        monomials.add(monomialParser(monomial))
        return monomials

    left, right = equation.split("=")
    return Equation(_parser(left), _parser(right))


def tosf(equation: Equation) -> Equation:  # to standard form
    new_equation = Equation()
    while equation.right[0].coefficient != 0:
        equation.left.add(equation.right.pop())
        equation.left[-1].coefficient *= -1
        if equation.right.is_empty:
            break
    for v in set((map(lambda x: tuple(x.variables), equation.left))):
        coef = 0
        vae = list(v)
        for i in filter(lambda x: tuple(x.variables) == v, equation.left):
            coef += i.coefficient
        if coef != 0 or vae == []:
            new_equation.left.add(
                Monomial(
                    coef,
                    sorted(vae, key=lambda x: (-x.exponent, x.name)) if vae != [] else [],
                )
            )
    new_equation.left.sort(key=lambda x: -sum(map(lambda y: y.exponent, x.variables)))
    new_equation.right = Expression(0)
    if new_equation.left.is_empty:
        new_equation.left = Expression(0)
    return new_equation


def toString(equation: Equation | Expression) -> str:
    res = []

    def _(side: str):
        f = 0
        for i in equation[side] if side != "" else equation:
            s = "-" if i.coefficient < 0 else ("+" if f else "")
            coef = str(abs(i.coefficient))
            res.append(s + (coef if coef != "1" or i.variables == [] else ""))
            f = 1
            for v, e in i.variables:
                res.append(v + (f"^{e}" if e != 1 else ""))

    if isinstance(equation, Expression):
        _("")
        return "".join(res)
    _("left")
    res.append("=")
    _("right")
    return "".join(res)


def rootSubstitution(
    equation: str | Equation | Expression, roots: dict[str, int]
) -> Equation | Expression:
    if isinstance(equation, str):
        equation = tosf(parser(equation))

    def _(side: str) -> list:
        res = Expression()
        for i in equation[side] if side != "" else equation:
            coef = i.coefficient
            vae = []
            for v, e in i.variables:
                if v in roots.keys():
                    coef *= roots[v] ** e
                else:
                    vae.append((v, e))
            res.add(Monomial(coef, vae))
        return res

    if isinstance(equation, Expression):
        return _("")
    return Equation(_("left"), _("right"))


def quadratic(equation: str):
    equation = tosf(parser(equation))
    a, b, c = map(lambda x: x.coefficient, equation.left)
    D = (b**2) - (4 * a * c)
    if D == 0:
        return [-b / 2 / a]
    if D < 0:
        return []
    return [(-b + D**0.5) / a / 2, (-b - D**0.5) / a / 2]
