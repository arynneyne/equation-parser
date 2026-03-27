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

    def __truediv__(self, other: Self | int | float) -> Self:  # division
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

    def __mod__(self, other: Self | int | float) -> Self:
        if isinstance(other, int | float):
            return Monomial(self.coefficient % other, self.variables)
        coef = self.coefficient % other.coefficient
        a = dict(self.variables)
        b = dict(other.variables)
        vae = []
        for i in set(list(a.keys()) + list(b.keys())):
            t = Variable(
                i,
                (b[i] if b.get(i) is not None else 0) - (a[i] if a.get(i) is not None else 0),
            )
            if t.exponent > 0:
                vae.append(t)
        return Monomial((coef if coef != 1 or vae == [] else 1), vae)

    @property
    def string(self) -> str:
        res = []

        res.append(
            str(self.coefficient)
            if abs(self.coefficient) != 1 or self.variables == []
            else ("-" if self.coefficient < 0 else "")
        )
        for v, e in self.variables:
            res.append(v + (f"^{e}" if e != 1 else ""))
        return "".join(res)

    @property
    def sorted(self) -> Self:
        return Monomial(
            self.coefficient, sorted(self.variables, key=lambda x: (-x.exponent, x.name))
        )

    def parse(monomial: str) -> Self:
        coef = re.match(r"(-?\d+|-)?", monomial).group()
        coef = -1 if coef == "-" else (1 if coef == "" else int(coef))
        g = re.findall(r"([a-z](\^-?\d+)?)", monomial)
        vae: list[Variable] = []  # variables and exponents
        for i in g:
            t = Variable(*i[0].split("^"))
            if t.exponent != 0:
                vae.append(t)
        return Monomial(coef, vae)


@dataclass
class Expression:
    monomials: list[Monomial] = field(default_factory=list)

    def __post_init__(self):
        if isinstance(self.monomials, Monomial):
            self.monomials = [self.monomials]
        if isinstance(self.monomials, int):
            self.monomials = [Monomial(self.monomials)]

    def __getitem__(self, key):
        return self.monomials[key]

    def __mul__(self, other: Self | Monomial) -> Self:
        res: Expression = Expression()
        if isinstance(other, Monomial):
            other = Expression(other)
        for i in self.sf:
            for j in other.sf:
                res.add(i * j)
        return res

    def __truediv__(self, other: Self) -> list[Self]:
        res: Expression = Expression()
        a = self.copy()
        b = other.copy()
        while a[0] % b[0] == Monomial(0):
            res.add(a[0] / b[0])
            a -= b * res[-1]
        return res, a

    def __floordiv__(self, other: Self) -> Self:
        return self.__truediv__(other)[0]

    def __mod__(self, other: Self) -> Self:
        return self.__truediv__(other)[1]

    def __add__(self, other: Self) -> Self:
        return Expression([*self, *other]).sf

    def __sub__(self, other: Self) -> Self:
        return Expression([*self, *(-other)]).sf

    def __neg__(self) -> Self:
        return Expression(list(map(lambda x: x * -1, self)))

    @property
    def string(self) -> str:
        res = []
        f = 1
        for m in self.monomials:
            res.append(("" if m.coefficient < 0 or f else "+") + m.string)
            f = 0
        return "".join(res)

    @property
    def is_empty(self) -> bool:
        return len(self.monomials) == 0

    @property
    def sf(self) -> Self:  # standard form
        res = Expression()
        for v in set((map(lambda x: tuple(x.variables), self))):
            coef = 0
            vae = list(v)
            for i in filter(lambda x: tuple(x.variables) == v, self):
                coef += i.coefficient
            if coef != 0 or vae == []:
                res.add(
                    Monomial(
                        coef,
                        vae,
                    ).sorted
                )
        res.sort(key=lambda x: -sum(map(lambda y: y.exponent, x.variables)))
        if res.is_empty:
            res = Expression(0)
        return res

    def add(self, monomial: Monomial):
        self.monomials.append(monomial)

    def sort(self, key):
        self.monomials.sort(key=key)

    def pop(self) -> Monomial:
        return self.monomials.pop()

    def copy(self) -> Self:
        return Expression(self.monomials)

    def parse(expression: str) -> Self:
        monomials = Expression()
        monomial = ""
        for index, item in enumerate(expression):
            if item not in "+-" or expression[index - 1] == "^" or monomial == "":
                monomial += item
            else:
                monomials.add(Monomial.parse(monomial))
                monomial = "-" if item == "-" else ""
        monomials.add(Monomial.parse(monomial))
        return monomials

    def substitution(self, roots: dict[str, int]) -> Self:
        res = Expression()
        for i in self.monomials:
            coef = i.coefficient
            vae = []
            for v, e in i.variables:
                if v in roots.keys():
                    coef *= roots[v] ** e
                else:
                    vae.append((v, e))
            res.add(Monomial(coef, vae))
        return res


@dataclass
class Equation:
    left: Expression = field(default_factory=Expression)
    right: Expression = field(default_factory=Expression)

    def __getitem__(self, key):
        return getattr(self, key)

    @property
    def is_eqaul(self) -> bool:
        return self.left.sf == self.right.sf

    @property
    def sf(self) -> Self:
        right = self.right.copy()
        left = self.left.copy()
        while right[0].coefficient != 0:
            left.add(right.pop())
            left[-1].coefficient *= -1
            if right.is_empty:
                break
        return Equation(self.left.sf, self.right.sf)

    @property
    def string(self) -> str:
        return self.left.string + "=" + self.right.string

    def parse(equation: str) -> Self:
        left, right = equation.split("=")
        return Equation(Expression.parse(left), Expression.parse(right))

    def substitution(self, roots: dict[str, int]) -> Self:
        return Equation(self.left.substitution(roots), self.right.substitution(roots))


def num(v: str | float | int) -> int | float:
    return int(v) if float(v).is_integer() else float(v)


def quadratic(equation: str):
    expression = Equation.parse(equation).sf.left
    a, b, c = map(lambda x: x.coefficient, expression)
    D = (b**2) - (4 * a * c)
    if D == 0:
        return [-b / 2 / a]
    if D < 0:
        return []
    return [(-b + D**0.5) / a / 2, (-b - D**0.5) / a / 2]
