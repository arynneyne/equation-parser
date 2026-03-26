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
                (a[i] if a.get(i) is not None else 0)
                + (b[i] if b.get(i) is not None else 0),
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
                (a[i] if a.get(i) is not None else 0)
                - (b[i] if b.get(i) is not None else 0),
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


@dataclass
class Equation:
    left: list[Monomial]
    right: list[Monomial] | None = None

    def __post_init__(self):
        if isinstance(self.left, Monomial):
            self.left = [self.left]
        if isinstance(self.right, Monomial):
            self.right = [self.right]

    @property
    def has_right(self) -> bool:
        return self.right is not None

    @property
    def is_eqaul(self) -> bool:
        return self.left == self.right

    def __getitem__(self, key):
        return getattr(self, key)


def num(v: str | float | int) -> int | float:
    return float(v) if "." in str(v) and int(v) != float(v) else int(v)


def parser(equation: str) -> Equation:
    def _parser(expression: str) -> list[Monomial]:
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

        monomials = []
        monomial = ""
        for index, item in enumerate(expression):
            if (
                item not in "+-"
                or expression[index - 1] == "^"
                or monomial == ""
            ):
                monomial += item
            else:
                monomials.append(monomialParser(monomial))
                monomial = "-" if item == "-" else ""
        monomials.append(monomialParser(monomial))
        return monomials

    s = equation.split("=")
    if len(s) == 1:
        return Equation(_parser(s[0]))
    left, right = s
    return Equation(_parser(left), _parser(right))


def tosf(equation: Equation) -> Equation:  # to standard form
    new_equation = Equation([])
    if equation.has_right:
        while equation.right[0].coefficient != 0:
            equation.left.append(equation.right.pop())
            equation.left[-1].coefficient *= -1
            if equation.right == []:
                equation.right = [Monomial(0)]
    for v in set((map(lambda x: tuple(x.variables), equation.left))):
        coef = 0
        vae = list(v)
        for i in filter(lambda x: tuple(x.variables) == v, equation.left):
            coef += i.coefficient
        if coef != 0 or vae == []:
            new_equation.left.append(
                Monomial(
                    coef,
                    sorted(vae, key=lambda x: (-x.exponent, x.name))
                    if vae != []
                    else [],
                )
            )
    new_equation.left.sort(
        key=lambda x: -sum(map(lambda y: y.exponent, x.variables))
    )
    new_equation.right = equation.right
    if new_equation.left == []:
        new_equation.left = [Monomial(0)]
    if new_equation.right == []:
        new_equation.right = [Monomial(0)]
    return new_equation


def toString(equation: Equation | list[Monomial]) -> str:
    res = []

    def _(side: str):
        f = 0
        for i in equation[side] if side != "" else equation:
            """
            coef = i.coefficient
            res.append(str(coef) if coef<0 else ('+' if f else '')+str(coef))
            """
            s = "-" if i.coefficient < 0 else ("+" if f else "")
            coef = str(abs(i.coefficient))
            res.append(s + (coef if coef != "1" or i.variables == [] else ""))
            f = 1
            for v, e in i.variables:
                res.append(v + (f"^{e}" if e != 1 else ""))

    if isinstance(equation, list):
        _("")
        return "".join(res)
    _("left")
    if equation.has_right:
        res.append("=")
        _("right")
    return "".join(res)


def rootSubstitution(
    equation: str | Equation, roots: dict[str, int]
) -> Equation:
    if isinstance(equation, str):
        equation = tosf(parser(equation))

    def _(side: str) -> list:
        res = []
        for i in equation[side]:
            coef = i.coefficient
            vae = []
            for v, e in i.variables:
                if v in roots.keys():
                    coef *= roots[v] ** e
                else:
                    vae.append((v, e))
            res.append(Monomial(coef, vae))
        return res

    left = _("left")
    if equation.has_right:
        right = _("right")
    else:
        right = None
    return Equation(left, right)


def multiplication(a: list[Monomial], b: list[Monomial]) -> list[Monomial]:
    res: list[Monomial] = []
    for i in a:
        for j in b:
            res.append(i * j)
    return res


def division(a: list[Monomial], b: list[Monomial]) -> list[Monomial]:
    res: list[Monomial] = []
    for i in a:
        for j in b:
            res.append(i / j)
    return res


def quadratic(equation: str):
    equation = tosf(parser(equation))
    a, b, c = map(lambda x: x.coefficient, equation.left)
    D = (b**2) - (4 * a * c)
    if D == 0:
        return [-b / 2 / a]
    if D < 0:
        return []
    return [(-b + D**0.5) / a / 2, (-b - D**0.5) / a / 2]
