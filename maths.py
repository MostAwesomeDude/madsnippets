import functools
import itertools
import math

class Continued(object):
    """
    An implementation of continued fractions.

    Continued fractions are a complex and elegant method for representing
    certain numbers. In particular, finite continued fractions are terrific
    for representing rationals, and infinite continued fractions can perfectly
    represent quadratic surds and certain transcendental identities.
    """

    finite = True
    """
    Whether this instance is finite or infinite.

    This is to avoid isinstance() calls on `digits`.
    """

    @classmethod
    def from_int(cls, i):
        instance = cls()
        instance.digitlist = [i]
        return instance

    @classmethod
    def from_rational(cls, numerator, denominator):
        instance = cls()
        while denominator:
            print numerator, denominator
            digit, numerator = divmod(numerator, denominator)
            instance.digitlist.append(digit)
            numerator, denominator = denominator, numerator
        instance.digitlist.append(numerator)
        instance.normalize()
        return instance

    @classmethod
    def e(cls):
        instance = cls()
        instance.finite = False
        def generator():
            yield 2
            i = 2
            mod = 1
            while True:
                mod += 1
                if mod % 3:
                    yield 1
                else:
                    yield i
                    i += 2
                    mod = 0
        instance.make_digits = generator()
        return instance

    @classmethod
    def sqrt(cls, i):
        instance = cls()
        instance.finite = False
        def generator(i):
            l = []
            m = 0
            d = 1
            a = int(math.sqrt(i))
            while (m, d, a) not in l:
                l.append((m, d, a))
                yield a
                m = d * a - m
                d = (i - m**2) / d
                a = int((math.sqrt(i) + m) / d)
            # Extract just the a component (index 2)
            repeating = zip(*l[l.index((m, d, a)):])[2]
            iterator = itertools.cycle(repeating)
            while True:
                yield next(iterator)
        instance.make_digits = generator(i)
        return instance

    def __init__(self):
        self.digitlist = []

    def __repr__(self):
        return str(self)

    def __str__(self):
        if self.finite:
            l = self.digitlist
        else:
            l = list(itertools.islice(self.digits, 10)) + ["..."]
        return "Continued(%s)" % l

    def __add__(self, other):
        return self.combine(other, (0, 1, 1, 0, 1, 0, 0, 0))

    def __sub__(self, other):
        return self.combine(other, (0, 1, -1, 0, 1, 0, 0, 0))

    def __mul__(self, other):
        return self.combine(other, (0, 0, 0, 1, 1, 0, 0, 0))

    def __div__(self, other):
        return self.combine(other, (0, 1, 0, 0, 0, 0, 1, 0))

    def __truediv__(self, other):
        return self.__div__(other)

    @property
    def digits(self):
        """
        Retrieve the digits of this continued fraction lazily.
        """

        if self.finite:
            return self.digitlist
        else:
            self.make_digits, retval = itertools.tee(self.make_digits)
            return retval

    def combine(self, other, initial):
        if isinstance(other, int):
            other = Continued.from_int(other)

        a, b, c, d, e, f, g, h = initial

        if self.finite:
            iterx = itertools.chain(self.digits, itertools.repeat(None))
        else:
            iterx = self.digits()
        if other.finite:
            itery = itertools.chain(other.digits, itertools.repeat(None))
        else:
            itery = other.digits()

        result = Continued()
        result.digits = []
        channel = True
        while any((e, f, g, h)):
            old = a, b, c, d, e, f, g, h
            print old
            ae = a // e if e else None
            bf = b // f if f else None
            cg = c // g if g else None
            dh = d // h if h else None
            if ae == bf and bf == cg and cg == dh:
                r = ae
                print r
                # Output a term.
                a, b, c, d, e, f, g, h = (e, f, g, h,
                    a - e * r, b - f * r, c - g * r, d - h * r)
                result.digits.append(r)
            else:
                # Which input to choose?
                # if None not in (ae, bf, cg) and abs(bf - ae) > abs(cg - ae):
                if channel:
                    # Input from x.
                    p = next(iterx)
                    if p is None:
                        # Infinity: Replicate channels.
                        a, c, e, g = b, d, f, h
                    else:
                        # Ingestion.
                        a, b, c, d, e, f, g, h = (
                            b, a + b * p, d, c + d * p,
                            f, e + f * p, h, g + h * p)
                else:
                    # Input from y.
                    q = next(itery)
                    if q is None:
                        # Infinity: Replicate channels.
                        a, b, e, f = c, d, g, h
                    else:
                        # Ingestion.
                        a, b, c, d, e, f, g, h = (
                            c, d, a + c * q, b + d * q,
                            g, h, e + g * q, f + h * q)
            if old == (a, b, c, d, e, f, g, h):
                channel = not channel
        return result

    def normalize(self):
        if not self.finite:
            raise ValueError, "Can't normalize infinite continued fractions!"
        try:
            while True:
                if self.digitlist[-1] == 1:
                    self.digitlist = self.digitlist[:-1]
                    self.digitlist[-1] += 1
                index = self.digitlist.index(0, 1)
                if index == len(self.digitlist) + 1:
                    self.digitlist = self.digitlist[:-1]
                else:
                    digit = sum(self.digitlist[index - 1:index + 2])
                    self.digitlist = (self.digitlist[:index - 1] + [digit] +
                        self.digitlist[index + 2:])
        except ValueError:
            pass

def gcd(a, b):
    """
    Return the greatest common divisor of a and b.

    >>> gcd(3, 6)
    3
    >>> gcd(19872, 526293)
    9
    """

    if not a:
        return b
    elif not b:
        return a

    while a and b and (a != b):
        if a > b:
            a -= b
        elif b > a:
            b -= a

    return a if a else b

def lcm(*args):
    """
    Return the least common denominator of the inputs.

    >>> lcm(27, 9, 3)
    27
    >>> lcm(2384, 2179)
    5194736
    """

    numbers = set(int(i) for i in args)

    if not numbers:
        return 0

    numbers.discard(0)

    while len(numbers) > 1:
        first = numbers.pop()
        second = numbers.pop()

        numbers.add((first * second)/gcd(first, second))

    return numbers.pop()

def factor(x):
    """
    Returns a list of factors of x, excluding 1.

    >>> factor(12)
    [2, 2, 3]
    >>> factor(127)
    [127]
    >>> factor(720)
    [2, 2, 2, 2, 3, 3, 5]
    """

    return list(lazyfactor(x))

def lazyfactor(x):
    """
    Returns a generator of factors of x, excluding 1.
    """

    while x > 1:
        prime = True
        for i in xrange(2, long(math.sqrt(x)) + 1):
            if not x % i:
                yield i
                x /= i
                prime = False
                break
        if prime:
            yield x
            break

def mma(old, new, weight):
    """
    Performs a Moving Modified Average, using the old value, new value,
    and a weight.

    Weight must be greater than zero.
    """

    return ((weight - 1) * old + new) / weight
