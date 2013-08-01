from collections import namedtuple


def key(args, kwargs):
    return args, frozenset(kwargs.iteritems())


def z(f):
    cache = {}
    def first(x):
        # Name this 'z' so that it's easy to see in repr().
        def z(*args, **kwargs):
            k = key(args, kwargs)
            if k in cache:
                v = cache[k]
            else:
                v = x(x)(*args, **kwargs)
                cache[k] = v
            return v
        return f(z)
    return first(first)


class Named(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name


class Lazy(object):
    value = None

    def __init__(self, f, *args):
        self._thunk = f, args

    def __repr__(self):
        return "%r(%r)" % self._thunk

    def __hash__(self):
        # Hash the only thing that is constant and hashable on this class.
        return hash(id(self))

    def force(self):
        if self.value is None:
            f, args = self._thunk
            self.value = f(*args)


def force(value):
    while isinstance(value, Lazy):
        value.force()
        value = value.value
    return value


Empty = Named("Empty")
Null = namedtuple("Null", "ts")
Exactly = namedtuple("Exactly", "x")
Red = namedtuple("Red", "l, f")
Cat = namedtuple("Cat", "first, second")
Alt = namedtuple("Alt", "first, second")
Rep = namedtuple("Rep", "l")
Delta = namedtuple("Delta", "l")


def const(x):
    return x


@z
def derivative(f):
    def inner(l, c):
        l = force(l)
        if l is Empty or isinstance(l, Null) or isinstance(l, Delta):
            return Empty
        elif isinstance(l, Exactly):
            if l.x == c:
                return Null((c,))
            else:
                return Empty
        elif isinstance(l, Cat):
            # Must be lazy.
            return Alt(
                    Lazy(Cat, Lazy(f, l.first, c), Lazy(const, l.second)),
                    Lazy(Cat, Lazy(Delta, l.first), Lazy(f, l.second, c)),
                )
        elif isinstance(l, Alt):
            # Must be lazy.
            return Alt(Lazy(f, l.first, c), Lazy(f, l.second, c))
        elif isinstance(l, Rep):
            return Red(Cat(f(l.l, c), l), lambda x: (x,))
        elif isinstance(l, Red):
            return Red(f(l.l, c), l.f)
        assert False, "Can't classify %r" % l
    return inner


@z
def compact(f):
    def inner(l):
        l = force(l)
        if isinstance(l, Cat):
            if Empty in l:
                return Empty
            if isinstance(l.first, Null):
                return Red(f(l.second), lambda xs: (l.first.ts, xs))
            if isinstance(l.second, Null):
                return Red(f(l.first), lambda xs: (xs, l.second.ts))
            # Must be lazy.
            return Cat(Lazy(f, l.first), Lazy(f, l.second))
        if isinstance(l, Alt):
            if l.first == Empty:
                return f(l.second)
            elif l.second == Empty:
                return f(l.first)
            # Must be lazy.
            return Alt(Lazy(f, l.first), Lazy(f, l.second))
        if isinstance(l, Rep):
            if l.x is Empty:
                return Null(())
            # Must be lazy.
            return Rep(Lazy(f, l.l))
        if isinstance(l, Red):
            if isinstance(l.l, Null):
                return Null(frozenset([l.f(t) for t in l.l.ts]))
            return Red(f(l.l), l.f)
        if isinstance(l, Delta):
            # Must be lazy.
            return Delta(Lazy(f, l.l))
        return l
    return inner


@z
def trees(f):
    def inner(l):
        l = force(l)
        if l is Empty:
            return set()
        elif isinstance(l, Null):
            return l.ts
        elif isinstance(l, Delta):
            return f(l.l)
        elif isinstance(l, Exactly):
            return set()
        elif isinstance(l, Alt):
            return f(l.first) | f(l.second)
        elif isinstance(l, Cat):
            return set(zip(f(l.first), f(l.second)))
        elif isinstance(l, Red):
            return set([l.f(x) for x in f(l.l)])
        elif isinstance(l, Rep):
            return set()
        assert False, "Can't classify %r" % l
    return inner


def parses(l, s):
    for c in s:
        l = compact(derivative(l, c))
        print l
    return trees(l)


def matches(l, s):
    return bool(parses(l, s))


def string(s):
    if not s:
        return Null(set())
    parser = Exactly(s[0])
    for c in s[1:]:
        parser = Cat(parser, Exactly(c))
    return parser
