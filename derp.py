from collections import namedtuple


def key(args, kwargs):
    return args, frozenset(kwargs.iteritems())


def z(f):
    cache = {}
    def first(x):
        def second(*args, **kwargs):
            k = key(args, kwargs)
            if k in cache:
                v = cache[k]
            else:
                v = x(x)(*args, **kwargs)
                cache[k] = v
            return v
        return f(second)
    return first(first)


class Named(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name


Empty = Named("Empty")
Null = namedtuple("Null", "ts")
Exactly = namedtuple("Exactly", "x")
Cat = namedtuple("Cat", "first, second")
Alt = namedtuple("Alt", "first, second")
Rep = namedtuple("Rep", "l")
Red = namedtuple("Red", "l, f")
Delta = namedtuple("Delta", "l")


@z
def derivative(f):
    def inner(l, c):
        if l is Empty or isinstance(l, Null) or isinstance(l, Delta):
            return Empty
        elif isinstance(l, Exactly):
            if l.x == c:
                return Null((c,))
            else:
                return Empty
        elif isinstance(l, Cat):
            return Alt(
                    Cat(f(l.first, c), l.second),
                    Cat(Delta(l.first), f(l.second, c)),
                )
        elif isinstance(l, Alt):
            return Alt(f(l.first, c), f(l.second, c))
        elif isinstance(l, Rep):
            return Red(Cat(f(l.l, c), l), lambda x: (x,))
        elif isinstance(l, Red):
            return Red(f(l.l, c), l.f)
        assert False, "Can't classify %r" % l
    return inner


@z
def compact(f):
    def inner(l):
        if isinstance(l, Cat):
            if Empty in l:
                return Empty
            if isinstance(l.first, Null):
                return Red(f(l.second), lambda xs: (l.first.ts, xs))
            if isinstance(l.second, Null):
                return Red(f(l.first), lambda xs: (xs, l.second.ts))
            return Cat(f(l.first), f(l.second))
        if isinstance(l, Alt):
            if l.first == Empty:
                return f(l.second)
            elif l.second == Empty:
                return f(l.first)
            return Alt(f(l.first), f(l.second))
        if isinstance(l, Rep):
            if l.x is Empty:
                return Null(())
            return Rep(f(l.l))
        if isinstance(l, Red):
            if isinstance(l.l, Null):
                return Null(frozenset([l.f(t) for t in l.l.ts]))
            return Red(f(l.l), l.f)
        if isinstance(l, Delta):
            return Delta(f(l.l))
        return l
    return inner


@z
def trees(f):
    def inner(l):
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
