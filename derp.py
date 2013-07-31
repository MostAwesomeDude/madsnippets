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


Empty = object()
Epsilon = object()
Exactly = namedtuple("Exactly", "x")
Sequence = namedtuple("Sequence", "first, second")
Union = namedtuple("Union", "first, second")
Repeat = namedtuple("Repeat", "x")


@z
def nullable(f):
    def inner(l):
        if l is Empty:
            return False
        elif l is Epsilon:
            return True
        elif isinstance(l, Exactly):
            return False
        elif isinstance(l, Sequence):
            return f(l.first) and f(l.second)
        elif isinstance(l, Union):
            return f(l.first) or f(l.second)
        elif isinstance(l, Repeat):
            return True
        assert False, "Can't classify %r" % l
    return inner


@z
def derivative(f):
    def inner(l, c):
        if l is Empty or l is Epsilon:
            return Empty
        elif isinstance(l, Exactly):
            if l.x == c:
                return Epsilon
            else:
                return Empty
        elif isinstance(l, Sequence):
            if nullable(l.first):
                return Union(Sequence(f(l.first, c), l.second), f(l.second, c))
            else:
                return Sequence(f(l.first, c), l.second)
        elif isinstance(l, Union):
            return Union(f(l.first, c), f(l.second, c))
        elif isinstance(l, Repeat):
            return Sequence(f(l.x, c), l)
        assert False, "Can't classify %r" % l
    return inner


def matches(l, s):
    for c in s:
        l = derivative(l, c)
    return nullable(l)
