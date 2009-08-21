# Requires Python 2.5 for functools.wrap
#
# If you want to use any of these with decorator.py, just put it in the same
# directory, and then prepend @decorator, e.g.
# @decorator
# class memoize(object):

import functools

class memoize(object):
    """
    Returns a memoized function.

    Memoized functions can cache their results, and will perform a standard
    dict lookup instead of recalculating their results when repeated inputs
    are given.

    If verbose is set to True, then memoize will print some basic statistics
    when destroyed.
    """

    def __init__(self, f, verbose=False):
        self.f = f
        self.verbose = verbose
        self.cache = dict()
        self.hits = 0
        self.misses = 0

    def __call__(self, *args, **kwargs):
        key = (args, frozenset(kwargs.iteritems())) if kwargs else args
        retval = self.cache.get(key)
        if retval:
            self.hits += 1
        else:
            self.misses += 1
            retval = self.f(*args, **kwargs)
            self.cache[key] = retval
        return retval

    def __del__(self):
        if self.verbose:
            print "[memoize] %s: %d%% (%d hits, %d misses)" % ( \
                self.f.func_name, \
                (self.hits * 100.0) / (self.hits + self.misses), \
                self.hits, self.misses)

def trace(f):
    """
    A trivial decorator that prints out trace messages when its function is
    called.
    """

    @functools.wraps(f)
    def deco(*args, **kwargs):
        print "[trace] Calling %s (args %s, kwargs %s)" % (f.__name__,
            args, kwargs)
        retval = f(*args, **kwargs)
        print "[trace] Returning from %s (return %s)" % (f.__name__,
            retval)
        return retval
    return deco

def accepts(*arg_types):
    """
    A debugging decorator that checks arguments and throws TypeError if the
    argument types are incorrect.

    @accepts(str, int)
    def spam(flavor, count):
        ...

    Should be used with @returns.
    """

    def tempfunc(f):
        @functools.wraps(f)
        def deco(*args, **kwargs):
            for i, t in enumerate(args):
                if arg_types[i] != type(args[i]):
                    raise TypeError, "Argument is %s, should be %s" \
                        % (type(args[i]), arg_types[i])
            return f(*args, **kwargs)
        return deco
    return tempfunc

def returns(arg_type=type(None)):
    """
    A debugging decorator that checks return values and throws TypeError if
    the return value type is incorrect.

    @returns(int)
    def spam():
        ...
        return i

    Note that if you wish to indicate no returned value, or None, use
    type(None) to obtain NoneType.

    @returns(type(None))
    def spam():
        ...
        return

    Should be used with @accepts.
    """
    def tempfunc(f):
        @functools.wraps(f)
        def deco(*args, **kwargs):
            retval = f(*args, **kwargs)
            if arg_type != type(retval):
                raise TypeError, "Return value is %s, should be %s" \
                    % (type(retval), arg_type)
            return retval
        return deco
    return tempfunc
