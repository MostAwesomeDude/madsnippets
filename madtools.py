def lazyunnest(iterable):
    """
    Lazily unnest nested iterables of arbitrary depth and length.
    """

    accumulator = []
    for i in iterable:
        if isinstance(i, (tuple, set, list, dict)):
            yield accumulator
            accumulator = []
            for j in lazyunnest(i):
                yield j
        else:
            accumulator.append(i)
    yield accumulator

def unnest(iterable):
    """
    Unnest an iterable of arbitrary depth and length.
    """

    return [i for i in lazyunnest(iterable)]
