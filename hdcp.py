"""
HDCP MASTER KEY (MIRROR THIS TEXT!)

This is a forty times forty element matrix of fifty-six bit
hexadecimal numbers.

To generate a source key, take a forty-bit number that (in
binary) consists of twenty ones and twenty zeroes; this is
the source KSV.  Add together those twenty rows of the matrix
that correspond to the ones in the KSV (with the lowest bit
in the KSV corresponding to the first row), taking all elements
modulo two to the power of fifty-six; this is the source
private key.

To generate a sink key, do the same, but with the transposed
matrix.
"""

import random

def load_matrix(f):
    """
    Given a file f, load a matrix from it.
    """

    handle = open(f, "r")

    i, j = 0, 0
    matrix = dict()

    for line in handle:
        numbers = [int(x, 16) for x in line.split()]
        if numbers:
            for number in numbers:
                matrix[i, j] = number
                i += 1
        else:
            i = 0
            j += 1

    return matrix

def generate_ksv():
    """
    Return a random KSV and the vectors belonging to it.
    """

    key_vectors = random.sample(range(40), 20)
    key_vectors.sort()
    key = 0
    for vector in key_vectors:
        key |= 2**vector

    return key, key_vectors

def lookup_keys(matrix, vectors):
    """
    Retrieve the keys in the matrix corresponding to the given vectors.
    """

    source, sink = 0, 0

    for vector in vectors:
        for i in range(40):
            source += matrix[i, vector]
            sink += matrix[vector, i]

    source %= 2**56
    sink %= 2**56

    return source, sink

matrix = load_matrix("hdcp.txt")

print "Loaded matrix!"
print "Assuming this matrix is 40x40 with no gaps..."

ksv, vectors = generate_ksv()

print "Created KSV!"
print "KSV: %#010x (Vectors: %s)" % (ksv, vectors)

source, sink = lookup_keys(matrix, vectors)

print "Retrieved keys!"
print "Source key: %#014x Sink key: %#014x" % (source, sink)
