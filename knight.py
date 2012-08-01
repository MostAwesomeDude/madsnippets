def all_moves_for(x, y):
    yield x + 1, y - 2
    yield x + 2, y - 1
    yield x + 2, y + 1
    yield x + 1, y + 2
    yield x - 1, y + 2
    yield x - 2, y + 1
    yield x - 2, y - 1
    yield x - 1, y - 2

def move_is_legal(x, y):
    return 0 <= x < 6 and 0 <= y < 5

fixpoints = {
    3:  (5, 2),
    6:  (0, 4),
    10: (5, 1),
    18: (1, 3),
    27: (5, 0),
}

def visit(x, y, visited, position):
    print "Position: %d Move: %d, %d" % (position, x, y)
    status()

    if position == 29:
        print "Success!"
        for i, move in enumerate(visited):
            print "Move %d: %r" % (i + 1, move)
        import sys; sys.exit()

    if position in fixpoints and (x, y) != fixpoints[position]:
        return

    for mx, my in all_moves_for(x, y):
        if move_is_legal(mx, my) and (mx, my) not in visited:
            visit(mx, my, visited + [(mx, my)], position + 1)

def status():
    global count, counts

    if count >= counts[0]:
        print "Status: %d" % count
        counts.append(counts.pop(0) * 10)

counts = [1, 2, 4, 5, 8]
count = 0

visit(0, 0, [], 0)
