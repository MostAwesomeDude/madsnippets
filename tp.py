from itertools import product

class Box:

    def __init__(self, (x, y)):
        self.x = x
        self.y = y

    def __repr__(self):
        return "<Box (%d, %d)>" % (self.x, self.y)

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def loc(self):
        return self.x, self.y

    def left(self):
        return self.x - 1, self.y

    def right(self):
        return self.x + 1, self.y

    def up(self):
        return self.x, self.y - 1

    def down(self):
        return self.x, self.y + 1

    def neighbors(self):
        return self.left(), self.right(), self.up(), self.down()

grid = {t: Box(t) for t in product(range(5), range(6))}
del grid[2, 0]
del grid[0, 3]
del grid[4, 3]
del grid[0, 4]
del grid[4, 4]
del grid[0, 5]
del grid[1, 5]
del grid[3, 5]
del grid[4, 5]

def step(w, g1, g2):
    if w.left() in grid:
        if w.left() not in (g1.loc(), g2.loc(), g2.right()):
            wn = Box(w.left())
            g1n = Box(g1.left()) if g1.left() in grid else g1
            g2n = Box(g2.right()) if g2.right() in grid else g2
            yield wn, g1n, g2n
    if w.right() in grid:
        if w.right() not in (g1.loc(), g2.loc(), g2.left()):
            wn = Box(w.right())
            g1n = Box(g1.right()) if g1.right() in grid else g1
            g2n = Box(g2.left()) if g2.left() in grid else g2
            yield wn, g1n, g2n
    if w.up() in grid:
        if w.up() not in (g1.loc(), g2.loc(), g2.down()):
            wn = Box(w.up())
            g1n = Box(g1.up()) if g1.up() in grid else g1
            g2n = Box(g2.down()) if g2.down() in grid else g2
            yield wn, g1n, g2n
    if w.down() in grid:
        if w.down() not in (g1.loc(), g2.loc(), g2.up()):
            wn = Box(w.down())
            g1n = Box(g1.down()) if g1.down() in grid else g1
            g2n = Box(g2.up()) if g2.up() in grid else g2
            yield wn, g1n, g2n

wolf = Box((2, 3))
golem1 = Box((2, 5))
golem2 = Box((2, 1))

def win(g1, g2):
    return ((g1.loc() == (1, 1) and g2.loc() == (3, 1))
        or (g1.loc() == (3, 1) and g2.loc() == (1, 1)))

stacks = [[(wolf, golem1, golem2)]]
count = 0
winner = None
while winner is None:
    count += 1
    print "Considering moveset", count
    newstacks = []
    for stack in stacks:
        for move in step(*stack[-1]):
            s = list(stack)
            if move in s:
                # print "Dropping recursive stack"
                continue
            s.append(move)
            if win(move[1], move[2]):
                winner = s
                break
            newstacks.append(s)
    stacks = newstacks

def d(b1, b2):
    if b1.x < b2.x:
        return "left"
    elif b1.x > b2.x:
        return "right"
    elif b1.y < b2.y:
        return "up"
    else:
        return "down"

print "Found a winner!"
print "Winner:", winner
previous = None
for i, entry in enumerate(winner):
    if previous:
        print "Move", i, ":", entry, d(entry[0], previous[0])
    else:
        print "Move", i, ":", entry
    previous = entry
