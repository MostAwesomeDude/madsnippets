from random import randrange
import sys

import numpy
import pygame
import pygame.gfxdraw

def clamp(i, low, high):
    return min(max(i, low), high)

class DnaPoint(object):

    def __init__(self, w, h):
        self.w = w
        self.h = h

        self.x = randrange(w)
        self.y = randrange(h)

    def mutate(self):
        retval = False

        # Point max mutation
        if not randrange(1500):
            self.x = randrange(self.w)
            self.y = randrange(self.h)
            retval = True
        # Point mid mutation
        if not randrange(1500):
            distance = randrange(-20, 20)
            self.x += distance
            self.y += distance
            retval = True
        # Point min mutation
        if not randrange(1500):
            distance = randrange(-3, 3)
            self.x += distance
            self.y += distance
            retval = True

        # Clamp
        self.x = clamp(self.x, 0, self.w)
        self.y = clamp(self.y, 0, self.h)

        return retval

class DnaPolygon(object):

    def __init__(self, w, h):
        self.w = w
        self.h = h

        self.vertices = [DnaPoint(w, h) for i in range(3)]
        self.color = [randrange(256) for i in range(3)]
        self.color.append(randrange(20, 120))

    def add_vertex(self):
        if len(self.vertices) == 10:
            return False

        self.vertices.insert(randrange(len(self.vertices)),
            DnaPoint(self.w, self.h))
        return True

    def remove_vertex(self):
        if len(self.vertices) == 3:
            return False

        self.vertices.pop(randrange(len(self.vertices)))
        return True

    def mutate(self):
        retval = False

        if not randrange(1500):
            self.add_vertex()
            retval = True
        if not randrange(1500):
            self.remove_vertex()
            retval = True
        for index in range(3):
            if not randrange(1500):
                self.color[index] = randrange(256)
                retval = True
        # Alpha is special
        if not randrange(1500):
            self.color[3] = randrange(20, 120)
            retval = True

        for vertex in self.vertices:
            if vertex.mutate():
                retval = True

        return retval

class DnaPolygonList(list):

    def mutate(self):
        retval = False

        if not randrange(700):
            self.insert(randrange(len(self)), DnaPolygon(self.w, self.h))
            retval = True
        if not randrange(1500):
            self.pop(randrange(len(self)))
            retval = True
        if not randrange(1500):
            first, second = randrange(len(self)), randrange(len(self))
            if first != second:
                self[first], self[second] = self[second], self[first]
                retval = True

        for polygon in self:
            if polygon.mutate():
                retval = True

        return retval

    def draw(self):
        surface = pygame.Surface((self.w, self.h), pygame.SRCALPHA, 32)
        for polygon in self:
            points = [(point.x, point.y) for point in polygon.vertices]
            pygame.gfxdraw.filled_polygon(surface, points, polygon.color)
        return surface

def fitness(original, sketch):
    first = pygame.surfarray.pixels3d(original)
    second = pygame.surfarray.pixels3d(sketch)
    difference = first - second
    return numpy.abs(difference).sum()

def draw(original, sketch, width):
    window = pygame.display.get_surface()
    window.blit(sketch, (0, 0))
    window.blit(original, (width, 0))
    pygame.display.flip()

def step(polygons):
    new = DnaPolygonList(polygons)
    new.w = polygons.w
    new.h = polygons.h

    mutated = new.mutate()
    while not mutated:
        mutated = new.mutate()

    return new

def save(polygons):
    import pickle
    pickle.dump(polygons, open("dump.pickle", "wb"))

def main():
    surface = pygame.image.load(sys.argv[1])
    width, height = surface.get_size()
    pygame.display.init()
    pygame.display.set_mode((width * 2, height), pygame.DOUBLEBUF)
    target_surface = surface.convert()

    error = 255 * 255 * 255 * width * height
    generation = 0
    polygons = DnaPolygonList()
    polygons.w = width
    polygons.h = height
    for i in range(15):
        polygons.append(DnaPolygon(width, height))
    poly_surface = polygons.draw()
    draw(surface, poly_surface, width)

    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                save(polygons)
                sys.exit()

        generation += 1
        new = step(polygons)
        poly_surface = polygons.draw()
        new_error = fitness(target_surface, poly_surface)
        if new_error < error:
            error = new_error
            polygons = new
            print "Generation %d, error %d, polygons %d" % (
                generation, error, len(polygons))
            draw(target_surface, poly_surface, width)

if __name__ == "__main__":
    main()
