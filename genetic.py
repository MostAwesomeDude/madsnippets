#!/usr/bin/env python

from collections import defaultdict
from copy import deepcopy
import os
import pickle
from random import randrange
import sys
import time

import numpy
import pygame
import pygame.gfxdraw

def clamp(i, low, high):
    return min(max(i, low), high)

def stats(name, d=defaultdict(int)):
    return
    d[name] += 1
    if not d[name] % 100:
        print "%s mutations: %d" % (name, d[name])

class DnaPoint(object):

    def __init__(self, w, h):
        self.w = w
        self.h = h

        self.x = randrange(w)
        self.y = randrange(h)

    def mutate(self):
        mutations = 0

        # Point max mutation
        if not randrange(1500):
            stats("Point maximal")
            self.x = randrange(self.w)
            self.y = randrange(self.h)
            mutations += 1
        # Point mid mutation
        if not randrange(1500):
            stats("Point medium")
            distance = randrange(-20, 20)
            self.x += distance
            self.y += distance
            mutations += 1
        # Point min mutation
        if not randrange(1500):
            stats("Point minimal")
            distance = randrange(-3, 3)
            self.x += distance
            self.y += distance
            mutations += 1

        # Clamp
        self.x = clamp(self.x, 0, self.w)
        self.y = clamp(self.y, 0, self.h)

        return mutations

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

        stats("New vertex")
        self.vertices.insert(randrange(len(self.vertices)),
            DnaPoint(self.w, self.h))
        return True

    def remove_vertex(self):
        if len(self.vertices) == 3:
            return False

        stats("Dropped vertex")
        self.vertices.pop(randrange(len(self.vertices)))
        return True

    def mutate(self):
        mutations = 0

        if not randrange(1500):
            if self.add_vertex():
                mutations += 1
        if not randrange(1500):
            if self.remove_vertex():
                mutations += 1
        for index in range(3):
            if not randrange(1500):
                stats("Changed color")
                self.color[index] = randrange(256)
                mutations += 1
        # Alpha is special
        if not randrange(1500):
            stats("Changed alpha")
            self.color[3] = randrange(20, 120)
            mutations += 1

        for vertex in self.vertices:
            if vertex.mutate():
                mutations += 1

        return mutations

class DnaPolygonList(list):

    def mutate(self):
        mutations = 0

        if not randrange(200):
            stats("New polygon")
            self.insert(randrange(len(self)), DnaPolygon(self.w, self.h))
            mutations += 1
        if not randrange(1500):
            stats("Dropped polygon")
            self.pop(randrange(len(self)))
            mutations += 1
        if not randrange(700):
            stats("Reordered polygon")
            first, second = randrange(len(self)), randrange(len(self))
            self[first], self[second] = self[second], self[first]
            mutations += 1

        for polygon in self:
            if polygon.mutate():
                mutations += 1

        return mutations

    def draw(self):
        surface = pygame.Surface((self.w, self.h))
        surface.fill((0, 0, 0))
        for polygon in self:
            points = [(point.x, point.y) for point in polygon.vertices]
            pygame.gfxdraw.filled_polygon(surface, points, polygon.color)
            pygame.gfxdraw.aapolygon(surface, points, polygon.color)
        return surface

def fitness(original_array, sketch):
    sketch_array = pygame.surfarray.pixels3d(sketch)
    difference = original_array - sketch_array
    return numpy.square(difference).sum(dtype=numpy.uint64)

def draw(original, polygons, width):
    sketch = polygons.draw()

    window = pygame.display.get_surface()
    window.fill((0, 0, 0))
    window.blit(sketch, (0, 0))
    window.blit(original, (width, 0))
    pygame.display.flip()

iterations = 0

def step(polygons):
    global iterations

    new = deepcopy(polygons)

    mutations = new.mutate()
    iterations += 1
    while not mutations:
        mutations = new.mutate()
        iterations += 1

    return new

def load(filename):
    return pickle.load(open(filename, "rb"))

def save(state, filename):
    pickle.dump(state, open(filename, "wb"))

def main():
    start_time = time.time()
    surface = pygame.image.load(sys.argv[1])
    pickle_name = "%s.pickle" % sys.argv[1]
    width, height = surface.get_size()
    pygame.display.init()
    pygame.display.set_mode((width * 2, height), pygame.DOUBLEBUF)
    target_surface = surface.convert()
    target_array = numpy.cast[numpy.int32](
        pygame.surfarray.array3d(target_surface))

    if os.path.exists(pickle_name):
        generation, error, polygons = load(pickle_name)
    else:
        error = 65536**3 * width * height
        generation = 0
        polygons = DnaPolygonList()
        polygons.w = width
        polygons.h = height
        for i in range(20):
            polygons.append(DnaPolygon(width, height))
    draw(surface, polygons, width)

    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                save((generation, error, polygons), pickle_name)
                pygame.image.save(polygons.draw(), "sketch.%s" % sys.argv[1])
                sys.exit()

        generation += 1
        new = step(polygons)
        new_error = fitness(target_array, new.draw())
        if new_error < error:
            error = new_error
            polygons = new
            draw(target_surface, polygons, width)
        elif generation % 1000:
            continue

        status = "Generation %d, IPS %d, error %d, polygons %d" % (
            generation, iterations / (time.time() - start_time), error,
            len(polygons))
        print status
        pygame.display.set_caption(status)

if __name__ == "__main__":
    main()
