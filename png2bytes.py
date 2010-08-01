#!/usr/bin/env python

import serial
import sys

import Image

image = Image.open(sys.argv[1])

s = serial.Serial(sys.argv[2])

l = []

for y in range(32):
    for x in range(128):
        pixel = image.getpixel((x, y))
        l.append(1 if sum(pixel) > 255 else 0)

commands = []
low, high = 0, 0
for i in range(len(l) / 8):
    row, i = divmod(i, 128)
    i |= row * 128 * 8
    x = (
        l[i + 128 * 7] << 7 |
        l[i + 128 * 6] << 6 |
        l[i + 128 * 5] << 5 |
        l[i + 128 * 4] << 4 |
        l[i + 128 * 3] << 3 |
        l[i + 128 * 2] << 2 |
        l[i + 128 * 1] << 1 |
        l[i + 128 * 0])
    commands.append("L" + chr(high) + chr(low) + chr(x))
    low += 1
    if low > 255:
        low = 0
        high += 1

commands.append("U")

for command in commands:
    s.write(command)
    if s.read() == ".":
        print "Sent command..."
