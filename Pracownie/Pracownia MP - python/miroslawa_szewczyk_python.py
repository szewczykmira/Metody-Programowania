# -*- coding: utf-8 -*-
from math import pow

class Pen:
    def __init__(self, unit=250):
        self.unit = unit
        # self.scalable_unit = 1. / (pow(2,self.n) - 1)
        self.path = [(0,0)]

    def last(self):
        return self.path[-1]

    def move(self, horizontal, vertical):
        return self.path.append(
                (self.last()[0] + horizontal,
                self.last()[1] + vertical))

class Direction:
    def __init__(self):
        self.x = 0
        self.y = 0
    def to_coord(self):
        return (0,0)

    def left(self):
        return self

    def right(self):
        return self

class Up(Direction):
    def to_coord(self):
        return (0,1)

    def left(self):
        return Left()

    def right(self):
        return Right()

class Down(Direction):
    def to_coord(self):
        return (0,-1)

    def left(self):
        return Right()

    def right(self):
        return Left()

class Left(Direction):
    def to_coord(self):
        return (-1,0)

    def left(self):
        return Down()

    def right(self):
        return Up()

class Right(Direction):
    def to_coord(self):
        return (1,0)

    def left(self):
        return Up()

    def right(self):
        return Down()

class Tortoise:
    def __init__(self):
        self.pen = Pen()
        self.direction = Up()

    def forward(self):
        self.pen.move(*self.direction.to_coord())

    def rotate_left(self):
        self.direction = self.direction.left()

    def rotate_right(self):
        self.direction = self.direction.right()

    def rotate(self, direction):
        if direction < 0:
            self.rotate_left()
        else:
            self.rotate_right()

class HillbertFractal:
    def __init__(self):
        self.tortoise = Tortoise()

    # Tortoise faces up
    # Tortoise should end up facing same direction he started
    def draw(self, n, rotation = 1):
        if n == 0:
            return

        self.tortoise.rotate(1 * rotation)
        self.draw(n-1, rotation * -1)
        self.tortoise.rotate(-1 * rotation)
        self.tortoise.forward()
        self.draw(n-1, rotation)
        self.tortoise.rotate(1 * rotation)
        self.tortoise.forward()
        self.tortoise.rotate(-1 * rotation)
        self.draw(n-1, rotation)
        self.tortoise.rotate(-1 * rotation)
        self.tortoise.rotate(-1 * rotation)
        self.tortoise.forward()
        self.tortoise.rotate(1 * rotation)
        self.draw(n-1, rotation * -1)
        self.tortoise.rotate(1 *  rotation)
        return self

    def path(self):
        return self.tortoise.pen.path

def header(max_x, max_y):
    return  """%IPS-Adobe-2.0 EPSF-2.0
%%BoundingBox: -1 -1 {0} {1}
newpath
0.0 0.0 moveto""".format(max_x, max_y)

def footer():
    return """.4 setlinewidth
stroke
showpage
%%Trailer
%EOF"""

def print_path_to_postscript(path):
    max_x = 0
    max_y = 0
    for p in path:
        x, y = p
        if x > max_x:
            max_x = x
        if y > max_y:
            max_y = y
    print(header(max_x + 1, max_y + 1))
    for p in path:
        x, y = p
        print(float(x), float(y), "lineto")
    print(footer())

print_path_to_postscript(HillbertFractal().draw(7).path())
