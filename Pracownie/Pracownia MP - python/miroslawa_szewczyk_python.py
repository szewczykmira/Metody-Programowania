# -*- coding: utf-8 -*-
from math import pow

class Pen:
    def __init__(self, unit=250):
        self.unit = unit
        # self.scalable_unit = 1. / (pow(2,self.n) - 1)
        self.path = [(0,0,0)]

    def last(self):
        return self.path[-1]
    
    def move(self,x,y,z):
        return self.path.append((self.last()[0] + x,
            self.last()[1] + y,
            self.last()[2] + z))

    def header(self, max_x, max_y):
        return  """%IPS-Adobe-2.0 EPSF-2.0
%%BoundingBox: -1 -1 {0} {1}
newpath
0.0 0.0 moveto""".format(max_x, max_y)

    def footer(self):
        return """.4 setlinewidth
stroke
showpage
%%Trailer
%EOF"""

    def print_path_to_postscript(self):
        xs = [x[0] for x in self.path]
        ys = [x[1] for x in self.path]
        max_xs = max(xs)
        max_ys = max(ys)
        print(self.header(max_xs + 1, max_ys + 1))
        for p in self.path:
            x, y = p
            print(float(x), float(y), "lineto")
        print(self.footer())


class Up:
    def to_coord(self):
        return (0, 1, 0)

    def left(self):
        return Left()

    def right(self):
        return Right()

    def closer(self):
        return Closer()

    def further(self):
        return Further()


class Down:
    def to_coord(self):
        return (0, -1, 0)

    def left(self):
        return Right()

    def right(self):
        return Left()

    def closer(self):
        return Closer()

    def further(self):
        return Further()


class Left:
    def to_coord(self):
        return (-1, 0, 0)

    def left(self):
        return Down()

    def right(self):
        return Up()

    def closer(self):
        return Closer()

    def further(self):
        return Further()


class Right:
    def to_coord(self):
        return (1, 0, 0)

    def left(self):
        return Up()

    def right(self):
        return Down()

    def closer(self):
        return Closer()

    def further(self):
        return Further()

class Closer:
    def to_coord(self):
        return (0, 0, -1)

    def left(self):
        return Right()

    def right(self):
        return Left()

    def closer(self):
        return Up()

    def further(self):
        return Down()

class Further:
    def to_coord(self):
        return (0, 0, 1)

    def left(self):
        return Left()

    def right(self):
        return Right()

    def closer(self):
        return Down()

    def further(self):
        return Up()


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

    def rotate_further(self):
        self.direction = self.direction.further()
    
    def rotate_closer(self):
        self.direction = self.direction.closer()

    def rotate(self, lr=0, fc=0):
        if lr < 0:
            self.rotate_left()
        elif lr > 0:
            self.rotate_right()

        if fc < 0:
            self.rotate_closer()
        elif fc > 0:
            self.rotate_further()


class HillbertFractal:
    def __init__(self):
        self.tortoise = Tortoise()

    # Tortoise faces up
    # Tortoise should end up facing same direction he started
    def draw(self, n, rotation = 1):
        if n == 0:
            return

        self.tortoise.rotate(lr=(1 * rotation))
        self.draw(n-1, rotation * -1)
        self.tortoise.rotate(lr=(-1 * rotation))
        self.tortoise.forward()
        self.draw(n-1, rotation)
        self.tortoise.rotate(lr=(1 * rotation))
        self.tortoise.forward()
        self.tortoise.rotate(lr=(-1 * rotation))
        self.draw(n-1, rotation)
        self.tortoise.rotate(lr=(-1 * rotation))
        self.tortoise.rotate(lr=(-1 * rotation))
        self.tortoise.forward()
        self.tortoise.rotate(lr=(1 * rotation))
        self.draw(n-1, rotation * -1)
        self.tortoise.rotate(lr=(1 *  rotation))
        return self

    def path(self):
        return self.tortoise.pen.path

    # Tortoise faces up
    # Tortoise should end up facing same direction he started
    def drawn(self, n):
        if n == 0:
            return

        # draw n-1
        # move up
        self.tortoise.forward()

        # draw n-1
        # move further
        self.tortoise.rotate(fc=1)
        self.tortoise.forward()
        # draw n-1
        # move down
        self.tortoise.rotate(fc=1)
        self.tortoise.forward()
        # draw n-1
        # move right
        self.tortoise.rotate(lr=1)
        self.tortoise.forward()
        # draw n-1
        # move closer
        self.tortoise.rotate(fc=-1)
        self.tortoise.forward()
        # draw n-1
        # move right
        self.tortoise.rotate(lr=1)
        self.tortoise.forward()
        # draw n-1
        # move further
        self.tortoise.rotate(fc=1)
        self.tortoise.forward()
        # draw n-1
        return self

print(HillbertFractal().drawn(1).path)
