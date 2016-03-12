# -*- coding: utf-8 -*-
import math

PREC = 4

class Vector:
    def __init__(self, x=0, y=1, z=0):
        x = float(x)
        y = float(y)
        z = float(z)
        self.x = round(x, PREC)
        self.y = round(y, PREC)
        self.z = round(z, PREC)

    def vector(self):
        return [self.x, self.y, self.z]

    def add(self, other):
        x, y, z = [x + y for (x,y) in zip(self.vector(), other.vector())]
        return Vector(x,y,z)

    def __str__(self):
        return str(self.vector())

    def __repr__(self):
        return self.__str__()


class Direction(Vector):
    def leftside():
        return Direction(-1, 0, 0)

    def rightside():
        return Direction(1, 0, 0)

    def upside():
        return Direction(0, 1, 0)

    def downside():
        return Direction(0, -1, 0)

    def inside():
        return Direction(0, 0, 1)

    def outside():
        return Direction(0, 0, -1)

    def yaw(d, a):
        x = math.cos(a) * d.x - math.sin(a) * d.y
        y = math.sin(a) * d.x + math.cos(a) * d.y
        z = d.z
        return Direction(x,y,z)

    def pitch(d, a):
        x = math.cos(a) * d.x + math.sin(a) * d.z
        y = d.y
        z = -1 * math.sin(a) * d.x + math.cos(a) * d.z
        return Direction(x,y,z)

    def roll(d, a):
        x = d.x
        y = math.cos(a) * d.y - math.sin(a) * d.z
        z = math.sin(a) * d.y - math.cos(a) * d.z
        return Direction(x,y,z)

    def left(d):
        return d.pitch(-math.pi/2)

    def right(d):
        return d.pitch(math.pi/2)

    def counterclockwise(d):
        return d.yaw(math.pi/2)

    def clockwise(d):
        return d.yaw(-math.pi/2)

    def closer(d):
        return d.roll(-math.pi/2)

    def further(d):
        return d.roll(math.pi/2)

class Pen:
    def __init__(self, unit=250):
        self.unit = unit
        # self.scalable_unit = 1. / (pow(2,self.n) - 1)
        self.path = [Vector(0,0,0)]

    def last(self):
        return self.path[-1]

    def move(self, direction):
        return self.path.append(self.last().add(direction))

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
        xs = [x.x for x in self.path]
        ys = [x.y for x in self.path]
        max_xs = max(xs)
        max_ys = max(ys)
        print(self.header(max_xs + 1, max_ys + 1))
        for p in self.path:
            x, y = p
            print(float(x), float(y), "lineto")
        print(self.footer())

    def print_path(self):
        xs = [x.x for x in self.path]
        ys = [x.y for x in self.path]
        print(self.header(max_xs + 1, max_ys +1))
        last = [0,0,0]
        last_real = Vector(0,0,0).vector
        for current in self.paht:
            vec = current.vector
            vector = [vec[0] - last_real[0],
                    vec[1] - last_real[1], vec[2] - last_real[2]]
            last = self.postscript_move(last, vector)
            last_real = vec
        print(self.footer())

    def postscript_move(self, last, vector):
        x, y, z = last
        x1, y1, z1 = vector
        if x is not 0:
            print(float(x) + float(x1), float(y), float(z), "lineto")
            return [x+x1, y, z]
        elif y is not 0:
            print(float(x), float(y) + float(y1), float(z), "lineto")
            return [x, y + y1, z]
        else:



class Tortoise:
    def __init__(self):
        self.pen = Pen()
        self.rotations = []

    def current_direction(self):
        d = Direction.inside()
        for r in self.rotations:
            d = r(d)
        return d

    def forward(self):
        d = self.current_direction()
        print("->", d)
        self.pen.move(d)

    def rotate(self, rotation):
        print(rotation.__name__)
        self.rotations.append(rotation)


class HillbertFractal:
    def __init__(self):
        self.tortoise = Tortoise()

    # Tortoise should end up facing same direction he started
    def draw(self, n):
        if n == 0:
            return

        # Prepare to draw n-1
        self.tortoise.rotate(Direction.right)
        self.tortoise.rotate(Direction.counterclockwise)
        # I.
        self.draw(n-1)
        # Rotate back after
        self.tortoise.rotate(Direction.clockwise)
        self.tortoise.rotate(Direction.left)
        # Rotate to face up
        self.tortoise.rotate(Direction.closer)
        self.tortoise.forward()
        # Prepare to draw n-1
        self.tortoise.rotate(Direction.clockwise)
        # II.
        self.draw(n-1)
        # Rotate back after
        self.tortoise.rotate(Direction.counterclockwise)
        # Rotate to face inside
        self.tortoise.rotate(Direction.further)
        self.tortoise.forward()
        # Prepare to draw n-1
        self.tortoise.rotate(Direction.closer)
        self.tortoise.rotate(Direction.counterclockwise)
        # III.
        self.draw(n-1)
        # Rotate back after
        self.tortoise.rotate(Direction.clockwise)
        self.tortoise.rotate(Direction.further)
        # Rotate to face down
        self.tortoise.rotate(Direction.further)
        self.tortoise.forward()
        # Prepare to draw n-1
        self.tortoise.rotate(Direction.further)
        # IV.
        self.draw(n-1)
        # Rotate back after
        self.tortoise.rotate(Direction.closer)
        # Rotate to face right
        self.tortoise.rotate(Direction.right)
        self.tortoise.forward()
        # Prepare to draw n-1
        self.tortoise.rotate(Direction.further)
        self.tortoise.rotate(Direction.counterclockwise)
        # V.
        self.draw(n-1)
        # Rotate back after
        self.tortoise.rotate(Direction.clockwise)
        self.tortoise.rotate(Direction.closer)
        # Rotate to face up
        self.tortoise.rotate(Direction.right)
        self.tortoise.forward()
        # Prepare to draw n-1
        self.tortoise.rotate(Direction.clockwise)
        # VI.
        self.draw(n-1)
        # Rotate back after
        self.tortoise.rotate(Direction.counterclockwise)
        # Rotate to face outside
        self.tortoise.rotate(Direction.further)
        self.tortoise.forward()
        # Prepare to draw n-1
        self.tortoise.rotate(Direction.closer)
        self.tortoise.rotate(Direction.clockwise)
        # VII.
        self.draw(n-1)
        # Rotate back after
        self.tortoise.rotate(Direction.counterclockwise)
        self.tortoise.rotate(Direction.further)
        # Rotate to face downward
        self.tortoise.rotate(Direction.further)
        self.tortoise.forward()
        # Prepare to draw n-1
        self.tortoise.rotate(Direction.right)
        self.tortoise.rotate(Direction.clockwise)
        self.tortoise.rotate(Direction.clockwise)
        # VIII.
        self.draw(n-1)
        # Rotate back after
        self.tortoise.rotate(Direction.counterclockwise)
        self.tortoise.rotate(Direction.counterclockwise)
        self.tortoise.rotate(Direction.left)
        # Rotate inside
        self.tortoise.rotate(Direction.closer)
        # Rotate face 2x
        self.tortoise.rotate(Direction.counterclockwise)
        self.tortoise.rotate(Direction.counterclockwise)
        return self

    def path(self):
        return self.tortoise.pen.path

print(HillbertFractal().draw(1).path())
