# -*- coding: utf-8 -*-
import math
import sys

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

    def reverse(rot, d):
        return {
                "left" : Direction.right,
                "right" : Direction.left,
                "counterclockwise" : Direction.clockwise,
                "clockwise" : Direction.counterclockwise,
                "closer" : Direction.further,
                "further" : Direction.closer
                }[rot.__name__](d)

class Pen:
    def __init__(self, n, pic_size, edge, observer, x, y, z, angle_x, angle_y):
        self.path = [Vector(0,0,0)]
        self.edge = edge #edge / (math.pow(2, n) - 1)
        self.x = x
        self.y = y
        self.z = z
        self.pic_size = pic_size
        self.angle_x = math.radians(angle_x)
        self.angle_y = math.radians(angle_y)

    def last(self):
        return self.path[-1]

    def move(self, direction):
        return self.path.append(self.last().add(direction))

    def header(self, max_x, max_y):
        return  """%IPS-Adobe-2.0 EPSF-2.0
%%BoundingBox: -1 -1 {0} {1}
newpath""".format(max_x, max_y)

    def footer(self):
        return """.4 setlinewidth
stroke
showpage
%%Trailer
%EOF"""

    def print_path(self):
        print(self.header(self.pic_size, self.pic_size))
        x, y, _ = self.project(self.path[0]).vector()
        print(float(x), float(y), "moveto")
        for current in self.path:
            x,y,_ = self.project(current).vector()
            print(float(x), float(y), "lineto")
        print(self.footer())

    def rotate(self, vec):
        rot_x = Direction.roll(vec, self.angle_x)
        rot_y = Direction.pitch(rot_x, self.angle_y)
        return rot_y

    def translate(self, vec):
        x,y,z = vec.vector()
        return Vector(x + self.x, y + self.y, z + self.z)

    def lengthen(self, vec):
        x,y,z = vec.vector()
        return Vector(x * self.edge, y * self.edge, z * self.edge)

    def project(self, vec):
        return self.rotate(self.translate(self.lengthen(vec)))


class Tortoise:
    def __init__(self, pen):
        self.pen = pen
        self.rotations = []

    def local_to_global(self, v):
        for r in self.rotations[::-1]:
            v = Direction.reverse(r, v)
        return v

    def current_direction(self):
        d = Direction.inside()
        return self.local_to_global(d)

    def current_top(self):
        d = Direction.upside()
        return self.local_to_global(d)

    def coords(self):
        return self.current_direction(), self.current_top()

    def forward(self, direction = Direction.inside()):
        d = self.local_to_global(direction)
        self.pen.move(d)

    def rotate(self, rotation):
        self.rotations.append(rotation)

    def back(self, n = 1):
        self.rotations = self.rotations[:len(self.rotations) - n]


class HillbertFractal:
    def __init__(self, n=1, size=500, length=None, observer=10,
            x=0, y=0, z=0, angle1=90, angle2=90):
        if not length:
            length = math.pow(2,n)
        pen = Pen(n, size, float(length), observer, float(x), float(y), float(z), float(angle1), float(angle2))
        self.tortoise = Tortoise(pen)

    
    # Tortoise should end up facing same direction he started
    def draw(self, n, rotate = False):
        if n == 0:
            return

        right = Direction.right if rotate else Direction.left
        left = Direction.left if rotate else Direction.right
        closer = Direction.closer if rotate else Direction.further
        further = Direction.further if rotate else Direction.closer
        clockwise = Direction.clockwise if rotate else Direction.counterclockwise
        counterclockwise = Direction.counterclockwise if rotate else Direction.clockwise

        rightside = Direction.rightside() if rotate else Direction.leftside()
        leftside = Direction.leftside() if rotate else Direction.rightside()
        downside = Direction.downside() if rotate else Direction.upside()
        upside = Direction.upside() if rotate else Direction.downside()
        inside = Direction.inside() if rotate else Direction.outside()
        outside = Direction.outside() if rotate else Direction.inside()

        # Prepare to draw n-1
        self.tortoise.rotate(right)
        self.tortoise.rotate(counterclockwise)
        # I.
        self.draw(n-1, rotate)
        # Rotate back after
        self.tortoise.back(2)

        self.tortoise.forward(upside)

        # Prepare to draw n-1
        self.tortoise.rotate(left)
        self.tortoise.rotate(closer)
        # II.
        self.draw(n-1, rotate)
        # Rotate back after
        self.tortoise.back(2)

        self.tortoise.forward(inside)

        # Prepare to draw n-1
        self.tortoise.rotate(left)
        self.tortoise.rotate(closer)
        # III.
        self.draw(n-1, rotate)
        # Rotate back after
        self.tortoise.back(2)

        self.tortoise.forward(downside)

        # Prepare to draw n-1
        self.tortoise.rotate(right)
        self.tortoise.rotate(right)
        self.tortoise.rotate(clockwise)
        self.tortoise.rotate(clockwise)
        # IV.
        self.draw(n-1, rotate)
        # Rotate back after
        self.tortoise.back(4)

        self.tortoise.forward(rightside)

        # Prepare to draw n-1
        self.tortoise.rotate(right)
        self.tortoise.rotate(right)
        self.tortoise.rotate(clockwise)
        self.tortoise.rotate(clockwise)
        # V.
        self.draw(n-1, rotate)
        # Rotate back after
        self.tortoise.back(4)

        self.tortoise.forward(upside)

        # Prepare to draw n-1
        self.tortoise.rotate(right)
        self.tortoise.rotate(closer)
        # VI.
        self.draw(n-1, rotate)
        # Rotate back after
        self.tortoise.back(2)

        self.tortoise.forward(outside)

        # Prepare to draw n-1
        self.tortoise.rotate(right)
        self.tortoise.rotate(closer)
        # VII.
        self.draw(n-1, rotate)
        # Rotate back after
        self.tortoise.back(2)

        self.tortoise.forward(downside)

        # Prepare to draw n-1
        # VIII.
        self.tortoise.rotate(left)
        self.tortoise.rotate(clockwise)
        self.draw(n-1, rotate)
        # Rotate back after
        self.tortoise.back(2)

        return self

    def path(self):
        return self.tortoise.pen.path


if __name__ == "__main__":
    HillbertFractal(*sys.argv[2:]).draw(int(sys.argv[2])).tortoise.pen.print_path()
