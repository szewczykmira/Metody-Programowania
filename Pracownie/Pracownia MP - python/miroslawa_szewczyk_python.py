# -*- coding: utf-8 -*-
from math import pow

class Tortoise:
    def __init__(self, n, unit=250):
        self.n = n
        self.unit = unit
        self.scalable_unit = 1. / (pow(2,self.n) - 1)

    def move(self, left, right, bottom, up):
        pass
