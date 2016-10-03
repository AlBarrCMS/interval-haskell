#!/usr/bin/python2

from random import uniform

deg = 3

poly = ""
for x_pow in range(deg):
    for y_pow in range(deg):
        coeff = uniform(-2, 2)
        if (x_pow == 0):
            x_str = ""
        else:
            x_str = "x^" + str(x_pow)
        if (y_pow == 0):
            y_str = ""
        else:
            y_str = "y^" + str(y_pow)
        poly += str(coeff) + x_str + y_str + " + "

poly = poly[0:-3]
print poly
