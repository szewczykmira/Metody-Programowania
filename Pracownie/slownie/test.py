import os
import sys
import random

def test_currency(currency, begin, end):
    rand = random.randint(1, int(end))
    for num in xrange(int(begin), int(end), rand):
        print num, ":"
        os.system("./slownie %s %s" % (str(num), currency))

test_currency(*sys.argv[1:])
