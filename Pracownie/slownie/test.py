import os
import sys

def test_currency(currency, begin, end):
    currency_file = open("%s.txt" % currency, 'w')
    for num in xrange(int(begin), int(end)):
        os.system("./slownie %s %s" % (str(num), currency))

test_currency(*sys.argv[1:])
