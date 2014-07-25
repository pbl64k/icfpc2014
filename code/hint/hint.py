import sys
import png

def bn(b):
    r = 0
    for x in b:
        r *= 2
        r += x
    return r

fn = './bkg.png'

reader = png.Reader(filename = fn)

pxs = list(reader.read_flat()[2])

#r = {}

dec = {(26, 26, 26): 0, (27, 27, 27): 1}

bits = []

for ix in range(0, len(pxs), 3):
    t = tuple(pxs[ix:ix + 3])
    bits.append(dec[t])
    #if t not in r:
    #    r[t] = 0
    #r[t] += 1
    #print t

lines = []
last = None

for ix in range(0, len(bits), 532):
    ll = bits[ix:ix + 532]
    l = [ll[0]]
    for jx in range(1, len(ll), 10):
        b = ll[jx:jx + 10]
        tt = b[0]
        assert all(map(lambda x: x == tt, b))
        l.append(tt)
    if last != l:
        lines.append(l)
        last = l

print len(lines)

lines0 = lines

lines0 = [[] for ix in range(len(lines))]

for l in lines:
    for l0, c in zip(lines0, l):
        l0.append(c)

#lines0 = [list(reversed(l)) for l in lines]
#lines0.reverse()

for l in lines0:
    print ''.join(map(lambda x: '.' if x == 0 else '#', l))

r = {}

frame = 5

nums = []

for l in lines0:
    s = ''
    for ix in range(0, len(l), frame):
        n = bn(l[ix:ix + frame])
        if n not in r:
            r[n] = 0
        r[n] += 1
        nums.append(n)
        s += ('%4d ' % n)
        #s += chr(ord('a') + n)
    print s

r = {}

for x in zip(nums, nums[1:]):
#for x in zip(nums, nums[1:], nums[2:]):
    if x not in r:
        r[x] = 0
    r[x] += 1

freq = sorted(map(lambda x: (r[x], x), r.keys()), reverse = True)

print len(freq)

for f, c in freq:
    print ('%s: %d' % (str(c), f))

s = ''

ct = {
    13: 't',
    0: 'h',
    8: 'e',
    }

for n in nums:
    s += ct[n] if n in ct else '?'

print s

