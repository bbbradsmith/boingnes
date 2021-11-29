#
# Bounce constant investigation
#

Y_BASE = 210
Y_TOP  = 88
Y_MAX  = 65 # maximum allowed height
TIME = 113 # time to bounce

# simulates given arc with starting VY and gravity AY
# returns (peak,time length)
def arc(vy,ay,trace=False):
    p = 0
    y = 0
    t = 0
    if (trace):
        print("arc(%5d,%5d)" % (vy,ay))
    while (y >= 0):
        if (trace):
            print("%3d: %6d %3d" % (t,vy,Y_BASE-(y//256)))
        y += vy
        p = max(p,y)
        vy -= ay
        t += 1
    return (p,t)

# guess an approximate arc, assuming a perfect parabola
def guess(p,t):
    # approximate as parabola:
    # c*x^2 = y
    # 2*c*x = vy
    # so: c * (t/2)^2 = p
    p = (Y_BASE - p) * 256
    c = p / (t*t/4)
    v = 2 * c * (t/2)
    a = v / (t/2)
    return (v,a)

# prints out test result
def test(vy,ay):
    (p,t) = arc(vy,ay)
    p = Y_BASE - (p//256)
    print("%5d,%5d: %3d %3d" % (vy,ay,p,t))
 

# guessing to meet Y_TOP, then check with a range of tests
(gv,ga) = guess(Y_TOP,TIME)
print("Guess: " + str((gv,ga)))
test_guess = False # turn this on to test
if test_guess:
    for ia in range(-5,6):
        for iv in range(-5,6):
            test(int(gv+iv),int(ga+ia))
else:
    arc(1110,20,True) # VY/AY, best fit found

# keep ay the same but adjust vy to meet Y_MAX
test_max = False
if test_max:
    for ia in range(0,200):
        test(int(1110+ia),20)
else:
    arc(1211,20,True) # VYMAX
