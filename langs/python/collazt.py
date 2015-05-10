#!/usr/bin/python2.6
 
lastValidK = 300
prevLengths = dict()
 
 
maxLengthK = None
k = 1
kstack = []
prevLengths[k] = 1
lastCachedK = 1
while lastCachedK <= lastValidK:
    try:
        bl = prevLengths[k]
        al = 1
        while kstack:
            ks = kstack.pop()
            prevLengths[ks] = bl+al
            al += 1;
        k = lastCachedK+1
        lastCachedK = k
        continue
    except:
        pass
    kstack.append(k)
    if k % 2 == 0:
        k = k/2
    else:
        k = 3*k+1
 
 
maxStartNumber = -1
maxLength = -1
 
for i in xrange(1,lastValidK+1):
    if prevLengths[i] > maxLength:
        maxLength = prevLengths[i]
        maxStartNumber = i
 
print(maxStartNumber, maxLength)
 