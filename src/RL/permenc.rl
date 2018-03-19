l0: entry
    goto l1
l1: fi k=10 l0 l7
    if k=0  l8 l2
l2: from l1
    k -= 1
    goto l3
l3: fi j=0  l2 l6
    if j=k  l7 l4
l4: from l3
    if x[j]>x[k] l5 l6
l5: from l4
    x[j] -= 1
    goto l6
l6: fi x[j]>=x[k] l5 l4
    j += 1
    goto l3
l7: from l3
    j -= k
    goto l1
l8: from l1
    exit
