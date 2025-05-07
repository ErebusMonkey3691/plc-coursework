let P = readFile(P.csv)
let Q = readFile(Q.csv)

let R = leftMerge(P, Q, P[0]==Q[0] )

output(R)