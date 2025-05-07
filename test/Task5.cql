let P = readFile(P.csv)
let Q = readFile(Q.csv)

let R = leftMerge(P, Q, P[1]==Q[1])

output(R)