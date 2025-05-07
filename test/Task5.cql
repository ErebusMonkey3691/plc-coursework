let E = readFile(P.csv)
let A = readFile(Q.csv)

let R = leftMerge(P, Q, P[0]==Q[0] )

output(R)