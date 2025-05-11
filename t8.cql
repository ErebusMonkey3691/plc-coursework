-- Task 8: Right Merge on Last Column

let P = readFile(P.csv)
let Q = readFile(Q.csv)
let R = leftMerge(Q, P, Q[3] == P[3])
output(R)





