let P = readFile(test/csvs/t5/P3.csv)
let Q = readFile(test/csvs/t5/Q3.csv)

let R = leftMerge(P, Q, P[0]==Q[0])

output(R)