let A = readFile(A.csv)
let B = readFile(B.csv)
let D = readFile(P.csv)

let C = cartesian(A,B,D)

output(C)