-- Task 10: Matching Pairs

let S = readFile(S.csv)
let T = readFile(T.csv)

let A = permutation(S, S[0]==S[1])
let B = permutation(T, T[1]==T[2])

let C = cartesian(A,B)

output(C[2],C[3])