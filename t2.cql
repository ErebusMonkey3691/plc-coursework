let A = readFile(A.csv)

let B = permutation(A, A[0] == A[1] && A[0] != 'Brenda')

output(B[2], B[0])