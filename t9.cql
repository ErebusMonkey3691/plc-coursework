-- Task 9: Paths of Length Three

let R = readFile(R.csv)

-- Find links between R2 and R3 first.
let A = cartesian(R,R)
let B = permutation(A, A[1]==A[2])

-- B[0],B[1],B[3] is now a path. (B[1]==B[2])

-- Find another link
let C = cartesian(B,R)
let D = permutation(C, C[3]==C[4])

-- v Shows all path points :D
-- output(D[0],D[1],D[3],D[5])

output(D[0], D[5]) -- Just the start and end
