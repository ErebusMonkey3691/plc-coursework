-- Task 7: Paired Composition

let F = readFile(F.csv)
let G = readFile(G.csv)

let combined = cartesian(F, G)

-- output a row containing f1, g3 if and only if: 
-- both of f2 and f3 are non-empty, 
-- f2=g1 and f3=g2

-- Column indices in combined: [f1,f2,f3,g1,g2,g3]
let filtered = permutation(combined, 
                           combined[1] != '' && 
                           combined[2] != '' && 
                           combined[1] == combined[3] && 
                           combined[2] == combined[4])

output(filtered[0], filtered[5])