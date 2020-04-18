let rec a x = x in
let rec b x = a (x + 5) + 1 in
let rec c x = b (x + 4) + 2 in
let rec d x = c (x + 3) + 3 in
let rec e x = d (x + 2) + 4 in
let rec f x = e (x + 1) + 5 in
f 0
