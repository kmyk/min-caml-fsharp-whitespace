let rec a x = x in
let rec b x = a (x + 5) + x in
let rec c x = b (x + 4) + x in
let rec d x = c (x + 3) + x in
let rec e x = d (x + 2) + x in
let rec f x = e (x + 1) + x in
f 0
