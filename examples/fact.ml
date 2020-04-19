let rec mult a b =
    if a = 0 then 0
    else b + mult (a - 1) b in
let rec fact n =
    if n = 0 then 1
    else mult n (fact (n - 1)) in
fact 10
