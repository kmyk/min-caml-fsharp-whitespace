let rec mult a b =
    if a = 0 then 0
    else b + mult (a - 1) b in
mult 3 7
