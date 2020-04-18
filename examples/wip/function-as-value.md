let twice f x = f (f x) in
let f x = x + 1 in
twice (twice f) 0

