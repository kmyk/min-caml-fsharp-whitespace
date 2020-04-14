let x = (1, 2, 3) in
let y = ((), x, 4) in
let z = (y, 5, x) in
let (a, b, c) = z in
let (d, e, f) = a in
let (g, h, i) = e in
g + h + i + f + b
