let rec quad x =
  let rec dbl x = x + x in
  dbl (dbl x) in
quad 123
