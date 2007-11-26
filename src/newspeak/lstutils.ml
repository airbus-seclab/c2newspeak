let rec init n f =
  if n <= 0 then []
  else begin
    let tl = init (n-1) f in
      (f n)::tl
  end
