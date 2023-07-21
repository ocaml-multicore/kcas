let iter_factor =
  let factor b = if b then 10 else 1 in
  factor (64 <= Sys.word_size)
  * factor (Sys.backend_type = Native)
  * factor (1 < Domain.recommended_domain_count ())
