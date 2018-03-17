assert <- function(cond, because, env = parent.frame())
  if (!cond) stop(interpolate(because, env), call. = FALSE)
