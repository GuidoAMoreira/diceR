describeDice <- function(d){
  stopifnot(is(d, "dice"))

  s <- function(name) methods::slot(d, name)
  partial <- paste(apply(cbind(s("dice_numbers"),
                    s("dice_sides")), 1,
              paste, collapse = "d"), collapse = " + ")

  if (s("bonus") > 0)
    paste(partial, "+", s("bonus"))
  else if (s("bonus") < 0)
    paste(partial, "-", abs(s("bonus")))
  else
    partial
}
