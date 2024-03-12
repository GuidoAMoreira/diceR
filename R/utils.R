#' @import methods

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

#' @importFrom stats convolve
addDiceProb <- function(dist1, dist2) {
  if (is.null(dist1)) return(dist2) else if(is.null(dist2)) return(dist1)
  stats::convolve(dist1, rev(dist2), type = "open")
}

calcKeepLargest <- function(dice, x) {
  s <- function(name) methods::slot(dice, name)

  if (x > sum(s("dice_numbers"))) stop("Roll does not have enough dice to keep.")
  orderDice <- sort.int(s("dice_sides"), decreasing = TRUE, index.return = TRUE)


}
