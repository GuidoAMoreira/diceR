#' @include dice-class.R
methods::setClass("diceXtra", contains = "dice",
                  slots = c(gimmick = "character", gimmickPar = "list",
                            other = "any"),
                  validity = function(object) {
  s <- function(name) methods::slot(object, name)
  if (!(s("gimmick") %in% c(
    "Keep largest x", "Drop largest x", "Keep lowest x", "Drop lowest x",
    "Reroll x", "Multiply by x"
  ))) stop("gimmick argument not recognized.")
  if (!(s("gimmick") %in% "Reroll x"))
    if (sum(s("dice_numbers")) < gimmickPar)
      stop(paste("Not enough dice being rolled for", s("gimmick"),
                 "gimmick."))
  if (length(gimmick) != length(gimmickPar))
    stop("gimmicks list must be same size as parameters list.")

  TRUE
})

methods::setMethod("initialize", "diceXtra", function(.Object, original, g, gp,
                                                      o = NULL){
  if (!is.numeric(gp)) stop("Parameter must be a number.")
  if (!any(is(.Object, "dice"), is(.Object, "diceXtra")))
    stop("Object must be a 'dice' or 'diceXtra' object.")
  methods::slot(.Object, "dice_numbers") <- methods::slot(original, "dice_numbers")
  methods::slot(.Object, "dice_sides") <- methods::slot(original, "dice_sides")
  methods::slot(.Object, "bonus") <- methods::slot(original, "bonus")
  if (is(.Object, "diceXtra")) {
    methods::slot(.Object, "gimmick") <-
      c(methods::slot(original, "gimmick"), g)
    methods::slot(.Object, "gimmickPar") <-
      c(methods::slot(original, "gimmickPar"), gp)
  } else {
    methods::slot(.Object, "gimmick") <- g
    methods::slot(.Object, "gimmickPar") <- list(gp)
  }
  methods::slot(.Object, "other") <- o

  # values slot
  ogVal <- methods::slot(.Object, "values")

  # probabilities slot
  # probs <- NULL
  # for (i in seq(length(dn))) {
  #   for (j in seq(dn[i])) {
  #     probs <- addDiceProb(probs, rep(1 / ds[i], ds[i]))
  #   }
  # }
  # methods::slot(.Object, "probabilities") <- probs

  .Object
})

#' @export
keepLargest <- function(original, n = 1L) {
  methods::new("diceXtra", original = original,
               g = "Keep largest x",
               gp = n)}

#' @export
dropLargest <- function(original, n = 1L) {
  methods::new("diceXtra", original = original,
               g = "Drop largest x",
               gp = n)}

#' @export
keepLowest <- function(original, n = 1L) {
  methods::new("diceXtra", original = original,
               g = "Keep lowest x",
               gp = n)}

#' @export
dropLowest <- function(original, n = 1L) {
  methods::new("diceXtra", original = original,
               g = "Drop lowest x",
               gp = n)}

#' @export
reroll <- function(original, results_to_reroll = c(1)) {
  methods::new("diceXtra", original = original,
               g = "Reroll",
               gp = results_to_reroll)}

#' @export
multiply <- function(original, factor = 1, roundFun = floor) {
  methods::new("diceXtra", original = original,
               g = "Multiply by x",
               gp = factor, o = roundFun)}
