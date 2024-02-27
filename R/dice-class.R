#### Class basics ####
methods::setClass("dice", methods::representation(
  dice_numbers = "numeric", dice_sides = "numeric", bonus = "numeric",
  values = "numeric", probabilities = "numeric"
), validity = function(object) {
  s <- function(name) methods::slot(object, name)
  if (length(s("dice_numbers")) !=
      length(s("dice_sides")))
    stop("dice numbers and sides must be the same size.")
  if (length(s("values")) !=
      length(s("probabilities")))
    stop("values and probabilities must be the same size.")
  if (!all(s("dice_numbers")))
    stop("dice numbers must be positive.")
  if (!all(s("dice_sides")))
    stop("dice sides must be positive.")
  TRUE
})

methods::setMethod("initialize", "dice", function(.Object, dn, ds, b){
  methods::slot(.Object, "dice_numbers") <- as.integer(dn)
  methods::slot(.Object, "dice_sides") <- as.integer(ds)
  methods::slot(.Object, "bonus") <- b

  # values slot
  minRoll = sum(dn) + b
  maxRoll = sum(dn * ds) + b
  v <- minRoll:maxRoll
  methods::slot(.Object, "values") = v

  # probabilities slot
  probs <- NULL
  for (i in seq(length(dn))) {
    for (j in seq(dn[i])) {
      probs <- addDiceProb(probs, rep(1 / ds[i], ds[i]))
    }
  }
  methods::slot(.Object, "probabilities") <- probs

  .Object
})

dice <- function(sides = 6) methods::new("dice", dn = 1, ds = sides, b = 0)

`%d%` <- function(n, s) n * dice(s)

#### Arithmetic ####
methods::setMethod("+", methods::signature(e1 = "dice", e2 = "numeric"),
                   function(e1,e2){
                     s <- function(name) methods::slot(e1, name)
                     methods::new("dice",
                                  dn = s("dice_numbers"),
                                  ds = s("dice_sides"),
                                  b = s("bonus") + e2)
                   })

methods::setMethod("+", methods::signature(e1 = "numeric", e2 = "dice"),
                   function(e1, e2) e2 + e1)

methods::setMethod("-", methods::signature(e1 = "dice", e2 = "numeric"),
                   function(e1,e2){
                     s <- function(name) methods::slot(e1, name)
                     methods::new("dice",
                                  dn = s("dice_numbers"),
                                  ds = s("dice_sides"),
                                  b = s("bonus") - e2)
                   })

methods::setMethod("-", methods::signature(e1 = "numeric", e2 = "dice"),
                   function(e1, e2) stop("Cannot subtract dice from number."))

methods::setMethod("*", methods::signature(e1 = "dice", e2 = "numeric"),
                   function(e1,e2){
                     s <- function(name) methods::slot(e1, name)
                     if (length(s("dice_numbers")) != 1 | s("dice_numbers") != 1)
                       stop("Can only multiply 1 die.")
                     methods::new("dice",
                                  dn = s("dice_numbers") * e2,
                                  ds = s("dice_sides"),
                                  b = s("bonus"))
                   })

methods::setMethod("*", methods::signature(e1 = "numeric", e2 = "dice"),
                   function(e1, e2) e2 * e1)

methods::setMethod("+", methods::signature(e1 = "dice", e2 = "dice"),
                   function(e1, e2){
                     s1 <- function(name) methods::slot(e1, name)
                     s2 <- function(name) methods::slot(e2, name)
                     findingNumbers <- function(sides, numbers, side)
                       ifelse(side %in% sides, numbers[sides == side], 0)

                     sides <- unique(sort(c(s1("dice_sides"), s2("dice_sides"))))
                     numbers <- rep(0, length(sides))
                     for (i in 1:length(sides))
                       numbers[i] <- findingNumbers(s1("dice_sides"),
                                                    s1("dice_numbers"),
                                                    sides[i]) +
                                     findingNumbers(s2("dice_sides"),
                                                    s2("dice_numbers"),
                                                    sides[i])
                     methods::new("dice",
                                  dn = numbers,
                                  ds = sides,
                                  b = s1("bonus") + s2("bonus"))
                   })

#### Comparisons ####

methods::setMethod("==", methods::signature(e1 = "dice", e2 = "dice"),
                   function(e1, e2){
                     s1 <- function(name) methods::slot(e1, name)
                     s2 <- function(name) methods::slot(e2, name)
                     prob <- 0
                     for (i in 1:length(s1("values")))
                       prob <- prob + s1("probabilities")[i] *
                        ifelse(s1("values")[i] %in% s2("values"),
                               s2("probabilities")[s2("values") == s1("values")[i] ],
                               0)

                     prob
                   })

methods::setMethod("!=", methods::signature(e1 = "dice", e2 = "dice"),
                   function(e1, e2) 1 - (e1 == e2))

methods::setMethod("<", methods::signature(e1 = "dice", e2 = "dice"),
                   function(e1, e2){
                     s1 <- function(name) methods::slot(e1, name)
                     s2 <- function(name) methods::slot(e2, name)
                     prob <- 0
                     for (i in 1:length(s1("values")))
                       prob <- prob + s1("probabilities")[i] *
                       ifelse(any(s1("values")[i] < s2("values")),
                              sum(s2("probabilities")[s2("values") > s1("values")[i] ]),
                              0)

                     prob
                   })

methods::setMethod("<=", methods::signature(e1 = "dice", e2 = "dice"),
                   function(e1, e2) (e1 < e2) + (e1 == e2))

methods::setMethod(">", methods::signature(e1 = "dice", e2 = "dice"),
                   function(e1, e2) 1 - (e1 <= e2))

methods::setMethod(">=", methods::signature(e1 = "dice", e2 = "dice"),
                   function(e1, e2) 1 - (e1 < e2))

methods::setMethod("==", methods::signature(e1 = "dice", e2 = "numeric"),
                   function(e1, e2){
                     s <- function(name) methods::slot(e1, name)
                     sum(sapply(e2, function(x)
                       ifelse(any(s("values") == x),
                       s("probabilities")[s("values") == x],
                       0)
                       ))
                   })

methods::setMethod("==", methods::signature(e1 = "numeric", e2 = "dice"),
                   function(e1, e2) e2 == e1)

methods::setMethod("!=", methods::signature(e1 = "dice", e2 = "numeric"),
                   function(e1, e2) 1 - (e2 == e1))

methods::setMethod("!=", methods::signature(e1 = "numeric", e2 = "dice"),
                   function(e1, e2) 1 - (e2 == e1))

methods::setMethod("<", methods::signature(e1 = "dice", e2 = "numeric"),
                   function(e1, e2){
                     s <- function(name) methods::slot(e1, name)
                     sapply(e2, function(x)
                     ifelse(any(s("values") < x),
                            sum(s("probabilities")[s("values") < x]),
                            0)
                     )
                   })

methods::setMethod("<=", methods::signature(e1 = "dice", e2 = "numeric"),
                   function(e1, e2) (e1 < e2) + (e1 == e2))

methods::setMethod(">", methods::signature(e1 = "dice", e2 = "numeric"),
                   function(e1, e2) 1 - (e1 <= e2))

methods::setMethod(">=", methods::signature(e1 = "dice", e2 = "numeric"),
                   function(e1, e2) 1 - (e1 < e2))

methods::setMethod("<", methods::signature(e1 = "numeric", e2 = "dice"),
                   function(e1, e2) e2 > e1)

methods::setMethod("<=", methods::signature(e1 = "numeric", e2 = "dice"),
                   function(e1, e2) e2 >= e1)

methods::setMethod(">", methods::signature(e1 = "numeric", e2 = "dice"),
                   function(e1, e2) e2 < e1)

methods::setMethod(">=", methods::signature(e1 = "numeric", e2 = "dice"),
                   function(e1, e2) e2 <= e1)

#### Accessor, print and plot ####
methods::setMethod("show", methods::signature(object = "dice"),
                   function(object) {
                    cat("Rolling ",describeDice(object), ":\n", sep="")
                    cat(object$roll(1), "\n")

                    invisible(object)
                   })

methods::setMethod("print", methods::signature(x = "dice"),
                   function(x, ...) show(x))

print.dice <- function(x, ...) show(x)

methods::setMethod("names", methods::signature(x = "dice"),
                   function(x) c("description", "distribution", "roll"))

methods::setMethod("$", methods::signature(x = "dice"),
                   function(x, name)
                     if (name == "description")
                       describeDice(x)
                     else if (name == "distribution"){
                       s <- function(n) methods::slot(x, n)
                       data.frame(
                         value = s("values"),
                         probability = s("probabilities")
                       )
                     }
                     else if (name == "roll") function(n = 1){
                       s <- function(name) methods::slot(x, name)
                       sample(s("values"), n, TRUE, prob = s("probabilities"))
                     })

methods::setMethod("plot", methods::signature(x = "dice"),
                   function(x, y, ...){
                     s <- function(n) methods::slot(x, n)
                     barplot(s("probabilities"),
                             names.arg = s("values"),
                             space = 0,
                             xlab = "Values",
                             ylab = "Probabilities", ...)
                   })

plot.dice <- function(x, y, ...) plot(x)

#### as methods ####
methods::setMethod("as.numeric", methods::signature(x = "dice"),
                   function(x, n = 1,  ...) x$roll(n))

#### Summaries ####
methods::setMethod("mean", methods::signature(x = "dice"),
                   function(x, ...){
                     s <- function(name) methods::slot(x, name)
                     sum(s("values") * s("probabilities"))
                   })

mean.dice <- function(x, ...) mean(x)

methods::setMethod("min", methods::signature(x = "dice"),
                   function(x, ...){
                     min(methods::slot(x, "values"))
                   })

min.dice <- function(x, ...) min(x)

methods::setMethod("max", methods::signature(x = "dice"),
                   function(x, ...){
                     max(methods::slot(x, "values"))
                   })

max.dice <- function(x, ...) max(x)

methods::setMethod("summary", methods::signature(object = "dice"),
                   function(object, ...){
                     s <- function(name) methods::slot(object, name)

                     cat("Dice rolls:", describeDice(object), "\n")
                     cat("Possible values:",min(object),"to",max(object),"\n")
                     m <- mean(object)
                     cat("Expected value:", m, "\n")
                     varDice <- sum(s("values")^2 * s("probabilities")) -
                       m^2
                     cat("Standard deviation:", sqrt(varDice))
                   })

