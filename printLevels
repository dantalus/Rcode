PrintLvls <- function(x) {

    print(data.frame(Lvls = sapply(x[sapply(x, is.factor)], nlevels),
                     Names = sapply(x[sapply(x, is.factor)], function(y)
                       paste0(levels(y), collapse = ", "))), right = FALSE)
  }
