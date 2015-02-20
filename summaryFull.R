 summaryFull <- function(data, ...){

    # Example
    #
    # data <- data.frame("A" = rnorm(500, 0, 1),
    #                    "B" = rnorm(500, 0, 1),
    #                    "C" = factor(sample(c("Ca", "Cb"), 500, replace = T)))
    #
    # a <- summaryA(data)
    #


    a <- list()
    v <- 1

    for (i in seq_len(ncol(data))) {

      if(is.numeric(data[, i])){

        a[[v]] <- paste(colnames(data[i]), " & ",
                        sum(is.na(data[, i])), " & ",
                        "& ",
                        round(mean(data[, i], na.rm = T), 1),
                        " (",
                        round(sd(data[, i], na.rm = T), 1),
                        ")",
                        " & ",
                        round(quantile(data[, i], 0.50, na.rm = T), 1),
                        " [",
                        round(quantile(data[, i], 0.25, na.rm = T), 1),
                        " to ",
                        round(quantile(data[, i], 0.75, na.rm = T), 1),
                        "]",
                        " & ",
                        round(min(data[, i], na.rm = T), 1),
                        " to ",
                        round(max(data[, i], na.rm = T), 1),
                        sep = "")

        v <- v + 1
      }


      if(is.factor(data[, i])){

        a[[v]] <- paste(colnames(data[i]), " & ",
                        sum(is.na(data[, i])), " &  &  &  & ",
                        sep = "")

        for (j in 1:length(table(data[,i]))){

          a[[v + j]] <- paste("\\multicolumn{1}{r}{\\textit{",
                              levels(data[, i])[j],
                              "}} &   & ",
                              round(table(data[, i])[j] / sum(table(data[, i])), 2),
                              "(",
                              table(data[, i])[j],
                              ")",
                              " &  &  &",
                              sep = "")

        }

        v <- v + j + 1


      }

    }


    do.call(rbind, a)

  }
