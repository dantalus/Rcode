

  makeRivPlot <- function(data, var1, var2, ...) {

    # Example
    # data <- data.frame(ID = c(1:500),
    #                    A  = factor(sample(c(1:4), 500, replace = T),
    #                                labels = c("A", "B", "C", "D")),
    #                    B  = factor(sample(c(1:3), 500, replace = T),
    #                                labels = c("Big", "Mid", "Small")))
    #
    # makeRivPlot(data, "A", "B")

    require(plyr)
    require(riverplot)
    require(RColorBrewer)

    names1 <- levels(droplevels(data[, var1]))
    names2 <- levels(droplevels(data[, var2]))

    var1 <- as.numeric(data[, var1])
    var2 <- as.numeric(data[, var2])

    edges <- data.frame(var1, var2 + max(var1, na.rm = T))
    edges <- count(edges)

    colnames(edges) <- c("N1", "N2", "Value")


    nodes <- data.frame(ID     = c(1:(length(names1) +
                                      length(names2))),
                        x      = c(rep(1, times = length(names1)),
                                   rep(2, times = length(names2))),
                        labels = c(names1, names2) ,
                        col    = c(brewer.pal(length(names1), "Set1"),
                                   brewer.pal(length(names2), "Set1")),
                        stringsAsFactors = FALSE)

    nodes$col <- paste(nodes$col, 95, sep = "")

    return(makeRiver(nodes, edges))

  }

  makeRivPlot3 <- function(data, var1, var2, var3, ...) {

    # Example
    # data <- data.frame(ID = c(1:500),
    #                    A  = factor(sample(c(1:4), 500, replace = T),
    #                                labels = c("A", "B", "C", "D")),
    #                    B  = factor(sample(c(1:3), 500, replace = T),
    #                                labels = c("Big", "Mid", "Small")),
    #                    C =  factor(sample(c(1:4), 500, replace = T),
    #                                labels = c("a", "b", "c", "d")))
    #
    # makeRivPlot3(data, "A", "B", "C")

    require(plyr)
    require(riverplot)
    require(RColorBrewer)

    names1 <- levels(droplevels(data[, var1]))
    names2 <- levels(droplevels(data[, var2]))
    names3 <- levels(droplevels(data[, var3]))

    var1 <- as.numeric(data[, var1])
    var2 <- as.numeric(data[, var2])
    var3 <- as.numeric(data[, var3])

    edges1 <- data.frame(var1, var2 + max(var1, na.rm = T))
    edges1 <- count(edges1)
    colnames(edges1) <- c("N1", "N2", "Value")


    edges2 <- data.frame(var2 + max(var1, na.rm = T),
                         var3 + max(var1, na.rm = T) + max(var2, na.rm = T))
    edges2 <- count(edges2)
    colnames(edges2) <- c("N1", "N2", "Value")

    edges <- rbind(edges1, edges2)

    nodes <- data.frame(ID     = c(1:(length(names1) +
                                      length(names2) +
                                      length(names3))),
                        x      = c(rep(1, times = length(names1)),
                                   rep(2, times = length(names2)),
                                   rep(3, times = length(names3))),
                        labels = c(names1, names2, names3) ,
                        col    = c(brewer.pal(length(names1), "Set1"),
                                   brewer.pal(length(names2), "Set1"),
                                   brewer.pal(length(names3), "Set1")),
                        stringsAsFactors = FALSE)

    nodes$col <- paste(nodes$col, 95, sep = "")

    return(makeRiver(nodes, edges))


  }


