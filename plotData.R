  plotData <- function(data, ...){
  
    library(ggplot2)
    library(viridis)
    library(ggthemes)
    library(ggalt)

    for (i in seq_along(data)) {

      tryCatch({

        readline(prompt="Press [enter] to continue")

        if(is.numeric(data[[i]])){

          if(length(unique(data[[i]])) < 8){

            data[[i]] <- factor(data[[i]])

            print(ggplot(data[i], aes_string(x =    names(data[i]),
                                             fill = names(data[i]))) +
                    geom_bar() +
                    ylab("Count") +
                    xlab(names(data[i])) +
                    scale_fill_viridis("", discrete = TRUE) +
                    theme_base() +
                    theme(panel.border = element_blank(),
                          legend.position = ""))

            print(table(data[i], dnn = names(data[i])))

            print(round(table(data[i]) / sum(table(data[i])), 2))

          }

          else{

            print(ggplot(data[i], aes_string(x = names(data[i]))) +
                    geom_bar(fill = viridis(1)) +
                    ylab("Count") +
                    xlab(names(data[i])) +
                    theme_base() +
                    theme(panel.border = element_blank(),
                          legend.position = ""))

            print(summary(data[i]))

          }

        }
        
        if(class(data[[i]]) == "POSIXct"){

            print(ggplot(data[i], aes_string(x = names(data[i]))) +
                    geom_bkde(fill= viridis(1)) +
                    ylab("Count") +
                    xlab(names(data[i])) +
                    theme_base() +
                    theme(panel.border = element_blank(),
                          legend.position = ""))

            print(summary(data[i]))
        }

        if(is.factor(data[[i]])){

          print(ggplot(data[i], aes_string(x =    names(data[i]),
                                           fill = names(data[i]))) +
                  geom_bar() +
                  ylab("Count") +
                  xlab(names(data[i])) +
                  scale_fill_viridis("", discrete = TRUE) +
                  theme_base() +
                  theme(panel.border = element_blank(),
                        legend.position = ""))

          print(table(data[i], dnn = names(data[i])))

          print(round(table(data[i]) / sum(table(data[i])), 2))

        }


        if(is.character(data[[i]])){

          data[[i]] <- factor(data[[i]])

          print(ggplot(data[i], aes_string(x =    names(data[i]),
                                           fill = names(data[i]))) +
                  geom_bar() +
                  ylab("Count") +
                  xlab(names(data[i])) +
                  scale_fill_viridis("", discrete = TRUE) +
                  theme_base() +
                  theme(panel.border = element_blank(),
                        legend.position = ""))

          print(table(data[i], dnn = names(data[i])))

          print(round(table(data[i]) / sum(table(data[i])), 2))

        }


      }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})

    }

  }
  
# plotData(iris)  
  
