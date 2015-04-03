fitframe <- function(target = getwd()) {
    
    # Turns Mplus .out files into a dataframe containing the AIC, BIC, ssaBIC, 
    # Entropy, and Log-Likelihood for each model. 
    #
    # Arguments:
    #
    # target - a folder containing the .out files. Defaults to the current
    #          working directory, while target = "subfolder" will point to a the
    #          named subfolder in your working directory. 
    # 
    # Returns: 
    #
    # A dataframe containing the model fit information that is well suited for
    # plotting with ggplot2.
    #
    # The function works best if you have a folder containing a set of N Mplus
    # .out files from mixture models with 1:N latent classes, and that these
    # .out files are named in a manner that preserves their natural ordering,
    # e.g. 01.out, 02.out...
    
    require (MplusAutomation)
    
    sums     <- as.data.frame(extractModelSummaries(target))  
    fit      <- list()
    a        <- c(seq(from = 1, to = nrow(sums)))
    b        <- c("AIC", "BIC", "aBIC", "Entropy", "Log Likelihood")
    fit[[1]] <- cbind(sums$AIC,     a, c(1))
    fit[[2]] <- cbind(sums$BIC,     a, c(2))
    fit[[3]] <- cbind(sums$aBIC,    a, c(3))
    fit[[4]] <- cbind(sums$Entropy, a, c(4))
    fit[[5]] <- cbind(sums$LL,      a, c(5))
    
    fit            <- as.data.frame(do.call(rbind, fit))
    colnames(fit)  <- c("value", "model", "stat")
    
    fit$stat2 <- factor(fit$stat, levels = c(1:5), labels = b)
    
    return(fit)
    
  }  

fitPlot <- function(data) {
    
               require(ggplot2)
    
               return(ggplot(subset(data, model > 0 & stat < 4), 
                             aes(x = model, y = value, shape = stat2)) +                
                      geom_point(size = 4) +                               
                      geom_line() +                                        
                      ylab("AIC/BIC/aBIC") +
                      xlab("Number of Profiles in the Model") +
                      scale_x_continuous(breaks = c(1:5)) +                          
                      theme(text = element_text (color = "black"), 
                            strip.background = element_blank(),
                            panel.grid.minor = element_blank(),
                            legend.position = "bottom", 
                            legend.title = element_blank())
            }
  
  
  entPlot <- function(data) {
    
               require(ggplot2)
    
               return(ggplot(subset(data, model > 0 & stat == 4), 
                             aes(x = model, y = value, shape = stat2)) +                
                        geom_point(size = 4) +                               
                        geom_line() +                                        
                        ylab("Entropy") +
                        xlab("Number of Profiles in the Model") +
                        scale_x_continuous(breaks = c(2:5)) +   
                        scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, .05)) + 
                        theme(text = element_text (color = "black"), 
                              panel.grid.minor = element_blank(),
                              legend.position = "bottom", 
                              legend.title = element_blank())
             }
