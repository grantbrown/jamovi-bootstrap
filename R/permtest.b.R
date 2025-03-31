
# This file is a generated template, your changes will not be overwritten

permtestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "permtestClass",
    inherit = permtestBase,
    private = list(
      .bootsamps = NULL,
      .init = function(){
        private$.bootsamps <- new.env()
      },
      .run = function() {
        
        # Bootstrap Function
        bootstrap <- function(variable, 
                              statistic,
                              nSamples = 1000,
                              ...)
        {
          sapply(1:nSamples, function(x){
            statistic(sample(variable, 
                             size = length(variable), 
                             replace = TRUE),
                      ...)
          })
        }
        
        # Check if we have groups:
        if (length(self$options$group) == 0 || 
            length(self$options$dep) == 0 ||
            length(unique(self$data[[self$options$group]])) != 2){
          self$results$text$setContent("Two groups required")
          return()
        } 
        
        get_f_diff <- function(f, x, g){
          levs <- unique(g)
          f(x[g == levs[1]]) - f(x[g == levs[2]])
        }
        
        
        
        stat <- ifelse(self$options$stat == "stat_mean", mean,
                 ifelse(self$options$stat == "stat_qtile", 
                        function(x){as.numeric(quantile(x, probs = as.numeric(self$options$qtile_det)/100))},
                 ifelse(self$options$stat == "stat_sd", sd, 
                 ifelse(self$options$stat == "stat_var", var, mean))))
        
        Y <- as.numeric(self$data[[self$options$dep]])
        observed <- get_f_diff(stat, Y, self$data[[self$options$group]])
        
        permdiffs <- replicate(self$options$replicates, {
          get_f_diff(stat, Y, sample(self$data[[self$options$group]]))
        })
        
        
        
        pval <- mean(abs(permdiffs) > abs(observed))
        results <- data.frame(Observed = observed, 
                          Variable = self$options$dep, 
                          By=self$options$group,
                          pval = pval)
        
        self$results$text$setContent(results)
      })
    
)