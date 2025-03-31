
# This file is a generated template, your changes will not be overwritten

onesampleClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "onesampleClass",
    inherit = onesampleBase,
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
          if (length(self$options$group) == 0){
            #formula <- as.formula(paste0("\`", self$options$dep, "\`~1", collapse = ""))
            levs <- "Overall"
            M <- matrix(1, nrow = nrow(self$data))
          } else{
            levs <- as.character(unique(self$data[[self$options$group]]))
            M <- Reduce("cbind", lapply(levs, function(lv){
              matrix(1*(self$data[[self$options$group]] == lv), ncol = 1)
            }))
          }
        
          alpha <- (100-self$options$conflevel)/100
          stat <- ifelse(self$options$stat == "stat_mean", mean,
                  ifelse(self$options$stat == "stat_median", median,
                  ifelse(self$options$stat == "stat_sd", sd, 
                  ifelse(self$options$stat == "stat_var", var, mean))))
        
          
          
          
          results <- Reduce("rbind", lapply(1:ncol(M), function(i){
            
            idx <- which(M[,i] > 0)
            Y <- as.numeric(self$data[[self$options$dep]][idx])
            private$.bootsamps[[levs[i]]] <-  bootstrap(Y, statistic = stat, 
                                      nSamples = self$options$replicates)
            data.frame(Group=levs[i],
                       Variable=self$options$dep, 
                       Observed=stat(Y),
                       Q025=quantile(private$.bootsamps[[levs[i]]], probs = c(alpha/2)),
                       Q975=quantile(private$.bootsamps[[levs[i]]], probs = c(1-alpha/2)))

          }))
          #names(private$.bootsamps) <- levs
          row.names(results) <- 1:nrow(results)
          self$results$text$setContent(results)
        },
        .plot=function(image, ...) {
          
          if (as.logical(self$options$plot)){
            
            
            
            combo_tbls <- lapply(names(private$.bootsamps), function(nm){
              data.frame(Group = nm, 
                         samples = as.numeric(private$.bootsamps[[nm]]))
              
            })
            
          
            plotData <- Reduce("rbind", combo_tbls)
            tryCatch({
              plot <- ggplot(plotData, aes(x = samples, fill = Group)) +
                geom_histogram()
              print(plot)
              return(TRUE)
            }, error = function(x){
              stop(paste("Names: (", paste0(names(plotData), collapse = ", "), ") dim:", paste0(dim(plotData), collapse = ", "),
                         ", Envnames: ", paste0(names(private$.bootsamps), collapse = ", ")))
            })
          }
          as.logical(self$options$plot)[1]
        })

)
