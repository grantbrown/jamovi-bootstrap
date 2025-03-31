
# This file is a generated template, your changes will not be overwritten

twosampleClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "twosampleClass",
    inherit = twosampleBase,
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
            length(unique(unique(self$data[[self$options$group]]))) != 2){
          self$results$text$setContent("Analysis requires a numeric dependent variable and exactly two levels in the grouping variable") 
          return()
        } else{
          formula <- jmvcore::constructFormula(self$options$dep,c(self$options$group))
          formula <- as.formula(paste0(formula, "-1"))
          levs <- as.character(unique(self$data[[self$options$group]]))
        }
        
        M <- model.matrix(formula, data = self$data)
        alpha <- (100-self$options$conflevel)/100
        stat <- ifelse(self$options$stat == "stat_mean", mean,
                       ifelse(self$options$stat == "stat_median", median,
                              ifelse(self$options$stat == "stat_sd", sd, 
                                     ifelse(self$options$stat == "stat_var", var, mean))))
        
        
        if (ncol(M) != 2){
          stop("Invalid design matrix")
        }
        
        idx1 <- which(M[,1] > 0)
        idx2 <- which(M[,2] > 0)
        
        Y1 <- as.numeric(self$data[[self$options$dep]][idx1])
        Y2 <- as.numeric(self$data[[self$options$dep]][idx2])
        bs1 <- bootstrap(Y1, statistic = stat, 
                         nSamples = self$options$replicates)
        bs2 <- bootstrap(Y2, statistic = stat, 
                         nSamples = self$options$replicates)
        
        if (self$options$comparison == "comp_diff"){
          private$.bootsamps[["difference"]] <-  bs1-bs2
        } else{
          private$.bootsamps[["difference"]] <-  bs1/bs2
        }
        results <- data.frame(Comparison=paste0(levs[1], " vs. ", levs[2]),
                   Type = ifelse(self$options$comparison == "comp_diff", "Difference", "Ratio"),
                   Variable=self$options$dep, 
                   Estimate=mean(private$.bootsamps[["difference"]]),
                   Q025=quantile(private$.bootsamps[["difference"]], probs = c(alpha/2)),
                   Q975=quantile(private$.bootsamps[["difference"]], probs = c(1-alpha/2)))
        row.names(results) <- ""
        
        self$results$text$setContent(results)
      },
      .plot=function(image, ...) {
        
        if (as.logical(self$options$plot)){
          
          if (!length(names(private$.bootsamps))){
            return()
          }
          
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
    