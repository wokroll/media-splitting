library(pracma)

#функція повертає інтегроване охоплення при заданому спліті
all_calculations <- function(df, media_split, budget, cpp){


   df_initial <- df[,c(1:6)]
   
   # обрахунок сплітів
   split_trp <- c()
   for (i in 1:5){
      split_trp[i] <- budget*media_split[i]/cpp[i]
   }
   # replace na with zero
   df_initial[is.na(df_initial)] <- 0
   
   model <- vector(mode = "numeric", length = 5)
   df_initial[which.max(df_initial[[4]]), 1]
   for (i in 1:(ncol(df_initial)-1)){
      model[i] <- cubicspline(df_initial[[1]], df_initial[[i+1]], split_trp[i])
   }
   
   # обрахунок по Шматову
   res <- 1 - prod(1-model)
   
   return(res)
}

# функція приймає датафрейм та декілька інших параметрів, щоб передати їх в all_calculations
# запускає all_calculations багато разів і вертає датафрейм залежності охоплення від сплітів(точніше частки першого медіа) 
plot_data <- function(df, budget, cpp){
   my_step <- 0.1
   results <- data.frame()
   for (i in seq(0, 1, my_step)){
      for (j in seq(0, 1-i, my_step)){
         for (k in seq(0, 1-(i+j), my_step)){
            for (m in seq(0, 1-(i+j+k), my_step)){
               current_split <- c(i, j, k, m, 1-sum(c(i,j,k,m)))
               results <- rbind(results, c(current_split, all_calculations(df = df, media_split = current_split, budget = budget, cpp = cpp )))
            }
         }
      }
   }
   colnames(results) <- c("Media 1","Media 2","Media 3","Media 4","Media 5", "Reach")
   
   return (results)
}

# функція приймає датафрейм з plot_data, знаходить максимальне охоплення і 
# повертає спліт, при якому його було досягнуто
optimal_split <- function(df){
   return(df[which.max(df[[6]]), 1:5] )
}



