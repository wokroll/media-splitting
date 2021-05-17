source("helpers.R", encoding = "utf-8")

#path <- "Audiences Data New.xlsx"
#df <- read_excel(path, sheet = "test")

#media_split = c(0.5, 0.125, 0.125, 0.125, 0.125)
#budget = 2000000
#cpp = as.numeric(c(unlist(df[1,c(7:11)])))

calc_gradients <- function(df, i, media_split, delta, budget, cpp){
  now <- all_calculations(df = df, media_split = media_split, budget, cpp)
  media_split[i] <- as.numeric(media_split[i] + delta)
  later <- all_calculations(df = df, media_split = media_split, budget, cpp)
  return (later - now)
  
}

# caclulate gradients
gradients <- function(df, media_split, delta, budget, cpp){
  grads <- numeric()
  for (i in seq(1, 5)){
    grads[i] <- calc_gradients(df, i, media_split, delta, budget, cpp)
  }
  return (grads)
}

optimize <- function(df, budget, cpp, alpha = 0.1, delta = 0.005, max_iter = 300){
  media_split <- replicate(5, 0.2)
  for ( i in 1:max_iter){
    grads <- gradients(df, media_split, delta, budget, cpp)
  
    #dynamic step for better convergence
    if (i == (max_iter * (1/3)) || i == (max_iter * (2/3))){
      alpha <- alpha / 2
    }
    # make step to the highest gradient and normalize result
    media_split[which.max(grads)] <- (media_split[which.max(grads)] * (1  + alpha))
    media_split <- media_split / sum(media_split)
  }

  media_split <- as.data.frame(matrix(as.numeric(media_split), nrow = 1, ncol = 5))
  colnames(media_split) <- c("Media 1","Media 2","Media 3","Media 4","Media 5")
  return (media_split)
}




