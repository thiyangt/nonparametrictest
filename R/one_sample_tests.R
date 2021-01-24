
binomial_test <- function(df, col.name,  value, 
                          param, sig = 0.05, 
                          alternative = c("two.sided", "less", "greater")){
  if(param > 1 & param < 0) stop("Invalid input for parameter. Include a value in [0, 1].")
  if(sig > 1 & sig < 0) stop("Invalid input for level of significance.")

  col.name <- as.name(col.name)
  # size
  size <- dim(df %>% select(all_of(col.name)))[1]
  # number of successes
  xdf <- df %>% select(all_of(col.name)) 
  xlogical <- xdf == value
  x <- sum(xlogical[,1])
  
  if(alternative == "two.sided"){
    p.value <- pbinom(x, size, prob=param) * 2
    
  } else if (alternative == "less"){
    
    p.value <- pbinom(x, size, prob=param)
  } else {
    p.value <- 1 - (pbinom(x, size, prob=param) - dbinom(x, size, prob=param) )
  }
  
  return(p.value)
}

df <- data.frame( x = c(rep(1, 8), rep(0, 2)) )
binomial_test(df=df, col.name="x",  value=1, 
              param=0.5, sig = 0.05, alternative = "greater")

