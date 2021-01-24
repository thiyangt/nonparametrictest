
binomial_test <- function(df, variable.name,  success.level, 
                          param, sig = 0.05, 
                          alternative = c("two.sided", "less", "greater")){
  if(param > 1 & param < 0) stop("Invalid input for parameter. Include a value in [0, 1].")
  if(sig > 1 & sig < 0) stop("Invalid input for level of significance.")

  variable.name <- as.name(variable.name)
  df$variable.name <- df %>% select(all_of(variable.name))
  xdf <- df$n[df %>% count(variable.name)]
  x <- xdf$variable.name==success.level
  size <- length(df$variable.name)
  
  if(alternative == "two.sided"){
    p.value <- pbinom(x, size, prob=param) * 2
    
  } else if (alternative == "less"){
    
    p.value <- pbinom(x, size, prob=param)
  } else {
    p.value <- 1 - (pbinom(x, size, prob=param) - dbinom(x, size, prob=param) )
  }
  
  return(p.value)
}

df <- data.frame(x=c(rep(1, 8), rep(0, 2)))
binomial_test(df=df, variable.name="x",  success.level=1, 
              param=0.5, sig = 0.05, alternative = "greater")

