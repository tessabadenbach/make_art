

library(ggplot2)




make_art <- function(seed=NULL){
  set.seed(seed)
  RandomNum <- sample(1:3, 1)
  print(RandomNum)
  set.seed(seed)
  random_color <- sample(colors(), 1)
  print(random_color)
  set.seed(seed)
  random_shape <- sample(5:20, 1)
  if (RandomNum == 1){
    set.seed(seed)
    random_parameter1 <- sample(2:20, 1)
    set.seed(seed)
    random_parameter2 <- sample(-2:-20, 1)
    seq(random_parameter2,random_parameter1, by = 0.025) %>%
      expand.grid(x=., y=.) %>%
      ggplot(aes(x = sin(x)^2 - log(abs(y)), y = -sin(y)^2 + log(abs(x))))+
      geom_point(alpha = 0.01, color = random_color, shape = random_shape, size = 0)+
      theme_void()+
      coord_equal()
  } else if (RandomNum == 2){
    set.seed(seed)
    random_parameter3 <- sample(-7:-14, 1)
    set.seed(seed)
    random_parameter4 <- sample(7:14,1)
    seq(random_parameter3,random_parameter4, by = 0.04) %>%
      expand.grid(x=., y=.) %>%
      ggplot(aes(x = x+pi*sin(y), y = y+ pi*sin(x)))+
      geom_point(alpha = 0.05, color = random_color, shape = random_shape, size = 0)+
      theme_void()+
      coord_polar()
  } else {
    fern <- function(x, p){
      if (p <= 0.01) {
        m <- matrix(c(0, 0, 0, .16), 2, 2)
        f <- c(0, 0)
      } else if (p <= 0.86) {
        m <- matrix(c(.85, -.04, .04, .85), 2, 2)
        f <- c(0, 1.6)
      } else if (p <= 0.93) {
        m <- matrix(c(.2, .23, -.26, .22), 2, 2)
        f <- c(0, 1.6)
      } else {
        m <- matrix(c(-.15, .26, .28, .24), 2, 2)
        f <- c(0, .44)
      }
      m %*% x + f
    }
    reps <- sample(1000:100000,1)
    p <- runif(reps)
    coords <- c(0, 0)
    m <- Reduce(fern, p, accumulate = T, init = coords)
    m <- t(do.call(cbind, m))

    plot(m, type = "p", cex = 0.1, col = random_color,
         xlim = c(-3, 3), ylim = c(0, 10),
         xlab = NA, ylab = NA, axes = FALSE)
  }
}




