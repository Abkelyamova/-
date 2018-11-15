euclideanDistance <- function(u, v) 
{
  sqrt(sum((u - v) ^ 2))
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
{
   l <- dim(xl)[1] 
   n <- dim(xl)[2] - 1
   distances <- matrix(NA, l, 2) 
   for (i in 1:l) 
   {
      distances[i,] <- c(i, metricFunction(xl[i, 1:n], z))
   }
   orderedXl <- xl[order(distances[, 2]),] 
   return (orderedXl)
}
  
kwNN <- function(xl, z, k, q) 
{ 
  orderedXl <- sortObjectsByDist(xl, z) 
  n <- dim(orderedXl)[2] - 1 
  m <- matrix(c('setosa','versicolor', 'virginica', 0, 0, 0), nrow = 3, ncol = 2)
  
  for(i in 1:k){ 
    orderedXl[i, 4] = q^i 
  } 
  classes <- orderedXl[1:k, (n+1):(n+2)]
  
  m[1,2]=sum(classes[classes$Species=='setosa', 2])
  m[2,2]=sum(classes[classes$Species=='versicolor', 2])
  m[3,2]=sum(classes[classes$Species=='virginica', 2])
  
  class <- m[,1][which.max(m[,2])]
  return (class) 
}


colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, 
     xlab = "Äëèíà Ëåïåñòêà", ylab = "Øèðèíà Ëåïåñòêà", main = "11wNN ïðè q=0.8") 

xl <- iris[, 3:5] 
x <- 0.7 
y <- 0 
while (x<7) 
{ 
  while (y<3) 
  { 
    
    z <- c(x, y) 
    class <- kwNN(xl, z, k=11, q=0.8) 
    points(z[1], z[2], pch = 1, col = colors[class], asp = 1) 
    y <- y+0.1
    
  } 
  
  y <- 0 
  x <- x+0.1 
}
