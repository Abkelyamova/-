estimateFisherCovarianceMatrix <- function(objects1, objects2, mu1, mu2)
  
{
  
  rows1 <- dim(objects1)[1]
  
  rows2 <- dim(objects2)[1]
  
  rows <- rows1 + rows2
  
  cols <- dim(objects1)[2]
  
  sigma <- matrix(0, cols, cols)
  
  for (i in 1:rows1)
    
  {
    
    sigma <- sigma + (t(objects1[i,] - mu1) %*% (objects1[i,] - mu1)) / (rows + 2)
    
  }
  
  for (i in 1:rows2)
    
  {
    
    sigma <- sigma + (t(objects2[i,] - mu2) %*% (objects2[i,] - mu2)) / (rows + 2)
    
  }
  
  return (sigma)
  
}

Sigma1 <- matrix(c(2, 0, 0, 2), 2, 2)

Sigma2 <- matrix(c(2, 0, 0, 2), 2, 2)

Mu1 <- c(1, 0)

Mu2 <- c(6, 0)

xy1 <- mvrnorm(n=ObjectsCountOfEachClass, Mu1, Sigma1)

xy2 <- mvrnorm(n=ObjectsCountOfEachClass, Mu2, Sigma2)

xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))

colors <- c("green3", "yellow")

plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp = 1, xlab = "Ось X", ylab = "Ось Y", main = "Линейный дискриминант Фишера")

objectsOfFirstClass <- xl[xl[,3] == 1, 1:2]

objectsOfSecondClass <- xl[xl[,3] == 2, 1:2]

mu1 <- estimateMu(objectsOfFirstClass)

mu2 <- estimateMu(objectsOfSecondClass)

Sigma <- estimateFisherCovarianceMatrix(objectsOfFirstClass, objectsOfSecondClass, mu1, mu2)

inverseSigma <- solve(Sigma)

alpha <- inverseSigma %*% t(mu1 - mu2)

mu_st <- (mu1 + mu2) / 2

beta <- mu_st %*% alpha

abline(beta / alpha[2,1], -alpha[1,1]/alpha[2,1], col = "red", lwd = 3, )