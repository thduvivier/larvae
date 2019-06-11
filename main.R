setwd("C:/Users/duviv/Documents/University/KUL/S2/Statistical consulting/Project1/KUL Consulting class 2019/larvae")
install.packages("magick")
install.packages("keras")
install.packages("BiocManager")
BiocManager::install("EBImage")
library(magick)
library(keras)
library(EBImage)

#####.
# 1. Import and transform images----
#####.

j<-1
paste(paste(larva, j, sep=""), j, sep="")
fhi = matrix(1, nrow = 3, ncol = 3)
fhi[2, 2] = -5

for(j in 1:82)
{
  if (j==17 || j==81)
  {
    j<-j+1
  }
  print(j)
  assign(x = paste("larva", j, sep=""), value = readImage(paste(j,'.jpg', sep="")))#this gives an 'image' data type
  assign((paste("larva", j, sep="")), filter2(get(paste("larva", j, sep="")), fhi))
}

#####.
# 2. Algorithm with curvature----
#####.

length.function <- function(larva){
  #Left bond
  n <- length(larva[,1,3])/4
  LB <- c(larva[n,,3])
  for(i in  1:5) #set i length to define the accuracy, 5 gets a decent accuracy
  {
    print(i)
    if(any(LB < 0.7))
    {
      n <- n/2
      LB <- c(larva[n,,3])
    }
    else
    {
      n <- n*1.5
      LB <- c(larva[n,,3])
    }
    while (all(LB > 0.7) && i == 5)
    {
      n <- n*1.5
      LB <- c(larva[n,,3])
    }
  }
  n
  
  hl <- which(LB < 0.7)
  hl <- mean(hl)
  x1 <- rbind(n, hl)
  
  #Right bond
  m <- length(larva[,1,3])*0.75
  RB <- c(larva[m,,3])
  for(i in  1:5)
  {
    print(i)
    if(any(RB < 0.7))
    {
      m <- m + (1920 - m)/2
      RB <- c(larva[m,,3])
    }
    else
    {
      m <- m - (1920 - m)/4
      RB <- c(larva[m,,3])
    }
    while (all(RB > 0.7) && i == 5)
    {
      m <- m - (1920 - m)/4
      RB <- c(larva[m,,3])
    }
  }
  
  m
  hr <- mean(which(RB < 0.7))
  x2 <- rbind(m, hr)
  
  #middle point
  o <- length(larva[,1,3])*0.5
  O <- c(larva[o,,3])
  oY <- mean(which(O < 0.7))
  x3 <- rbind(o, oY)
  
  dist <- sqrt(sum((x1 - x3) ^ 2)) + sqrt(sum((x3 - x2) ^ 2))
  return(dist)
}

#fill the matrix
distances <- matrix(ncol=3, nrow=82)
for (i in 1:82)
{
  if (i == 17 || i == 81)
  {
    # distances[i,1] <- paste("non existing larva", i, sep=" ")
    i <- i+1
  }
  print(paste("number", i))
  distances[i,1] <- as.numeric(length.function(get(paste("larva", i, sep=""))))
  larva_surface <- get(paste("larva", i, sep=""))[,,3] < 0.7
  distances[i,2] <- as.numeric(sum(larva_surface, na.rm = TRUE))
  distances[i,3] <- distances[i,2]/ distances[i,1]
}

write.table(distances, file ="distances_output")

mean(distances[,3], na.rm = TRUE)
cbind(distances, rowMeans(a[,1:10]) )distances[,2] /(mean(distances[,3], na.rm = TRUE))
distances <- cbind(distances, distances[,2] /(mean(distances[,3], na.rm = TRUE)))
distances[,5] <- distances[,1] - distances[,4]
names <- c("length", "surface", "ratio", "Estimated-length", "Difference")
colnames(distances) <- names

####
# 3. Treat all with surface----
####

df <- distances_output
names <- c("length", "surface", "ratio")
colnames(df) <- names
avg <- mean(df$ratio, na.rm = TRUE)


surface <- matrix(ncol=1, nrow=82)
for (i in 1:82)
{
  if (i == 17 || i == 81)
  {
    # distances[i,1] <- paste("non existing larva", i, sep=" ")
    i <- i+1
  }
  print(paste("number", i))
  larva_surface <- get(paste("larva", i, sep=""))[,,3] < 0.7
  surface[i,1] <- as.numeric(sum(larva_surface, na.rm = TRUE))
}

surface2 <- surface[,1] / avg
surface <- cbind(surface, surface2)
