setwd("C:/Users/Annie/Desktop/이통특")

# loading packages
library(fda)
library(ggplot2)
library(ggmap)
library(reshape2)
library(dplyr)
library(ggrepel)
library(data.table)

# 0. data preprocessing
data <- read.csv("KoreanDataSheet_지점추가.csv") # 2018 data

region <- data$Region
rownames(data) <- region ; data <- data[,-1]
head(data)

# 1. Plotting
# 1.1 Raw data
data.mlt <- melt(data[,-1]) # remain only month(variable) and value(temp)
head(data.mlt)
data.mlt <- cbind(region = rep(region, 12), data.mlt)
colnames(data.mlt) <- c("region", "month", "temp")
data.mlt$region <- as.factor(data.mlt$region)
head(data.mlt)

mean.dat <- data.mlt %>% group_by(month) %>% summarise(m = mean(temp))
with(mean.dat, month <- as.character(month))

ggplot(data.mlt, aes(x = month, y = temp, group = region, color = region)) +
  geom_line() + geom_point(size = 2) + ggtitle("Raw Data") + theme_bw() + 
  geom_line(data = mean.dat, aes(x = month, y = m, group = 1), inherit.aes = FALSE, size = 1.3)

# 1.2 Normalized data
nor_data <- t(apply(data[,-1], 1, scale))
data.mlt <- melt(nor_data)
head(data.mlt)
colnames(data.mlt) <- c("region", "month", "temp")
data.mlt$region <- as.factor(data.mlt$region)
head(data.mlt)

mean.dat <- data.mlt %>% group_by(month) %>% summarise(m = mean(temp))
with(mean.dat, month <- as.character(month))

ggplot(data.mlt, aes(x = month, y = temp, group = region, color = region)) +
  geom_line() + geom_point(size = 2) + ggtitle("Scaled Data") + theme_bw() + 
  geom_line(data = mean.dat, aes(x = month, y = m, group = 1), inherit.aes = FALSE, size = 1.3)

# 1.3 Observatory
register_google(key='AIzaSyDLbJFdxvyywERerFK2piCIDPLjNLMlbQk')

obs <- fread("관측지점.csv")
df <- obs[종료일 == ""&지점 %in% data$RegionID, .(name = 지점명, lon = 경도, lat = 위도)]
head(df)

cen <- c(mean(df$lon),mean(df$lat))
map <- get_googlemap(center=cen,
                     maptype="roadmap",
                     zoom=7)

ggmap(map) + geom_point(data = df, aes(x = lon, y = lat), size = 4.5, col = "red") 
#+ geom_text_repel(data = df,aes(label = name), col = "red", size= 4.5) 


# 2. smoothing using regression analysis
# Select the number of basis
z <- NULL
for(i in 1:5){
  monthbasis <- create.fourier.basis(c(0, 12), nbasis = (2*i +1), period = 12)
  fd_obj <- smooth.basis(1:12, t(data[,-1]), monthbasis, fdnames = list("month","region","Deg C"))$fd
  est <- eval.fd(1:12, fd_obj)
  mse <- mean((t(data[,-1])-est)^2)
  z <- c(z, mse)
}

df <- data.frame(x = c(3,5,7,9,11), y = z)
ggplot(df,aes(x,y)) + geom_point(size = 3, col = "navy") + geom_line(size = 1,col = "navy") + 
  theme_bw() + scale_x_continuous(breaks = c(3,5,7,9,11)) + xlab("number of basis") + ylab("MSE")

monthbasis <- create.fourier.basis(c(0, 12), nbasis = 7, period = 12)
plot(monthbasis, lwd = 2)
fd_obj <- smooth.basis(1:12, t(data[,-1]), monthbasis, fdnames = list("month","region","Deg C"))$fd
plot(fd_obj, lty = 1)


smooth.basis.fun <- function(x) {
  result <- smooth.basis(1:12, x, monthbasis)
  return(result$fd)
}
data.smooth <- apply(data[,-1], 1, smooth.basis.fun)
# str(data.smooth)
plot(data.smooth[[1]], xlab="month", ylab="temperature",
     col=1, main="smooth temperature", ylim=c(-10, 40))
for (i in 2:nrow(data)) lines(data.smooth[[i]], col=i) 

# 2.1. first derivative of smoothing data

# ex
# plot(deriv.fd(data.smooth[[1]], 1))
# plot(deriv.fd(data.smooth[[1]]))

data.smooth.1 <- list()
for (i in 1:90){
  data.smooth.1[[i]] <- deriv.fd(data.smooth[[i]], 1)
}
plot(data.smooth.1[[1]], xlab="month", ylab="temperature",
     col=1, main="the first derivative curves", ylim=c(-11, 11))
for (i in 2:90) lines(data.smooth.1[[i]], col=i)

# 2.2. second derivative of smoothing data

data.smooth.2 <- list()
for (i in 1:90){
  data.smooth.2[[i]] <- deriv.fd(data.smooth[[i]], 2)
}
plot(data.smooth.2[[1]], xlab="month", ylab="temperature",
     col=1, main="the second derivative curves", ylim=c(-10, 10))
for (i in 2:90) lines(data.smooth.2[[i]], col=i)


# 3. Principal Component Analysis
# 3.1 Multivariate PCA
pc.cr <- prcomp(t(data[,-1]), scale. = T)
summary(pc.cr)
df <- as.data.frame(pc.cr$x[,1:2])
ggplot(df, aes(PC1, PC2))+ theme_bw() +
  geom_text(aes(label = rownames(df)), col = "purple", size= 6) 

screeplot(pc.cr, main = "", col = "red", type = "lines", pch = 1, npcs = length(pc.cr$sdev), lwd = 2)
biplot(pc.cr)

# 3.2 Functional PCA
monthbasis <- create.fourier.basis(c(0,12), nbasis = 7, period = 12)
plot(monthbasis)
fd_obj <- smooth.basis(1:12, t(data[,-1]), monthbasis, fdnames = list("month","region","Deg C"))$fd
pca_obj <- pca.fd(fd_obj, nharm = 7, centerfns = T )

# (1) Eigenfunctions
fdmat <- as.data.frame(eval.fd(1:12, pca_obj[[1]]))
ggplot(fdmat, aes(x = 1:12, y = fdmat[,1])) + geom_point(size = 3) + geom_line(size = 1) + 
  xlab("month") + ylab("Value of PC curve") + ggtitle(paste("PC 1")) + theme_bw()+
  ylim(c(-0.5, 0.5)) + geom_abline(slope = 0, intercept = 0, linetype = "dashed", col = "grey50")+
  scale_x_continuous(breaks = c(2,4,6,8,10,12))

ggplot(fdmat, aes(x = 1:12, y = fdmat[,2])) + geom_point(size = 3) + geom_line(size = 1) + 
  xlab("month") + ylab("Value of PC curve") + ggtitle(paste("PC 2")) + theme_bw()+
  ylim(c(-0.8, 0.5)) + geom_abline(slope = 0, intercept = 0, linetype = "dashed", col = "grey50")+
  scale_x_continuous(breaks = c(2,4,6,8,10,12))

ggplot(fdmat, aes(x = 1:12, y = fdmat[,3])) + geom_point(size = 3) + geom_line(size = 1) + 
  xlab("month") + ylab("Value of PC curve") + ggtitle(paste("PC 3")) + theme_bw()+
  ylim(c(-0.8, 0.5)) + geom_abline(slope = 0, intercept = 0, linetype = "dashed", col = "grey50")+
  scale_x_continuous(breaks = c(2,4,6,8,10,12))

# (2) Mean +/- eigenfunctions
plot.pca.fd(pca_obj)

# (3) Eigenvalues
eigvals <- pca_obj[[2]]
df <- data.frame(x = 1:7, y = eigvals)

ggplot(df, aes(x,y)) + geom_point(size = 3, col = "red") + geom_line(size = 1, col = "red") + 
  xlab("Eigenvalue Number") + ylab("Eigenvalues") + theme_bw() + 
  scale_x_continuous(breaks = 1:7)

# (4) Score functions
harmscr <- data.frame(name = region, pca_obj[[3]])

ggplot(harmscr, aes(X1, X2)) + theme_bw() + xlab("PC 1") + ylab("PC 2") + 
  geom_text(aes(label = name), col = "blue", size= 6) 


# 4. Clustering - Region
set.seed(123)
str(data)
data.clustering <- data[, -1]

data.kmeans <- kmeans(data.clustering, centers=3)
data.kmeans$centers

data.clustering$cluster <- as.factor(data.kmeans$cluster)

ggplot(aes(data.clustering$Jan, data.clustering$Aug, colour=cluster),
      data=data.clustering) + theme_bw() + 
  geom_text(label=rownames(data.clustering), nudge_x = 0.25, nudge_y = 0.25, check_overlap = T,
            size = 7)
