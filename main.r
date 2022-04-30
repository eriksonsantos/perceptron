rm(list = ls())
library('plot3D')
source('/media/erikson/Arquivos/UFMG/2021-2/Redes Neurais/Primeira Parte/Exercicio4/code/R/perceptron.r')
source('/media/erikson/Arquivos/UFMG/2021-2/Redes Neurais/Primeira Parte/Exercicio4/code/R/functions.r')
s1<- 0.4
s2 <- 0.4

nc <- 200

xc1 <-matrix(rnorm(nc*2),ncol=2)*s1 +  t(matrix((c(2,2)),ncol=nc,nrow=2) )
xc2 <-matrix(rnorm(nc*2),ncol=2)*s2 +  t(matrix((c(4,4)),ncol=nc,nrow=2) )


#plot(xc1[,1],xc1[,2],col='red',xlim=c(0,6),ylim=c(0,6),xlab='x_1',ylab='x_2')
#par(new=T)
#plot(xc2[,1],xc2[,2],col='blue',xlim=c(0,6),ylim=c(0,6),xlab='',ylab='')

x1_reta <-seq(6/100,6,6/100)
y <- -x1_reta+6

#par(new=T)
#plot(x1_reta,x2_reta,type='l',col='orange',xlim=c(0,6),ylim=c(0,6),xlab='',ylab='')

x_train <- rbind(xc1,xc2)
teste2 <- cbind(x1_reta,y)
y_train1<- matrix(1, nrow = 200, ncol=1)
y_train2 <- matrix(0, nrow = 200, ncol=1)
y_train <- rbind(y_train1, y_train2)

retlist <- perceptron(x_train,y_train,0.1,0.1,30000,1)

w <- unlist(retlist[1])
erro <- unlist(retlist[2])

seqi <- seq(0,6,0.1)
seqj <- seq(0,6,0.1)

M <- matrix(0, nrow=length(seqi), ncol=length(seqj))

ci <- 0

for (i in seqi) {
  ci <- ci + 1
  cj <- 0
  
  for (j in seqj) {
    cj <- cj + 1
    x <- as.matrix(cbind(-1,i,j))
    M[ci,cj] <- 1.0*((x%*%w) >=0)
    
  }
  
}

plot(xc1[,1],xc1[,2],col='red',xlim=c(0,6),ylim=c(0,6),xlab='x_1',ylab='x_2')
par(new=T)
plot(xc2[,1],xc2[,2],col='blue',xlim=c(0,6),ylim=c(0,6),xlab='',ylab='')
par(new=T)
contour(seqi,seqj,M,xlim=c(0,6),ylim=c(0,6),xlab='',ylab='')


ribbon3D(seqi,seqj,xlim=c(0,6),ylim=c(0,6),M,colkey = F)
scatter3D(xc1[,1],xc1[,2],matrix(0, nrow = dim(xc1)[1]),add=T,col='blue',colkey = F)
par(new=T)
scatter3D(xc2[,1],xc2[,2],matrix(0, nrow = dim(xc2)[1]),add=T,col='red',colkey = F)

