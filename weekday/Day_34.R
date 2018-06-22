library(imager)
hub <- load.example("hubble") %>% grayscale
plot(hub,main="Hubble Deep Field")

layout(t(1:2))
set.seed(2)
points <- rbinom(100*100,1,.001) %>% as.cimg
blobs <- isoblur(points,5)
plot(points,main="Random points")
plot(blobs,main="Blobs")

rbinom(100*100,1,.001) %>% as.cimg
imhessian(blobs)
Hdet <- with(imhessian(blobs),(xx*yy - xy^2))
plot(Hdet,main="Determinant of Hessian")
threshold(Hdet,"99%") %>% plot(main="Determinant: 1% highest values")

lab <- threshold(Hdet,"99%") %>% label
plot(lab,main="Labelled regions")

df <- as.data.frame(lab) %>% subset(value>0)
head(df,3)
unique(df$value)

library(plyr)
centers <- ddply(df,.(value),summarise,mx=mean(x),my=mean(y))
centers <- dplyr::group_by(df,value) %>% dplyr::summarise(mx=mean(x),my=mean(y))
plot(blobs)
with(centers,points(mx,my,col="red"))

nblobs <- blobs+.001*imnoise(dim=dim(blobs))
plot(nblobs,main="Noisy blobs")
get.centers <- function(im, thr="99%")
{
  dt <- imhessian(im) %$% { xx*yy - xy^2 } %>% threshold(thr) %>% label
  as.data.frame(dt) %>% subset(value>0) %>% dplyr::group_by(value) %>% dplyr::summarise(mx=mean(x), my=mean(y))
}

plot(nblobs)
get.centers(nblobs,"99%") %$% points(mx,my,col="red")

nblobs.denoised <- isoblur(nblobs,2)
plot(nblobs.denoised)
get.centers(nblobs.denoised,"99%") %$% points(mx,my,col="red")

plot(hub)
get.centers(hub,"99.8%") %$% points(mx,my,col="red")

plot(hub)
isoblur(hub,5) %>% get.centers("99.8%") %$% points(mx,my,col="red")

#Compute determinant at scale "scale". 
hessdet <- function(im,scale=1) isoblur(im,scale) %>% imhessian %$% { scale^2*(xx*yy - xy^2) }
#Note the scaling (scale^2) factor in the determinant
plot(hessdet(hub,1),main="Determinant of the Hessian at scale 1")

dat <- ldply(c(2,3,4),function(scale) hessdet(hub,scale) %>% as.data.frame %>% mutate(scale=scale))
library(ggplot2)
p <- ggplot(dat,aes(x,y))+geom_raster(aes(fill=value))+facet_wrap(~ scale)
p+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0),trans=scales::reverse_trans())

scales <- seq(2,20,l=10)

d.max <- llply(scales,function(scale) hessdet(hub,scale)) %>% parmax
plot(d.max,main="Point-wise maximum across scales")

i.max <- llply(scales,function(scale) hessdet(hub,scale)) %>% which.parmax
plot(i.max,main="Index of the point-wise maximum \n across scales")

#Get a data.frame of labelled regions
labs <- d.max %>% threshold("96%") %>% label %>% as.data.frame
#Add scale indices
labs <- mutate(labs,index=as.data.frame(i.max)$value)
regs <- dplyr::group_by(labs,value) %>% dplyr::summarise(mx=mean(x),my=mean(y),scale.index=mean(index))
p <- ggplot(as.data.frame(hub),aes(x,y))+geom_raster(aes(fill=value))+geom_point(data=regs,aes(mx,my,size=scale.index),pch=2,col="red")
p+scale_fill_gradient(low="black",high="white")+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0),trans=scales::reverse_trans())