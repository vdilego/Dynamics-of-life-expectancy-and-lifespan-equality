rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(ggplot2)
library(RColorBrewer)

# Set working directory
setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Dynamics-of-life-expectancy-and-lifespan-equality/Reproduce Results/")

# Loading data
load("Data/Sweden_HMD.RData")

#rearrange by increasnig life expectancy
LT.SWE.1[,eo := ex[1], by = list(Year)]
LT.SWE.1      <- LT.SWE.1[order(eo),]
LT.SWE.1$Year2 <- unlist(lapply(1:length(unique(LT.SWE.1$Year)), function(x){rep(x,111)}))

gdata::keep(LT.SWE.1,sure = T)
#source some useful functions
source('Functions_1.R')

# Original: get the observed life expectancy and lifespan equality
SWE.ind <- LT.SWE.1[,list(h= h.frommx(mx,'f'), ex=ex[1]), by = list(Year2,Sex)]

# Optimal scenario: get every point in life expectancy from the max weights in w.W
# I need a functin that get the weights from mx

wW.h.frommx <- function(mx,sex='f'){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  weights_eo    <- c(dx[-length(ax)]* (ex[-length(ax)] + ax[-length(ax)]*(ex[-1]-ex[-length(ax)])),ex[length(ax)])
  l     <- length(ax)
  vx    <- dx*(ax*c(ex[-1L], 0) + (1-ax)*ex)
  vx[l] <-  ex[l]
  v     <- sum(vx)
  
  H.plus<- vx/(ex*lx)
  
  W_eta <- 1/ex[1L]+(1/v)*(1+log(lx)-H.plus)
  W_eta[is.infinite(W_eta)] <- NA
  wW <- W_eta*weights_eo
  wW
}

ex.optim.2 <- function(par,mx1,ex2,j){
  mx2 <- mx1
  mx2[j] <- mx1[j]*par
  (ex2 - LifeExpectancy(mx2))^2
}

#all years
years <- unique(SWE.ind$Year2)

#initial vector of rates
mx.init      <- LT.SWE.1[LT.SWE.1$Year2 == min(years),]$mx
ex0          <- LifeExpectancy(mx.init)
Results.dt.2 <- cbind(min(years),1,LifeExpectancy(mx.init),h.frommx(mx.init,'f'),
                      LifeExpectancy(mx.init),h.frommx(mx.init,'f'))

i <- 2
j <- which.max(wW.h.frommx(mx.init))

for(i in years[-1]){
  
  #i <- 1888
  #for(i in 1887:1885){
  
  mx <- LT.SWE.1[LT.SWE.1$Year2 == i,]$mx
  ex <- LifeExpectancy(mx)
  h  <- h.frommx(mx,'f')
  
  objective <- .0011
  
  while(objective > .001){
    
    x        <- optimize(f = ex.optim.2,interval = c(0,100),tol = .00001,mx1=mx.init,ex2=ex,j)
    objective<- x$objective
    fact     <- x$minimum
    
    if (objective <= .001){
      mx2      <- mx.init
      mx2[j]   <- mx2[j]*fact
      ex.new   <- LifeExpectancy(mx2,'f')
      h.new  <- h.frommx(mx2,'f')
      b <- cbind(i,fact,ex,h,ex.new,h.new)
      Results.dt.2 <- rbind(Results.dt.2,b)
      #update mx
      mx.init <- mx2
    }
    
    if (objective > .001){
      #print(c(i,j,fact,objective))
      mx.init[j]   <- 0
      j <- which.max(wW.h.frommx(mx.init))
    }
  } 
}


Results.dt.2 <- data.table(Results.dt.2)
names(Results.dt.2) <- c('Year','Factor','eo.original','h.original','ex.new.optimal','h.new.optimal')

ex.original.col <-brewer.pal(9,name = 'Blues')[c(7)]

Fig.5 <- ggplot()+
  
  geom_point(data = Results.dt.2,aes(x=eo.original,y = h.original),show.legend = F,shape = 16, col= ex.original.col,size= 2,alpha=I(1/4))+
  geom_smooth(data = Results.dt.2,aes(x = eo.original, y = h.original), method = "lm", se=F,col="gray29",size=1.5,lty=2,show.legend = F) + # if I want a linear one
  geom_point(data = Results.dt.2,  aes(x=eo.original,y = h.new.optimal),show.legend = F,shape = 15, col='pink' ,size= 3,alpha=I(1/4))+
  
  geom_text(data = Scenarios.data[Year %in% years4,],aes(x=eo.original,y = 3,label = Year),show.legend = F,col = 'darkgrey',angle = 0)+
  geom_segment(data = Scenarios.data[Year %in% years4,],aes(x = eo.original, y = h.original, xend = eo.original, yend = 2.9),show.legend = F, col = 'grey')+
  
  scale_x_continuous(expression(paste("Life expectancy (", e[o],")")))+
  coord_cartesian(xlim=c(18, 95))+
  scale_y_continuous(expression(paste("Lifespan equality (", h,")")))+
  
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.new.optimal),]$h.new.optimal, label = "Optimal", col = ex.optimal.col, hjust = 0)+
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.new.optimal),]$h.new.optimal-.08, label = "equality", col = ex.optimal.col, hjust = 0)+
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.original),]$h.original, label = "Observed", col = ex.original.col, hjust = 0)+
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.original),]$h.original-.08, label = "points", col = ex.original.col, hjust = 0)+
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.new.constant),]$h.new.constant, label = "Constant", col = ex.const.col, hjust = 0)+
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.new.constant),]$h.new.constant-.08, label = "change", col = ex.const.col, hjust = 0)+
  annotate("text", x = fig.label.x , y =  Scenarios.data[which.max(Scenarios.data$h.new.constant),]$h.new.constant-.08*2, label = "over age", col = ex.const.col, hjust = 0)+
  
  
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,legend.key = element_blank()
        ,strip.text = element_text(color='black')
        ,axis.line.x = element_line(color="black", size = .5)
        ,axis.line.y = element_line(color="black", size = .5),
        strip.placement = 'outside',
        strip.background =  element_rect(colour="white", fill="white"))
previous_theme <- theme_set(theme_bw())

Fig.5

