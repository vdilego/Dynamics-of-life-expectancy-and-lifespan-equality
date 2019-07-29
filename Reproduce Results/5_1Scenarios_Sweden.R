
rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(ggplot2)

# Set working directory
setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Dynamics-of-life-expectancy-and-lifespan-equality/Reproduce Results/")

# Loading data
load("Data/Sweden_HMD.RData")


#source some useful functions
source('Functions_1.R')

# Original: get the observed life expectancy and lifespan equality
SWE.ind <- LT.SWE.1[,list(h= h.frommx(mx,'f'), ex=ex[1]), by = list(Year,Sex)]


# Constant scenario: get every point in life expectancy from constant changes

#a function to minimize the difference ot two life expectancies by a constant factor
ex.optim <- function(par,mx1,ex2){
  (ex2 - LifeExpectancy(mx1*(par)))^2
}

#all years
years2 <- unique(LT.SWE.1$Year)

#initial vector of rates
mx.init <- LT.SWE.1[LT.SWE.1$Year == min(years2),]$mx
ex0     <- LifeExpectancy(mx.init)

# a data,table
Results.dt <- cbind(min(years2),1,LifeExpectancy(mx.init),h.frommx(mx.init,'f'),
                    LifeExpectancy(mx.init),h.frommx(mx.init,'f'))

# for to update mx.init, could also be done with lapply sugin <<-
for(i in years2[-1]){
  
  mx      <- LT.SWE.1[LT.SWE.1$Year == i,]$mx
  ex      <- LifeExpectancy(mx)
  h       <- h.frommx(mx,'f')
  x       <- optimize(f = ex.optim,interval = c(0,10),tol = .00001,mx1=mx.init,ex2=ex)
  fact    <- x$minimum
  ex.new  <- LifeExpectancy(mx.init*fact,'f')
  h.new <- h.frommx(mx.init*fact,'f')
  
  b          <- cbind(i,fact,ex,h,ex.new,h.new)
  Results.dt <- rbind(Results.dt,b)
  
  #update mx
  mx.init <- mx.init*fact
}

Results.dt <- data.table(Results.dt)
names(Results.dt) <- c('Year','Factor','eo.original','h.original','eo.new.constant','h.new.constant')

# Optimal scenario: get every point in life expectancy from the bottom of the age-at-death distribution

#get factor
ex.optim.2 <- function(par,mx1,ex2,j){
  mx2 <- mx1
  mx2[j] <- mx1[j]*par
  (ex2 - LifeExpectancy(mx2))^2
}

#all years
years3 <- years2

#initial vector of rates
mx.init      <- LT.SWE.1[LT.SWE.1$Year == min(years3),]$mx
ex0          <- LifeExpectancy(mx.init)
Results.dt.2 <- cbind(min(years3),1,LifeExpectancy(mx.init),h.frommx(mx.init,'f'),
                      LifeExpectancy(mx.init),h.frommx(mx.init,'f'))

j <- 1
for(i in years3[-1]){
  
  #i <- 1888
  #for(i in 1887:1885){
  
  mx <- LT.SWE.1[LT.SWE.1$Year == i,]$mx
  ex <- LifeExpectancy(mx)
  h <- h.frommx(mx,'f')
  
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
      j <- j+1
    }
  } 
}

Results.dt.2 <- data.table(Results.dt.2)
names(Results.dt.2) <- c('Year','Factor','eo.original','h.original','ex.new.optimal','h.new.optimal')


####get a third scenario with the highest weights
LT.SWE.1[,eo := ex[1], by = list(Year)]
LT.SWE.1      <- LT.SWE.1[order(eo),]
LT.SWE.1$Year2 <- unlist(lapply(1:length(unique(LT.SWE.1$Year)), function(x){rep(x,111)}))

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


#all years
years <- unique(SWE.ind$Year2)

#initial vector of rates
mx.init      <- LT.SWE.1[LT.SWE.1$Year2 == min(years),]$mx
ex0          <- LifeExpectancy(mx.init)
Results.dt.3 <- cbind(min(years),1,LifeExpectancy(mx.init),h.frommx(mx.init,'f'),
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
      Results.dt.3 <- rbind(Results.dt.3,b)
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


Results.dt.3 <- data.table(Results.dt.3)
names(Results.dt.3) <- c('Year','Factor','eo.original','h.original','ex.new.optimal.wW','h.new.optimal.wW')


Scenarios.data <-   cbind(Results.dt[,c('Year','eo.original','h.original','h.new.constant')],Results.dt.2[,c('h.new.optimal')],Results.dt.3[,c('ex.new.optimal.wW', 'h.new.optimal.wW')])

save(Scenarios.data, file = 'Data/ScenariosData.RData')
