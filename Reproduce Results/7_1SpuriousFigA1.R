# Box graph
rm(list=ls(all=TRUE))

library(data.table)
library(reshape)
library(latticeExtra)
library(gridExtra)
library(lmtest)

# Set working directory
setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Dynamics-of-life-expectancy-and-lifespan-equality/Reproduce Results/")

# Loading data
load("Data/HMD_Data.RData")

# Be careful with the missing years of Belgium
Data  <- HMDL[HMDL$Year >= 1900 & HMDL$PopName !="BEL",]
# Get data for belgium in consecutive years
Bel   <- HMDL[HMDL$Year >= 1919 & HMDL$PopName=="BEL",]
Data  <- data.table(rbind(Data,Bel))
Data[, Sex1:= ifelse(Sex == 'f', 'Females', 'Males')]

gdata::keep(Data,sure = T)

source('Functions_1.R')
############## Calculate lifespan equality measures
Results           <- Data[,list(h=h.frommx(mx = mx,sex = Sex[1]), 
                                v=my.cv.frommx(mx = mx,sex = Sex[1]),
                                g=log.G.frommx(mx = mx,sex = Sex[1]),
                                eo = ex[1],
                                Period = cut(Year,breaks = c(1900,1920,1959,Inf),labels = c("1900-1921","1921-1959","1960 onwards"),include.lowest = T)),
                          by = list(PopName,Sex,Sex1,Year)]

#Calculate differences on life expectancy and lifespan equality indicators
Dif.data           <- Results[,list(dif.h = diff(h),
                                    dif.g = diff(g),
                                    dif.v = diff(v),
                                    dif.eo = diff(eo),
                                    dif.year= Y(Year,lag.2 = 1), 
                                    Period = cut(Year[-1],breaks = c(1900,1920,1959,Inf),labels = c("1900-1921","1921-1959","1960 onwards"),include.lowest = T)),
                              by = list(PopName,Sex)]


### Test for spurious regression
### a part from the fact that both measures are weighted by the same density

### As a rule of thumb, Granger and Newbold [1974] suggested that one
### should be suspicious if the R2 is greater than the Durbin-Watson statistic
### (see Durbin and Watson [1950], Durbin and Watson [1951], and Durbin and
### Watson [1971]). A theoretical basis for their finding was provided by Phillips
### [1986].

R.sqr.data  <- Results[, list(R.sqr.h = R.sqr(h,eo),
                                 R.sqr.g = R.sqr(g,eo),
                                 R.sqr.v = R.sqr(v,eo),
                                 DW.h = dw(h,eo),
                                 DW.g = dw(g,eo),
                                 DW.v = dw(v,eo)), by = list(PopName,Sex)]


#Proportion of R^2 larger than DW
Prop     <- R.sqr.data[,3:5] - R.sqr.data[,6:8]
Prop     <- Prop[,ifelse(.SD > 0,1,0),]
colSums(Prop)/dim(Prop)[1]*100
prop.leq <- c(colSums(Prop)/dim(Prop)[1]*100)[1]

# mean and sd
colMeans(R.sqr.data[,3:8])

#take a look
Fig.A1<- xyplot(R.sqr.h ~ DW.h ,data=R.sqr.data,groups=Sex,
              main =expression(paste("Life expectancy (", e[o],") vs lifespan equality (",h,')')),
       pch=19,col=c("red","blue"),cex=1.9,
       xlim=c(0,2),
       ylab=list(quote(R^2),cex=1.6),       
       ylim=c(0,1.01),
       scales=list(x=list(cex=1.5,at=c(seq(0,2,.25))),
                   y=list(cex=1.5,at=c(seq(0,1,.25)))),
       xlab=list("Durbin-Watson statistic",cex=1.6),
       par.settings=my.settings1,
       key = list(x=.8,y=.2, title="Sex",background="white", 
                  text=list(c("Females","Males"))
                  ,cex=1.5,
                  points=list(pch=19,col=c("red","blue"))),             
       panel = function(x, y, ...){ 
         panel.polygon(x=c(0,1,0),y=c(0,1.01,1.01),col=makeTransparent("grey",100))
         panel.text(.39,.75,labels=paste(as.character(round(prop.leq,1)),"%"),cex=1.7)
         panel.xyplot(x, y, ...)
       }) 

Fig.A1


pdf(file="Figures/FigureA1.pdf",width=10,height=8,pointsize=4)
grid.arrange(Fig.A1, ncol=1)
dev.off()

