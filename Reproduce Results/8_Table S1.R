############# Written by JMA
############# 20/07/2019
library(data.table)
library(reshape)
library(ggplot2)
library(viridis)
library(gridExtra)

# set your working directory
setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Dynamics-of-life-expectancy-and-lifespan-equality/Reproduce Results/")

#source("1_GetHMDData.R") # Just in case you want updated HMD Data

# Loading data
load("Data/HMD_Data.RData")

# Be careful with the missing years of Belgium
Data  <- HMDL[HMDL$Year >= 1900 & HMDL$PopName !="BEL",]
# Get data for belgium in consecutive years
Bel   <- HMDL[HMDL$Year >= 1919 & HMDL$PopName=="BEL",]
Data  <- data.table(rbind(Data,Bel))
Data[, Sex1:= ifelse(Sex == 'f', 'Females', 'Males')]

gdata::keep(Data,sure = T)

#number of populations (contries and regions)
length(unique(Data$PopName))

#max life expectancy in the dataset
Data[which.max(Data$ex),]

#latest year for sweden
Data[PopName == 'SWE' & Year == 2017 & Age == 0,]

#load useful functions
source("Functions_1.R")

############## Calculate lifespan equality measures
Results           <- Data[,list(h=h.frommx(mx = mx,sex = Sex[1]), 
                                v=my.cv.frommx(mx = mx,sex = Sex[1]),
                                g=log.G.frommx(mx = mx,sex = Sex[1]),
                                eo = ex[1],
                                Period = cut(Year,breaks = c(1900,1920,1959,Inf),labels = c("1900-1921","1921-1959","1960 onwards"),include.lowest = T)),
                          by = list(PopName,Sex,Sex1,Year)]

load('Data/Threshold.ages.Rdata')


TableS1 <- merge(x = Results,y = Threshold.ages,by = c('PopName','Sex','Year'))
TableS1 <- TableS1[order(PopName,Sex,Year),]
TableS1 <- TableS1[, list(Initial = min(Year),
                          Final  = max(Year),
                          eo = round(eo[1],2),
                          h = round(h[1],2),
                          g = round(g[1],2),
                          v =round(v[1],2),
                          a.h = round(a.h[1],2),
                          a.g = round(a.g[1],2),
                          a.v = round(a.v[1],2)
                          ), by = list(PopName,Sex)]

Females <- TableS1[Sex == 'f']
Males <- TableS1[Sex == 'm']


NamesAbv        <- unique(Females$PopName)

Country.names  <- c("Australia","Austria","Belgium", "Bulgaria","Belarus","Canada","Switzerland","Chile","Czech Republic",
                    "East Germany","Germany", "West Germany", "Denmark","Spain","Estonia","Finland", "France (civilian)","France",
                    "England and Wales (Civilian)",
                    "England and Wles (non-Civilian)",
                    "Northern Ireland","U.K.","Scotland","Greece","HRV", "Hungary","Ireland","Iceland",
                    "Israel","Italy","Japan","Korea", "Lithuania","Luxembourg","Latvia","Netherlands","Norway",
                    "New Zealand (Maori)","New Zealand (Non Maori)","New Zealand",
                    "Poland","Portugal","Russia","Slovakia","Slovenia","Sweden","Taiwan","Ukraine","U.S.A.")

Males$Country <- Females$Country <- "l"

for ( i in 1:length(Country.names)){
  Females$Country[Females$PopName == NamesAbv[i]] <- Country.names[i]
  Males$Country[Males$PopName == NamesAbv[i]] <- Country.names[i]
}

Females <- Females[,c('Country','Initial','Final','eo','h','g','v','a.h','a.g','a.v'),]
Males <- Males[,c('Country','Initial','Final','eo','h','g','v','a.h','a.g','a.v'),]

write.csv(Females, file= 'TableS1_Females.csv')
write.csv(Males, file= 'TableS1_Males.csv')








