
# 
# ### New fig
# SWE.dif      <- Dif.data[Dif.data$PopName =='SWE' & Dif.data$Sex == 'f',]
# SWE.lev      <- Results[Results$PopName =='SWE' & Results$Sex == 'f',]
# 
# e0.SWE <-  ggplot() +  
#   geom_line(data = japan.dif,aes(Year,dif.le/mean(dif.le)), col='black')+
#   geom_line(data = japan.dif,aes(Year,dif.eta/mean(dif.eta)),col='red')+
#   #geom_line(data = japan.lev,aes(Year,ex/leq),col='blue')+
#   scale_x_continuous('')+
#   scale_y_continuous('Changes')+
#   ggtitle("One-year changes in life expectancy and lifespan equality")+
#   theme(text = element_text(size = 15))+
#   theme(plot.background = element_blank())
# 
# previous_theme <- theme_set(theme_bw())
# First.dif2
# 
# 
# 
# First.dif4 <- ggplot() +  
#   #geom_line(data = japan.dif,aes(Year,prop), col='black')+
#   geom_line(data = japan.dif,aes(Year,dif.le/mean(dif.le)-dif.eta/mean(dif.eta)),col='blue')+
#   #geom_line(data = japan.lev,aes(Year,ex/leq),col='blue')+
#   scale_x_continuous('Year')+
#   scale_y_continuous('Change in life expectancy - change in lifespan equality')+
#   theme(text = element_text(size = 15))+
#   theme(plot.background = element_blank())
# First.dif4
# 
# 
# require(gridExtra)
# pdf(file="R/Figures/Figure_propor_new.pdf",width=9,height=9.5,pointsize=4)
# grid.arrange(First.dif2,First.dif4,nrow=2)
# dev.off()



### New fig 2
SWE.dif      <- Dif.data[Dif.data$PopName =='SWE' & Dif.data$Sex == 'f',]
SWE.lev      <- Results[Results$PopName =='SWE' & Results$Sex == 'f',]
gdata::keep(SWE.lev,sure=T)

e0.SWE <-  ggplot() +  
  geom_line(data = SWE.lev,aes(Year,ex), col='black')+
  scale_x_continuous('Year')+
  scale_y_continuous('Life expecyancy')+
  ggtitle("Life expectancy for Swedish females")+
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank())

previous_theme <- theme_set(theme_bw())
e0.SWE

eta.SWE <-  ggplot() +  
  geom_line(data = SWE.lev,aes(Year,leq), col='red')+
  scale_x_continuous('Year')+
  scale_y_continuous('Lifespan equality')+
  ggtitle("Lifespan equality for Swedish females")+
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank())

previous_theme <- theme_set(theme_bw())
eta.SWE

require(gridExtra)
r1 <- grid.arrange(e0.SWE,eta.SWE,nrow=1)


#get the cointegration relation for SWE
library(urca)
library(vars)
library(FitAR)

model.data <- SWE.lev[,c('leq','ex')]
l <- VARselect(model.data, lag.max = 5, type = 'both')$selection[1]
H1.trace      <- ca.jo(model.data, type="trace", K=l, spec="longrun",season = NULL,ecdet = "const")
H1.trace@cval
H1.trace@teststat

H1.trace@V
vecm <-     cajorls(H1.trace,r = 1) 
vecm$rlm$residuals


library(tsDyn)
#http://web.vu.lt/mif/a.buteikis/wp-content/uploads/2018/04/Lecture_07.pdf
model.swe <- VECM(model.data,lag=l-1, r=1, include = 'const')
model.swe
model.swe$coefficients
summary(model.swe)
t(model.swe$model.specific$beta)



VARrep(model.swe)
res <- model.swe$residuals[,1]

plot(res)

res.fig <-  ggplot() +  
  geom_rect(aes(xmin=1914, xmax=1919, ymin=-Inf, ymax=Inf), 
            fill = "red", alpha=.1, show.legend = F)+ 
  geom_line(aes(SWE.lev$Year[-c(114:115)],res), col='blue')+
  geom_point(aes(1916:1918,res[17:19]), col='black')+
  scale_x_continuous('Year')+
  scale_y_continuous('Residuals')+
  ggtitle("Residuals of cointegration")+
  theme(text = element_text(size = 15))+
  theme(plot.background = element_blank())

previous_theme <- theme_set(theme_bw())
res.fig


require(gridExtra)
pdf(file="R/Figures/Figure2.pdf",width=10.5,height=8.5,pointsize=4)
grid.arrange(r1 ,res.fig,nrow=2)
dev.off()


