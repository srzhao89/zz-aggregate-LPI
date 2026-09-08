#############################################################################################
## Aggregate Luenberger Productivity Indicators and Its Decompositions: Theory and Application
## Valentin Zelenyuk and Shirong Zhao
#############################################################################################
rm(list=ls())
require(readxl)
require(Benchmarking)
require(BMisc)
source("./Functions/eff.components.R")
#### Set Seed ####
if (exists(".Random.seed")) {
  save.seed=.Random.seed
  flag.seed=TRUE
} else {
  flag.seed=FALSE
}
set.seed(90001)

###############################################################
df0 <- read.csv("Data/data.csv")
dim(df0)
###############################################################

df <- makeBalancedPanel(df0, idname="state", tname="year")

unique(df$Year)

X=cbind(df$x1,df$x2,df$x3,df$x4)
Y=cbind(df$y1,df$y2,df$y3)
Wx=cbind(df$w1,df$w2,df$w3,df$w4)
Wy=cbind(df$p1,df$p2,df$p3)


###############################################################
year.start=1960
year.end=2015
year=c(year.start:year.end)
no.years=length(year)-1

L=rep(NA,no.years)
E1=rep(NA,no.years)
E2=rep(NA,no.years)
T1=rep(NA,no.years)
T2=rep(NA,no.years)
S1=rep(NA,no.years)
S2=rep(NA,no.years)
S3=rep(NA,no.years)

for (i in 1:(length(year)-1)) {
  
  i1=which(df$year==year[i])
  df1=df[i1,]
  
  i2=which(df$year==year[i+1])
  df2=df[i2,]
  
  cat ("Year: ",year[i]," Observations:",dim(df1)[1],"\n")

  X1=cbind(df1$x1,df1$x2,df1$x3,df1$x4)
  Y1=cbind(df1$y1,df1$y2,df1$y3)
  Wx1=cbind(df1$w1,df1$w2,df1$w3,df1$w4)
  Wy1=cbind(df1$p1,df1$p2,df1$p3)
  
  revenue.cost1=apply(Wx1*X1, 1, sum)+apply(Wy1*Y1, 1, sum)
  s1=revenue.cost1/sum(revenue.cost1)
  
  
  X2=cbind(df2$x1,df2$x2,df2$x3,df2$x4)
  Y2=cbind(df2$y1,df2$y2,df2$y3)
  Wx2=cbind(df2$w1,df2$w2,df2$w3,df2$w4)
  Wy2=cbind(df2$p1,df2$p2,df2$p3)
  
  revenue.cost2=apply(Wx2*X2, 1, sum)+apply(Wy2*Y2, 1, sum)
  s2=revenue.cost2/sum(revenue.cost2)
  
  tt=eff.components(x1=X1,y1=Y1,x2=X2,y2=Y2,gx1=X1,gy1=Y1,gx2=X2,gy2=Y2)
  
  c11=tt$c11
  c12=tt$c12
  c21=tt$c21
  c22=tt$c22
  v11=tt$v11
  v12=tt$v12
  v21=tt$v21
  v22=tt$v22
  
  
  L[i]=0.5*mean(c11-c21+c12-c22)
  E1[i]=mean(c11-c22)
  T1[i]=0.5*mean(c12-c11+c22-c21)
  #
  E2[i]=mean(v11-v22)
  S1[i]=mean(v22-c22+c11-v11)
  #
  T2[i]=0.5*mean(v12-v11+v22-v21)
  S2[i]=0.5*mean(c12-v12+v21-c21+
                c11-v11+v22-c22)
  #
  S3[i]=0.5*mean(c12-v12+v11-c11+
                c22-v22+v21-c21)
  
}




cumsum(L)
cumsum(E1)
cumsum(T1)
cumsum(E2)
cumsum(S1)
cumsum(T2)
cumsum(S2)
cumsum(S3)

summary(L-E1-T1)
summary(L-E2-S1-T1)
summary(L-E2-S2-T2)
summary(L-E2-S1-S3-T2)

###############################################################
#### Make as Latex

res=cbind(L,E1,T1,E2,S1,T2,S2,S3)

year1=c(year.start:c(year.end-1))
year2=c((year.start+1):year.end)

tex1=formatC(year1,width=5,digits=0,format="f")
tex2=formatC(year2,width=5,digits=0,format="f")

tex=paste(tex1,"&",tex2)
for (k in 1:ncol(res)) {
  tex = paste(tex,"&",formatC(res[,k],width=7,digits = 4,format = "f"))
}
tex = paste(tex,"\\\\")

write(tex,file="./Output/lpi-sources-simple-all.tex")


###############################################################
###############################################################
## Plot Cumulative Values for Various Components


cum.L= c(0,cumsum(L))
cum.E1=c(0,cumsum(E1))
cum.T1=c(0,cumsum(T1))
cum.E2=c(0,cumsum(E2))
cum.S1=c(0,cumsum(S1))
cum.T2=c(0,cumsum(T2))
cum.S2=c(0,cumsum(S2))
cum.S3=c(0,cumsum(S3))

all.min=min(cum.L,cum.E1,cum.T1,cum.E2,cum.S1,cum.T2,cum.S2,cum.S3)
all.max=max(cum.L,cum.E1,cum.T1,cum.E2,cum.S1,cum.T2,cum.S2,cum.S3)

setEPS()
postscript(file = paste("./Figures/plot-cumulative-simple.eps"))


x=year
plot(x,cum.L, type="l",lty=1,lwd=3,col = 1, xaxt='n',ylim=c(all.min,all.max),
     xlab="Year", ylab = "TFP and Its Sources")
lines(x,cum.E1, type="l",lty=2,lwd=3,col = 2)
lines(x,cum.T1, type="l",lty=3,lwd=5,col = 3)
lines(x,cum.E2, type="l",lty=4,lwd=3,col = 4)
lines(x,cum.S1, type="l",lty=5,lwd=3,col = 5)
lines(x,cum.T2, type="l",lty=6,lwd=3,col = 6)
lines(x,cum.S2, type="l",lty=7,lwd=3,col = 7)
lines(x,cum.S3, type="l",lty=8,lwd=3,col = 8)

axis(1, at = seq(1960, 2015, by = 5))

legend('topleft',legend = c(expression(hat(L)),expression(hat(E)[1]),expression(hat(T)[1]),expression(hat(E)[2]),
                            expression(hat(S)[1]),expression(hat(T)[2]),expression(hat(S)[2]),expression(hat(S)[3])), 
       col = c(1:8), lty=c(1:8), lwd =rep(3,3,5,3,3,3,3,3))




dev.off()



###############################################################
###############################################################
## Plot Unweighted Mean Values Only for LPI



all.min=min(L)
all.max=max(L)

setEPS()
postscript(file = paste("./Figures/plot-lpi-simple.eps"))


x=year[-1]
plot(x,L, type="l",lty=1,lwd=3,col = 1, xaxt='n',ylim=c(all.min,all.max),
     xlab="Year", ylab = "TFP")
abline(h = 0, lty = 2)
axis(1, at = seq(1960, 2015, by = 5))

legend('topleft',legend = c(expression(hat(L))), 
       col = c(1), lty=c(1), lwd =rep(3))

dev.off()
















