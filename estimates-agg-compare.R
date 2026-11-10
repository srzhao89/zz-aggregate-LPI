#############################################################################################
## Aggregate Luenberger Productivity Indicators and Its Decompositions: Theory and Application
## Valentin Zelenyuk and Shirong Zhao
#############################################################################################
rm(list=ls())
require(readxl)
require(Benchmarking)
require(BMisc)
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

L.in=rep(NA,no.years)
L.ou=rep(NA,no.years)
L.io=rep(NA,no.years)
M.in=rep(NA,no.years)
M.ou=rep(NA,no.years)
M.io=rep(NA,no.years)

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
  
  #revenue.cost1=apply(Wx1*X1, 1, sum)+apply(Wy1*Y1, 1, sum)
  #s1=revenue.cost1/sum(revenue.cost1)
  
  
  X2=cbind(df2$x1,df2$x2,df2$x3,df2$x4)
  Y2=cbind(df2$y1,df2$y2,df2$y3)
  Wx2=cbind(df2$w1,df2$w2,df2$w3,df2$w4)
  Wy2=cbind(df2$p1,df2$p2,df2$p3)
  
  #revenue.cost2=apply(Wx2*X2, 1, sum)+apply(Wy2*Y2, 1, sum)
  #s2=revenue.cost2/sum(revenue.cost2)
  
  #tt=eff.components(x1=X1,y1=Y1,x2=X2,y2=Y2,gx1=X1,gy1=Y1,gx2=X2,gy2=Y2)
  
  # redefine the inputs and outputs that can be used in DEA linear Programming
  x1=X1
  y1=Y1
  x2=X2
  y2=Y2
  gx1=X1
  gy1=Y1
  gx2=X2
  gy2=Y2
  
  # Aggregate LPI With Input and Output Directions
  c11.io= Benchmarking::dea.direct(X=x1,Y=y1,
                                DIREC=cbind(gx1,gy1),
                                XREF=x1,YREF=y1,
                                ORIENTATION="in-out",
                                RTS="crs")$objval
  c12.io= Benchmarking::dea.direct(X=x1,Y=y1,
                                DIREC=cbind(gx1,gy1),
                                XREF=x2,YREF=y2,
                                ORIENTATION="in-out",
                                RTS="crs")$objval
  c21.io= Benchmarking::dea.direct(X=x2,Y=y2,
                                DIREC=cbind(gx2,gy2),
                                XREF=x1,YREF=y1,
                                ORIENTATION="in-out",
                                RTS="crs")$objval
  c22.io= Benchmarking::dea.direct(X=x2,Y=y2,
                                DIREC=cbind(gx2,gy2),
                                XREF=x2,YREF=y2,
                                ORIENTATION="in-out",
                                RTS="crs")$objval
  
  revenue.cost1=apply(Wx1*X1, 1, sum)+apply(Wy1*Y1, 1, sum)
  Sxy1=revenue.cost1/sum(revenue.cost1)
  revenue.cost2=apply(Wx2*X2, 1, sum)+apply(Wy2*Y2, 1, sum)
  Sxy2=revenue.cost2/sum(revenue.cost2)
  
  L.io[i]=0.5*sum(c11.io*Sxy1-c21.io*Sxy2+c12.io*Sxy1-c22.io*Sxy2)
  
  M.io[i]=((sum((1+c21.io)*Sxy2)/sum((1+c11.io)*Sxy1))*(sum((1+c22.io)*Sxy2)/sum((1+c12.io)*Sxy1)))^(-0.5)
  
  
  # Aggregate LPI With Output Directions
  gx1=matrix(0,nrow=nrow(X1),ncol=ncol(X1))
  gx2=matrix(0,nrow=nrow(X2),ncol=ncol(X2))
  gy1=Y1
  gy2=Y2
  
  c11.ou= Benchmarking::dea.direct(X=x1,Y=y1,
                                   DIREC=cbind(gx1,gy1),
                                   XREF=x1,YREF=y1,
                                   ORIENTATION="in-out",
                                   RTS="crs")$objval
  c12.ou= Benchmarking::dea.direct(X=x1,Y=y1,
                                   DIREC=cbind(gx1,gy1),
                                   XREF=x2,YREF=y2,
                                   ORIENTATION="in-out",
                                   RTS="crs")$objval
  c21.ou= Benchmarking::dea.direct(X=x2,Y=y2,
                                   DIREC=cbind(gx2,gy2),
                                   XREF=x1,YREF=y1,
                                   ORIENTATION="in-out",
                                   RTS="crs")$objval
  c22.ou= Benchmarking::dea.direct(X=x2,Y=y2,
                                   DIREC=cbind(gx2,gy2),
                                   XREF=x2,YREF=y2,
                                   ORIENTATION="in-out",
                                   RTS="crs")$objval
  
  revenue1=apply(Wy1*Y1, 1, sum)
  Sy1=revenue1/sum(revenue1)
  revenue2=apply(Wy2*Y2, 1, sum)
  Sy2=revenue2/sum(revenue2)
  
  L.ou[i]=0.5*sum(c11.ou*Sy1-c21.ou*Sy2+c12.ou*Sy1-c22.ou*Sy2)
  
  
  # Aggregate LPI With Input Directions
  gx1=X1
  gx2=X2
  gy1=matrix(0,nrow=nrow(Y1),ncol=ncol(Y1))
  gy2=matrix(0,nrow=nrow(Y2),ncol=ncol(Y2))
  
  c11.in= Benchmarking::dea.direct(X=x1,Y=y1,
                                   DIREC=cbind(gx1,gy1),
                                   XREF=x1,YREF=y1,
                                   ORIENTATION="in-out",
                                   RTS="crs")$objval
  c12.in= Benchmarking::dea.direct(X=x1,Y=y1,
                                   DIREC=cbind(gx1,gy1),
                                   XREF=x2,YREF=y2,
                                   ORIENTATION="in-out",
                                   RTS="crs")$objval
  c21.in= Benchmarking::dea.direct(X=x2,Y=y2,
                                   DIREC=cbind(gx2,gy2),
                                   XREF=x1,YREF=y1,
                                   ORIENTATION="in-out",
                                   RTS="crs")$objval
  c22.in= Benchmarking::dea.direct(X=x2,Y=y2,
                                   DIREC=cbind(gx2,gy2),
                                   XREF=x2,YREF=y2,
                                   ORIENTATION="in-out",
                                   RTS="crs")$objval
  
  cost1=apply(Wx1*X1, 1, sum)
  Sx1=cost1/sum(cost1)
  cost2=apply(Wx2*X2, 1, sum)
  Sx2=cost2/sum(cost2)
  
  L.in[i]=0.5*sum(c11.in*Sx1-c21.in*Sx2+c12.in*Sx1-c22.in*Sx2)
  
  
  # Aggregate Output-Oriented MPI
  m11.ou= Benchmarking::dea(X=x1,Y=y1,
                            XREF=x1,YREF=y1,
                            ORIENTATION="out",
                            RTS="crs")$objval
  m12.ou= Benchmarking::dea(X=x1,Y=y1,
                            XREF=x2,YREF=y2,
                            ORIENTATION="out",
                            RTS="crs")$objval
  m21.ou= Benchmarking::dea(X=x2,Y=y2,
                            XREF=x1,YREF=y1,
                            ORIENTATION="out",
                            RTS="crs")$objval
  m22.ou= Benchmarking::dea(X=x2,Y=y2,
                            XREF=x2,YREF=y2,
                            ORIENTATION="out",
                            RTS="crs")$objval
  
  #revenue1=apply(Wy1*Y1, 1, sum)
  #s1=revenue1/sum(revenue1)
  #revenue2=apply(Wy2*Y2, 1, sum)
  #s2=revenue2/sum(revenue2)
  
  M.ou[i]=((sum(m21.ou*Sy2)/sum(m11.ou*Sy1))*(sum(m22.ou*Sy2)/sum(m12.ou*Sy1)))^(-0.5)
  
  
  # Aggregate Input-Oriented MPI
  m11.in= Benchmarking::dea(X=x1,Y=y1,
                            XREF=x1,YREF=y1,
                            ORIENTATION="in",
                            RTS="crs")$objval
  m12.in= Benchmarking::dea(X=x1,Y=y1,
                            XREF=x2,YREF=y2,
                            ORIENTATION="in",
                            RTS="crs")$objval
  m21.in= Benchmarking::dea(X=x2,Y=y2,
                            XREF=x1,YREF=y1,
                            ORIENTATION="in",
                            RTS="crs")$objval
  m22.in= Benchmarking::dea(X=x2,Y=y2,
                            XREF=x2,YREF=y2,
                            ORIENTATION="in",
                            RTS="crs")$objval
  
  #cost1=apply(Wx1*X1, 1, sum)
  #s1=cost1/sum(cost1)
  #cost2=apply(Wx2*X2, 1, sum)
  #s2=cost2/sum(cost2)
  
  M.in[i]=((sum(m21.in*Sx2)/sum(m11.in*Sx1))*(sum(m22.in*Sx2)/sum(m12.in*Sx1)))^(0.5)
  
  
}










###############################################################
#### Make as Latex

res=cbind(L.ou,log(M.ou),L.in,log(M.in),L.io,log(M.io))

year1=c(year.start:c(year.end-1))
year2=c((year.start+1):year.end)

tex1=formatC(year1,width=5,digits=0,format="f")
tex2=formatC(year2,width=5,digits=0,format="f")

tex=paste(tex1,"&",tex2)
for (k in 1:ncol(res)) {
  tex = paste(tex,"&",formatC(res[,k],width=7,digits = 4,format = "f"))
}
tex = paste(tex,"\\\\")

write(tex,file="./Output/proposition1.tex")


summary(L.ou-log(M.ou))
summary(L.in-log(M.in))
summary(L.io-log(M.io))




#################  correlations  #################
data <- data.frame(
  L.ou,log(M.ou),L.in,log(M.in),L.io,log(M.io)
)

# Calculate the correlation matrix
cor_matrix <- cor(data, use = "complete.obs", method = "pearson")

print(cor_matrix)





###############################################################
# Plot these six vectors
setEPS()
postscript(file = paste("./Figures/plot-all.eps"))


all.min=min(L.ou,log(M.ou),L.in,log(M.in),L.io,log(M.io))
all.max=max(L.ou,log(M.ou),L.in,log(M.in),L.io,log(M.io))

x=year[-1]
plot(x, L.ou,      type="l",lty=1,lwd=3,col = 1, xaxt='n',ylim=c(all.min,all.max),
     xlab="Year", ylab = "Productivity Indices")
lines(x,log(M.ou), type="l",lty=2,lwd=3,col = 2)
lines(x,L.in,      type="l",lty=3,lwd=3,col = 3)
lines(x,log(M.in), type="l",lty=4,lwd=3,col = 4)
lines(x,L.io,      type="l",lty=5,lwd=3,col = 5)
lines(x,log(M.io), type="l",lty=6,lwd=3,col = 6)


axis(1, at = seq(1960, 2015, by = 5))

legend('topleft',legend = c(expression(hat(L)^o),expression(log(hat(M)^o)), 
                            expression(hat(L)^i),expression(log(hat(M)^i)),
                            expression(hat(L)^io),expression(log(hat(M)^io))), 
       lty=c(1:6), lwd =c(3,3,3,3,3,3), col = c(1:6))

dev.off()

###################################################################



###############################################################
# Plot the three L measures
setEPS()
postscript(file = paste("./Figures/plot-L.eps"))


all.min=min(L.ou,log(M.ou),L.in,log(M.in),L.io,log(M.io))
all.max=max(L.ou,log(M.ou),L.in,log(M.in),L.io,log(M.io))

x=year[-1]
plot(x, L.ou,      type="l",lty=1,lwd=3,col = 1, xaxt='n',ylim=c(all.min,all.max),
     xlab="Year", ylab = "Productivity Indices")
lines(x,L.in,      type="l",lty=3,lwd=3,col = 3)
lines(x,L.io,      type="l",lty=5,lwd=3,col = 5)


axis(1, at = seq(1960, 2015, by = 5))

legend('topleft',legend = c(expression(hat(L)^o),
                            expression(hat(L)^i),
                            expression(hat(L)^io)), 
       lty=c(1,3,5), lwd =c(3,3,3), col = c(1,3,5))

dev.off()

###################################################################




###############################################################
# Plot the L and M measures
setEPS()
postscript(file = paste("./Figures/plot-L-M.eps"))


all.min=min(L.ou,log(M.ou),L.in,log(M.in),L.io,log(M.io))
all.max=max(L.ou,log(M.ou),L.in,log(M.in),L.io,log(M.io))

x=year[-1]

par(mfrow = c(3, 1))

##############################
plot(x, L.ou,      type="l",lty=1,lwd=3,col = 1, xaxt='n',ylim=c(all.min,all.max),
     xlab="Year", ylab = "Productivity Indices")
lines(x,log(M.ou), type="l",lty=2,lwd=3,col = 2)

axis(1, at = seq(1960, 2015, by = 5))

legend('topleft',legend = c(expression(hat(L)^o),
                            expression(log(hat(M)^o))), 
       lty=c(1,2), lwd =c(3,3), col = c(1,2))


##############################
plot(x, L.in,      type="l",lty=3,lwd=3,col = 3, xaxt='n',ylim=c(all.min,all.max),
     xlab="Year", ylab = "Productivity Indices")
lines(x,log(M.in), type="l",lty=4,lwd=3,col = 4)

axis(1, at = seq(1960, 2015, by = 5))

legend('topleft',legend = c(expression(hat(L)^i),
                            expression(log(hat(M)^i))), 
       lty=c(3,4), lwd =c(3,3), col = c(3,4))


##############################
plot(x, L.io,      type="l",lty=5,lwd=3,col = 5, xaxt='n',ylim=c(all.min,all.max),
     xlab="Year", ylab = "Productivity Indices")
lines(x,log(M.io), type="l",lty=6,lwd=3,col = 6)

axis(1, at = seq(1960, 2015, by = 5))

legend('topleft',legend = c(expression(hat(L)^io),
                            expression(log(hat(M)^io))), 
       lty=c(5,6), lwd =c(3,3), col = c(5,6))



dev.off()

###################################################################









