#############################################################################################
## Aggregate Luenberger Productivity Indicators and Its Decompositions: Theory and Application
## Valentin Zelenyuk and Shirong Zhao
#############################################################################################

rm(list=ls())
require(readxl)
require(Benchmarking)
#### Set Seed ####
if (exists(".Random.seed")) {
  save.seed=.Random.seed
  flag.seed=TRUE
} else {
  flag.seed=FALSE
}
set.seed(90001)



###############################################################
df <- read.csv("RawData/Table02StateProductivityAccounts.csv")
###############################################################

varnames=c("Total agricultural output price index", 
"Total agricultural output quantity", 
"Livestock and products price index",
"Livestock and products quantity", 
"Crops price index",
"Crops quantity", 
"Farm related output price index",
"Farm related output quantity",
"Total input price index",
"Total input quantity", 
"Capital services excluding land price index",         
"Capital services excluding land quantity",
"Land service flows price index",                      
"Land service flows quantity", 
"Labor input price index",                             
"Labor input quantity", 
"Total intermediate inputs price index",               
"Total intermediate inputs quantity") 

df.new=0
for (i in 1:length(varnames)) {
  
  varname=varnames[i]
  ii=which(df$Variable.Name==varname)
  df.new=cbind(df.new,df[ii,7])
  
}

df.new=df.new[,-1]
df.new=cbind(df[ii,c(4,6)],df.new)

colnames(df.new)=c("state","year","p","y","p1","y1","p2","y2","p3","y3","w","x","w1","x1","w2","x2","w3","x3","w4","x4")
df.new=as.data.frame(df.new)



write.csv(df.new,"Data/data.csv",row.names = FALSE)









