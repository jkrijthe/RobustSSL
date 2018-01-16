#Using this function we can generate Figure 1a from Raudys & Duin 1998.

source("asymptotic.R")

Ns <- seq(2,25,1)
EPn <- vapply(Ns,function(x){raudys(4.65,x,50,rep=1000)},1)
df_raudys <- data.frame(N=Ns,error=EPn)

Ns <- seq(2,25,1)
EPn <- vapply(Ns,function(x){raudys(4.65,x,50,type="total",rep=1000)},1)
df_raudys_ext <- data.frame(N=Ns,error=EPn)

Ns <- seq(5,25,1)
EPn <- vapply(Ns,function(x){raudys_fixed(4.65,x,50,type="total",rep=1000)},1)
df_raudys_fixed <- data.frame(N=Ns,error=EPn)

Ns <- seq(25,40,1)
EPn <- vapply(Ns,function(x){raudys_big(4.65,x,50)},1)
df_raudys_big <- data.frame(N=Ns,error=EPn)

Ns <- seq(25,40,1)
EPn <- vapply(Ns,function(x){raudys_big(4.65,x,50,N_l=5)},1)
df_raudys_continued_semi <- data.frame(N=Ns,error=EPn)

lc_asymptotic <- bind_rows(list("Within"=rbind(df_raudys),"Total"=df_raudys_ext,"Semi-supervised"=df_raudys_fixed, "Continued"=df_raudys_big,"Continued Semi"=df_raudys_continued_semi),.id="name")

save(lc_asymptotic,file="asymptotic_curves.RData")
