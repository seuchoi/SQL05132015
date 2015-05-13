# Cool!

setwd("/restricted/projectnb/brainseq/seuchoi/T1Cov/Result/TypeI/Balanced")
data1<-read.csv("TypeI_Balanced_Combined_AdoptAsp_Result_Empirical_Threshold_05082015.csv",header=T,as.is=T)
setwd("/restricted/projectnb/brainseq/seuchoi/T1Cov/Result/TypeI/Unbalanced")
data2<-read.csv("TypeI_Unbalanced_Combined_AdoptAsp_Result_Empirical_Threshold_08052015.csv",header=T,as.is=T)

data3<-rbind(data1,data2)


data4<-subset(data3,nsim!=0)
subset(data4,is.na(data4$EmpT_NB_QLD_a0.05))

#res1<-lm(NB.TD.Scale ~ ncase + ncont + disp + Ncov ,data=data4)
#summary(res1)
#data4$NB.TD.Scale.hat<-res1$fitted.values

#res2<-lm(NB.TD.Loc ~ ncase + ncont + disp + Ncov ,data=data4)
#summary(res2)
#data4$NB.TD.Loc.hat<-res2$fitted.values

#############
#############
#############

res1.1<-lm(NB.TD.Scale ~ ncase + I(ncase^2) + ncont + I(ncont^2) + disp + Ncov ,data=data4)
summary(res1.1)
data4$NB.TD.Scale.hat<-res1.1$fitted.values

res2.1<-lm(NB.TD.Loc ~ ncase + I(ncase^2) + ncont +I(ncont^2) + disp + Ncov ,data=data4)
summary(res2.1)
data4$NB.TD.Loc.hat<-res2.1$fitted.values

#############
#############
#############

#res3<-lm(FL.Scale ~ ncase + ncont + disp + Ncov ,data=data4)
#summary(res3)

#res4<-lm(FL.Loc ~ ncase + ncont + disp + Ncov ,data=data4)
#summary(res4)

res3.1<-lm(FL.Scale ~ ncase + I(ncase^2) + ncont +I(ncont^2) + disp + Ncov ,data=data4)
summary(res3.1)
data4$FL.Scale.hat<-res3.1$fitted.values

res4.1<-lm(FL.Loc ~ ncase + I(ncase^2) + ncont +I(ncont^2) + disp + Ncov ,data=data4)
summary(res4.1)
data4$FL.Loc.hat<-res4.1$fitted.values

data5<-data4[,c(names(data4)[1:20],names(data4)[45:48])]
setwd("/restricted/projectnb/brainseq/seuchoi/T1Cov/Result/TypeI/")

write.table(data5,"TypeI_Balanced_Unbalanced_Combined_AdoptAsp_Result_Empirical_Parameter_05082015.csv",col.names=T,row.names=F,sep=",",quote=F,na="")



######################
###################### test

setwd("/restricted/projectnb/brainseq/seuchoi/T1Cov/Result/TypeI/")
data5[1:4,]

png("Ncase_vs_NB.TD.Scale.png")
plot(data5$ncase,data5$NB.TD.Scale)
dev.off()

png("logNcase_vs_NB.TD.Scale.png")
plot(log(data5$ncase),data5$NB.TD.Scale)
dev.off()

######

png("Ncont_vs_NB.TD.Scale.png")
plot(data5$ncont,data5$NB.TD.Scale)
dev.off()

png("logNcont_vs_NB.TD.Scale.png")
plot(log(data5$ncont),data5$NB.TD.Scale)
dev.off()


######



png("disp_vs_NB.TD.Scale.png")
plot(data5$disp,data5$NB.TD.Scale)
dev.off()

png("Ncov_vs_NB.TD.Scale.png")
plot(data5$Ncov,data5$NB.TD.Scale)
dev.off()

##############
##############
png("Ncase_vs_NB.TD.Loc.png")
plot(data5$ncase,data5$NB.TD.Loc)
dev.off()

png("logNcase_vs_NB.TD.Loc.png")
plot(log(data5$ncase),data5$NB.TD.Loc)
dev.off()


###############
png("Ncont_vs_NB.TD.Loc.png")
plot(data5$ncont,data5$NB.TD.Loc)
dev.off()

png("logNcont_vs_NB.TD.Loc.png")
plot(log(data5$ncont),data5$NB.TD.Loc)
dev.off()





png("disp_vs_NB.TD.Loc.png")
plot(data5$disp,data5$NB.TD.Loc)
dev.off()

png("Ncov_vs_NB.TD.Loc.png")
plot(data5$Ncov,data5$NB.TD.Loc)
dev.off()

##############
##############

png("NB.TD.Scale.hat_vs_NB.TD.Loc.png")
plot(data5$NB.TD.Scale.hat,data5$NB.TD.Loc)
dev.off()


####################
####################
setwd("/restricted/projectnb/brainseq/seuchoi/T1Cov/Result/TypeI/")
png("Ncase_vs_FL.Scale.png")
plot(data4$ncase,data4$FL.Scale)
dev.off()

png("Ncont_vs_FL.Scale.png")
plot(data4$ncont,data4$FL.Scale)
dev.off()

png("disp_vs_NB.TD.Scale.png")
plot(data5$disp,data5$NB.TD.Scale)
dev.off()

png("Ncov_vs_NB.TD.Scale.png")
plot(data5$Ncov,data5$NB.TD.Scale)
dev.off()


