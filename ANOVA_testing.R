#May 2019
#Joy Buongiorno
#JBuongior21@gmail.com
#Determining normality of data, post-hoc Tukey testing, and ANOVA for time-series.

iso.1<-read.table("Isotopes_LN.13.1.txt", header= TRUE, sep="\t")
qqnorm(iso.1[,5]) #test for normality with plot
shapiro.test(iso.1[,5]) #test for normality. Non normal data is significant at 0.05 alpha.Data is normal.
model.1<-lm(d18O~Layer, data=iso.1) #linear model, wherein oxy isotopes are a response of time.
model.1.covar<-lm(d18O~d13C, data=iso.1)#linear model, wherein oxy and carbon isotopes are responding to each other. 
anova(model.1.covar) #gives P value of R2 value
summary(model.1.covar) #gives R2 value
library(Hmisc)
cor(iso.1$d13C, iso.1$d18O, method = "spearman") #gives rs value
iso.1_sig<-rcorr(iso.1$d13C, iso.1$d18O, type = "spearman") #another way to get rs value
iso.1_sig$P #provides P-value with greater resolution.
anova(model.1)
library(agricolae) #library for Tukey testing.
oxy_iso.1<-iso.1$d18O #making data frame with just oxy isotopes from sample LN-13-1.
carb_iso.1<-iso.1$d13C #making data frame with just carb isotopes from LN-13-1.
rel_time.1<-iso.1$Org_name #making data frame with column from iso.1 that denotes relative time.
LN.1_oxy<-lm(oxy_iso.1 ~ rel_time.1) #linear model wherein oxy isotopes are explained by relative time.
LN.1_carb<-lm(carb_iso.1~rel_time.1) #linear model wherein carb isotopes are explained by relative time.
out<-HSD.test(LN.1_oxy, "rel_time.1", group = TRUE) #Tukey testing, values of oxy isotopes, as grouped by same relative time.
out
out_carb<-HSD.test(LN.1_carb, "rel_time.1", group=TRUE) #Tukey testing, values of carb isotopes, as grouped by same relative time.

iso.5<-read.table("Isotopes_LN.13.5.txt", header=TRUE, sep="\t")
qqnorm(iso.5[,5])
shapiro.test(iso.5[,5])
model.5.covar<-lm(d18O~d13C, data = iso.5)
anova(model.5.covar)
summary(model.5.covar)
model.5<-lm(d18O~Layer, data=iso.5)
anova(model.5)
iso.5_nona<-na.omit(iso.5)
cor(iso.5_nona$d13C, iso.5_nona$d18O, method = "spearman")
iso.5_sig<-rcorr(iso.5_nona$d13C, iso.5_nona$d18O, type="spearman")
iso.5_sig$P
oxy_iso.5<-iso.5$d18O
carb_iso.5<-iso.5$d13C
rel_time.5<-iso.5$Org_name
LN.5_oxy<-lm(oxy_iso.5~rel_time.5)
LN.5_carb<-lm(carb_iso.5~rel_time.5)
out.5<-HSD.test(LN.5_oxy, "rel_time.5", group = TRUE)
out.5
out.5_carb<-HSD.test(LN.5_carb, "rel_time.5", group=TRUE)

iso.10<-read.table("Isotopes_LN.13.10.txt", header=TRUE, sep="\t")
qqnorm(iso.10[,5])
shapiro.test(iso.10[,5])
model.10.covar<-lm(d18O~d13C, data=iso.10)
anova(model.10.covar)
summary(model.10.covar)
model.10<-lm(d18O~Layer, data=iso.10)
anova(model.10)
iso.10_sig<-rcorr(iso.10$d13C, iso.10$d18O, type = "spearman")
iso.10_sig$P
oxy_iso.10<-iso.10$d18O
carb_iso.10<-iso.10$d13C
rel_time.10<-iso.10$Org_name
LN.10_oxy<-lm(oxy_iso.10~rel_time.10)
LN.10_carb<-lm(carb_iso.10~rel_time.10)
out.10<-HSD.test(LN.10_oxy, "rel_time.10", group = TRUE)
out.10
out.10_carb<-HSD.test(LN.10_carb, "rel_time.10", group = TRUE)

iso.13<-read.table("Isotopes_LN.13.13.txt", header=TRUE, sep="\t")
qqnorm(iso.13[,5])
shapiro.test(iso.13[,5])
tran_iso<-log10(iso.13[,5]) #not able to make normal at alpha=0.05. Data not used for ANOVA/Tukey testing.
qqnorm(tran_iso)
shapiro.test(tran_iso)
model.13.covar<-lm(d18O~d13C, data = iso.13)
anova(model.13.covar)
summary(model.13.covar)
model.13<-lm(d18O~Layer+Lamina_type, data=iso.13) 
anova(model.13) #not reported
iso.13_sig<-rcorr(iso.13$d13C, iso.13$d18O, type="spearman")
iso.13_sig$P
oxy_iso.13<-iso.13$d18O
carb_iso.13<-iso.13$d13C
rel_time.13<-iso.13$Org_name
LN.13_oxy<-lm(oxy_iso.13~rel_time.13)
LN.13_carb<-lm(carb_iso.13~rel_time.13)
out.13<-HSD.test(LN.13_oxy, "rel_time.13", group = TRUE)
out.13 #not reported
out.13_carb<-HSD.test(LN.13_carb, "rel_time.13", group=TRUE)

iso.20<-read.table("Isotopes_LN.13.20.txt", header=TRUE, sep="\t")
qqnorm(iso.20[,5])
shapiro.test(iso.20[,5])
model.20.covar<-lm(d18O~d13C, data = iso.20)
anova(model.20.covar)
summary(model.20.covar)
model.20<-lm(d18O~Layer, data=iso.20)
anova(model.20)
oxy_iso.20<-iso.20$d18O
carb_iso.20<-iso.20$d13C
iso.20_sig<-rcorr(iso.20$d13C, iso.20$d18O, type="spearman")
rel_time.20<-iso.20$Org_name
LN.20_oxy<-lm(oxy_iso.20~rel_time.20)
LN.20_carb<-lm(carb_iso.20~rel_time.20)
out.20<-HSD.test(LN.20_oxy, "rel_time.20", group=TRUE)
out.20
out.20_carb<-HSD.test(LN.20_carb, "rel_time.20", group=TRUE)
iso.20_4pt<-read.table("Isotopes_LN.13.20_4pt.txt", header=TRUE, sep="\t") #testing to see if grouping by subsets of four laminae will provide different Tukey results.
oxy_4pt<-iso.20_4pt$d18O #making data frame of grouped oxy values.
Groupings.20<-iso.20_4pt$Grouping #making data frame of groupings.
four.pt_model<-lm(oxy_4pt~Groupings.20) #linear model, wherein oxy values are explained by groupings. 
four.out.20<-HSD.test(four.pt_model, "Groupings.20", group=TRUE) #Tukey testing, values of oxy, as grouped by "groupings".

iso.28<-read.table("Isotopes_LN.13.28.txt", header=TRUE, sep="\t")
qqnorm(iso.28[,5])
shapiro.test(iso.28[,5])
model.28.covar<-lm(d18O~d13C, data = iso.28)
anova(model.28.covar)
summary(model.28.covar)
model.28<-lm(d18O~Layer, data=iso.28)
anova(model.28)
iso.28_sig<-rcorr(iso.28$d13C, iso.28$d18O, type="spearman")
iso.28_sig$P
oxy_iso.28<-iso.28$d18O
carb_iso.28<-iso.28$d13C
rel_time.28<-iso.28$Org_name
LN.28_oxy<-lm(oxy_iso.28~rel_time.28)
LN.28_carb<-lm(carb_iso.28~rel_time.28)
out.28<-HSD.test(LN.28_oxy, "rel_time.28", group = TRUE)
out.28
out.28_carb<-HSD.test(LN.28_carb, "rel_time.28", group=TRUE)
oxy_iso.28.nona<-na.omit(oxy_iso.28) #removing NA values from oxy data
range(oxy_iso.28.nona) #finding range of 3A oxy values
carb_iso.28.nona<-na.omit(carb_iso.28) #removing NA values from carb data
range(carb_iso.28.nona) #finding range of 3A carb values

carb_iso.5_nona<-na.omit(iso.5$d13C) #removing all NA values
carb_iso.20_nona<-na.omit(iso.20$d13C) #removing all NA values
threeC_carb<-cbind(carb_iso.1, carb_iso.5_nona, carb_iso.10, carb_iso.13, carb_iso.20_nona) #binding all zone 3C carb values together.
range(threeC_carb) #finding range of 3C carb values.
oxy_iso.5_nona<-na.omit(iso.5$d18O) #removing all NA values.
oxy_iso.20_nona<-na.omit(iso.20$d18O) #removing all NA values.
threeC_oxy<-cbind(oxy_iso.1, oxy_iso.5_nona, oxy_iso.10, oxy_iso.13, oxy_iso.20_nona) #binding all zone 3C oxy values together.
range(threeC_oxy) #finding range of 3C oxy values.

all_carb_iso<-read.table("all_carb_iso.txt", header=TRUE, sep="\t")
all_oxy_iso<-read.table("all_oxy_iso.txt", header=TRUE, sep="\t")
all_iso<-cbind(all_carb_iso, all_oxy_iso) #binding all carb and oxy values together (from both zones)
shapiro.test(all_iso$d13C) #testing for normality of carb values
shapiro.test(all_iso$d18O) #testing for normality of oxy values
model.all_covar<-lm(all_iso$d13C~all_iso$d18O) #linear model, wherein carb and oxy values respond together.
anova(model.all_covar) #gives P value of R2 value of entire dataset.
summary(model.all_covar) #gives R2 of entire dataset
all_corr<-rcorr(all_iso$d13C, all_iso$d18O, type = "spearman")
all_corr$P
