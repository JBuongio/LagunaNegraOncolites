#March 2019
#Joy Buongiorno
#JBuongior21@gmail.com
#Analysis of organic geochemistry and elemental analysis

library(ggplot2)
library(RColorBrewer)
org_dat<-read.csv("Organic_geochem.csv")
head(org_dat)

CN_plot<-ggplot(org_dat, aes(x=C.N, y=d13C)) +
  geom_point(aes(fill=Sample,), colour="black", pch=21, size=4, stroke=2) +
  theme(text = element_text(size=30)) +
  scale_fill_manual(values=c("LN-13-1" = "#E69F00", "LN-13-5"= "#56B4E9", "LN-13-10" = "#009E73", "LN-13-13"= "#F0E442", "LN-13-20" ="#0072B2", "LN-13-28" ="#CC79A7")) +
  scale_color_manual(values=c("LN-13-1" = "#E69F00", "LN-13-5"= "#56B4E9", "LN-13-10" = "#009E73", "LN-13-13"= "#F0E442", "LN-13-20" ="#0072B2", "LN-13-28" ="#CC79A7")) + 
  scale_y_continuous(limits = c(-30,-5)) +
  labs(y=expression(paste(delta^{13}, "C (\u2030 vs. PDB)")))

####### Elements, cross plots ##################

elem_data<-read.csv("Elements_geobio_for_R.csv")
head(elem_data)

Sr_plot<-ggplot(elem_data, aes(x=d18O, y=Sr)) +
  geom_point(aes(fill=Sample,), colour="black", pch=21, size=4, stroke=2) +
  theme(text = element_text(size=30)) +
  scale_fill_manual(values=c("LN-13-1" = "#E69F00", "LN-13-5"= "#56B4E9", "LN-13-10" = "#009E73", "LN-13-13"= "#F0E442", "LN-13-20" ="#0072B2", "LN-13-28" ="#CC79A7")) +
  scale_color_manual(values=c("LN-13-1" = "#E69F00", "LN-13-5"= "#56B4E9", "LN-13-10" = "#009E73", "LN-13-13"= "#F0E442", "LN-13-20" ="#0072B2", "LN-13-28" ="#CC79A7")) + 
  labs(y="[Sr] ppm", x=expression(paste(delta^{18}, "O (\u2030 vs. PDB)"))) +
  scale_x_continuous(labels = function(x) ifelse(x>0, paste0("+", x), x)) 


Mg_plot<-ggplot(elem_data, aes(x=d18O, y=Mg)) +
  geom_point(aes(fill=Sample,), colour="black", pch=21, size=4, stroke=2) +
  theme(text = element_text(size=30)) +
  scale_fill_manual(values=c("LN-13-1" = "#E69F00", "LN-13-5"= "#56B4E9", "LN-13-10" = "#009E73", "LN-13-13"= "#F0E442", "LN-13-20" ="#0072B2", "LN-13-28" ="#CC79A7")) +
  scale_color_manual(values=c("LN-13-1" = "#E69F00", "LN-13-5"= "#56B4E9", "LN-13-10" = "#009E73", "LN-13-13"= "#F0E442", "LN-13-20" ="#0072B2", "LN-13-28" ="#CC79A7")) + 
  labs(y="[Mg] ppm", x=expression(paste(delta^{18}, "O (\u2030 vs. PDB)"))) +
  scale_x_continuous(labels = function(x) ifelse(x>0, paste0("+", x), x)) 

####### Elements, time-series ##################
Elem_1_datc<-subset(elem_data, Sample %in% c("LN-13-1"))
Elem_5_datc<-subset(elem_data, Sample %in% c("LN-13-5"))
head(Elem_5_datc)
Elem_10_datc<-subset(elem_data, Sample %in% c("LN-13-10"))
Elem_13_datc<-subset(elem_data, Sample %in% c("LN-13-13"))
Elem_20_datc<-subset(elem_data, Sample %in% c("LN-13-20"))
Elem_28_datc<-subset(elem_data, Sample %in% c("LN-13-28"))

############ LN-13-1 ################
Elem_1_Mg<- ggplot(data= Elem_1_datc, aes(x=Layer, y=Mg)) +
  geom_point(aes(fill=Sample),pch=21, colour = "black", size=5) +
  geom_line() +
  labs(x="Layer from outside", y="[Mg] (ppm) * 1000") +
  scale_y_continuous(limits=c(0, 25000), labels = function(x)x/1000)+
  theme(text = element_text(size=30)) +
  scale_x_reverse() +
  coord_flip()

Elem_1_Sr<- ggplot(data= Elem_1_datc, aes(x=Layer, y=Sr)) +
  geom_point(aes(fill=Sample),pch=21, colour = "black", size=5) +
  scale_fill_manual(values=c("blue")) +
  geom_line() +
  labs(x="Layer from outside", y="[Sr] (ppm)") +
  theme(text = element_text(size=30)) +
  scale_y_continuous(limits=c(0, 8000))+
  scale_x_reverse() +
  coord_flip()

############ LN-13-5 ################

Elem_5_Mg<- ggplot(data= Elem_5_datc, aes(x=Layer, y=Mg)) +
  geom_point(aes(fill=Sample),pch=21, colour = "black", size=5) +
  geom_line() +
  labs(x="Layer from outside", y="[Mg] (ppm) * 1000") +
  scale_y_continuous(limits=c(0, 25000), labels = function(x)x/1000)+
  theme(text = element_text(size=30)) +
  scale_x_reverse() +
  coord_flip()

Elem_5_Sr<- ggplot(data= Elem_5_datc, aes(x=Layer, y=Sr)) +
  geom_point(aes(fill=Sample),pch=21, colour = "black", size=5) +
  scale_fill_manual(values=c("blue")) +
  geom_line() +
  labs(x="Layer from outside", y="[Sr] (ppm)") +
  theme(text = element_text(size=30)) +
  scale_y_continuous(limits=c(0, 8000))+
  scale_x_reverse() +
  coord_flip()

############ LN-13-10 ################

Elem_10_Mg<- ggplot(data= Elem_10_datc, aes(x=Layer, y=Mg)) +
  geom_point(aes(fill=Sample),pch=21, colour = "black", size=5) +
  geom_line() +
  labs(x="Layer from outside", y="[Mg] (ppm) * 1000") +
  scale_y_continuous(limits=c(0, 25000), labels=function(x)x/1000)+
  theme(text = element_text(size=30)) +
  scale_x_reverse() +
  coord_flip()

Elem_10_Sr<- ggplot(data= Elem_10_datc, aes(x=Layer, y=Sr)) +
  geom_point(aes(fill=Sample),pch=21, colour = "black", size=5) +
  scale_fill_manual(values=c("blue")) +
  geom_line() +
  labs(x="Layer from outside", y="[Sr] (ppm)") +
  theme(text = element_text(size=30)) +
  scale_y_continuous(limits=c(0, 8000))+
  scale_x_reverse() +
  coord_flip()

############ LN-13-13 ################

Elem_13_Mg<- ggplot(data= Elem_13_datc, aes(x=Layer, y=Mg)) +
  geom_point(aes(fill=Sample),pch=21, colour = "black", size=5) +
  geom_line() +
  labs(x="Layer from outside", y="[Mg] (ppm) * 1000") +
  scale_y_continuous(limits=c(0, 25000), labels=function(x)x/1000)+
  theme(text = element_text(size=30)) +
  scale_x_reverse() +
  coord_flip()

Elem_13_Sr<- ggplot(data= Elem_13_datc, aes(x=Layer, y=Sr)) +
  geom_point(aes(fill=Sample),pch=21, colour = "black", size=5) +
  scale_fill_manual(values=c("blue")) +
  geom_line() +
  labs(x="Layer from outside", y="[Sr] (ppm)") +
  theme(text = element_text(size=30)) +
  scale_y_continuous(limits=c(0, 8000))+
  scale_x_reverse() +
  coord_flip()

############ LN-13-20 ################

Elem_20_Mg<- ggplot(data= Elem_20_datc, aes(x=Layer, y=Mg)) +
  geom_point(aes(fill=Sample),pch=21, colour = "black", size=5) +
  geom_line() +
  labs(x="Layer from outside", y="[Mg] (ppm) * 1000") +
  scale_y_continuous(limits=c(0, 25000), labels=function(x)x/1000)+
  theme(text = element_text(size=30)) +
  scale_x_reverse() +
  coord_flip()

Elem_20_Sr<- ggplot(data= Elem_20_datc, aes(x=Layer, y=Sr)) +
  geom_point(aes(fill=Sample),pch=21, colour = "black", size=5) +
  scale_fill_manual(values=c("blue")) +
  geom_line() +
  labs(x="Layer from outside", y="[Sr] (ppm)") +
  theme(text = element_text(size=30)) +
  scale_y_continuous(limits=c(0, 8000))+
  scale_x_reverse() +
  coord_flip()

############ LN-13-28 ################

Elem_28_Mg<- ggplot(data= Elem_28_datc, aes(x=Layer, y=Mg)) +
  geom_point(aes(fill=Sample),pch=21, colour = "black", size=5) +
  geom_line() +
  labs(x="Layer from outside", y="[Mg] (ppm) * 1000") +
  scale_y_continuous(limits=c(0, 25000), labels=function(x)x/1000)+
  theme_bw(base_size=30) +
  scale_x_reverse() +
  coord_flip()

Elem_28_Sr<- ggplot(data= Elem_28_datc, aes(x=Layer, y=Sr)) +
  geom_point(aes(fill=Sample),pch=21, colour = "black", size=5) +
  scale_fill_manual(values=c("blue")) +
  geom_line() +
  labs(x="Layer from outside", y="[Sr] (ppm)") +
  theme_bw(base_size=30) +
  scale_y_continuous(limits=c(0, 8000))+
  scale_x_reverse() +
  coord_flip()

