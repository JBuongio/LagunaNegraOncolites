#March 2019
#Joy Buongiorno
#JBuongior21@gmail.com
#Analysis of ndividual oncolite carbonate isotope values

library(ggplot2)
iso_cross_cols<-c( "d18O" = "blue", "d13C" = "red")

########### LN-13-1 #################
LN.13.1<-read.csv("Isotopes_LN.13.1.txt", header=TRUE, sep="\t")

carbon_iso_1<- ggplot(data= LN.13.1, aes(x=Layer, y=d13C)) +
  geom_point(aes(fill=d13C), pch=23, colour = "black", size=5) +
  labs(x="Layer from outside", y=expression(paste(delta^{13}, "C (\u2030 vs. PDB)"))) +
  scale_y_continuous(limits=c(6,16), labels = function(x) ifelse(x>0, paste0("+", x), x)) +
  theme(text = element_text(size=30)) +
  scale_fill_gradient(low = "lightblue1", high= "midnightblue") +
  scale_x_reverse() +
  coord_flip()

oxygen_iso_1<- ggplot(data= LN.13.1, aes(x=Layer, y=d18O)) +
  geom_point(aes(fill=d18O), pch=21, colour = "black", size=5) +
  labs(x="Layer from outside", y=expression(paste(delta^{18}, "O (\u2030 vs. PDB)"))) +
  scale_y_continuous(limits=c(-3,6), labels = function(x) ifelse(x>0, paste0("+", x), x)) +
  theme(text = element_text(size=30)) +
  scale_fill_gradient(low = "mistyrose", high= "orangered4") +
  scale_x_reverse() +
  coord_flip()

########### LN-13-5 #################
LN.13.5<-read.csv("Isotopes_LN.13.5.txt", header=TRUE, sep="\t")

carbon_iso_5<- ggplot(data= LN.13.5, aes(x=Layer, y=d13C)) +
  geom_point(aes(fill=d13C), pch=23, colour = "black", size=5) +
  labs(x="Layer from outside", y=expression(paste(delta^{13}, "C (\u2030 vs. PDB)"))) +
  scale_y_continuous(limits=c(6,16), labels = function(x) ifelse(x>0, paste0("+", x), x)) +
  theme(text = element_text(size=30)) +
  scale_fill_gradient(low = "lightblue1", high= "midnightblue") +
  scale_x_reverse() +
  coord_flip()

oxygen_iso_5<- ggplot(data= LN.13.5, aes(x=Layer, y=d18O)) +
  geom_point(aes(fill=d18O), pch=21, colour = "black", size=5) +
  labs(x="Layer from outside", y=expression(paste(delta^{18}, "O (\u2030 vs. PDB)"))) +
  scale_y_continuous(limits=c(-3,6), labels = function(x) ifelse(x>0, paste0("+", x), x)) +
  theme(text = element_text(size=30)) +
  scale_fill_gradient(low = "mistyrose", high= "orangered4") +
  scale_x_reverse() +
  coord_flip()

########### LN-13-10 #################
LN.13.10<-read.csv("Isotopes_LN.13.10.txt", header=TRUE, sep="\t")

carbon_iso_10<- ggplot(data= LN.13.10, aes(x=Layer, y=d13C)) +
  geom_point(aes(fill=d13C), pch=23, colour = "black", size=5) +
  labs(x="Layer from outside", y=expression(paste(delta^{13}, "C (\u2030 vs. PDB)"))) +
  scale_y_continuous(limits=c(6,16), labels = function(x) ifelse(x>0, paste0("+", x), x)) +
  theme(text = element_text(size=30)) +
  scale_fill_gradient(low = "lightblue1", high= "midnightblue") +
  scale_x_reverse() +
  coord_flip()

oxygen_iso_10<- ggplot(data= LN.13.10, aes(x=Layer, y=d18O)) +
  geom_point(aes(fill=d18O), pch=21, colour = "black", size=5) +
  labs(x="Layer from outside", y=expression(paste(delta^{18}, "O (\u2030 vs. PDB)"))) +
  scale_y_continuous(limits=c(-3,6), labels = function(x) ifelse(x>0, paste0("+", x), x)) +
  theme(text = element_text(size=30)) +
  scale_fill_gradient(low = "mistyrose", high= "orangered4") +
  scale_x_reverse() +
  coord_flip()

########### LN-13-13 #################
LN.13.13<-read.csv("Isotopes_LN.13.13.txt", header=TRUE, sep="\t")

carbon_iso_13<- ggplot(data= LN.13.13, aes(x=Layer, y=d13C)) +
  geom_point(aes(fill=d13C), pch=23, colour = "black", size=5) +
  labs(x="Layer from outside", y=expression(paste(delta^{13}, "C (\u2030 vs. PDB)"))) +
  scale_y_continuous(limits=c(6,16), labels = function(x) ifelse(x>0, paste0("+", x), x)) +
  theme(text = element_text(size=30)) +
  scale_fill_gradient(low = "lightblue1", high= "midnightblue") +
  scale_x_reverse() +
  coord_flip()

oxygen_iso_13<- ggplot(data= LN.13.13, aes(x=Layer, y=d18O)) +
  geom_point(aes(fill=d18O), pch=21, colour = "black", size=5) +
  labs(x="Layer from outside", y=expression(paste(delta^{18}, "O (\u2030 vs. PDB)"))) +
  scale_y_continuous(limits=c(-3,6), labels = function(x) ifelse(x>0, paste0("+", x), x)) +
  theme(text = element_text(size=30)) +
  scale_fill_gradient(low = "mistyrose", high= "orangered4") +
  scale_x_reverse() +
  coord_flip()

########### LN-13-20 #################
LN.13.20<-read.csv("Isotopes_LN.13.20.txt", header=TRUE, sep="\t")

carbon_iso_20<- ggplot(data= LN.13.20, aes(x=Layer, y=d13C)) +
  geom_point(aes(fill=d13C), pch=23, colour = "black", size=5) +
  labs(x="Layer from outside", y=expression(paste(delta^{13}, "C (\u2030 vs. PDB)"))) +
  scale_y_continuous(limits=c(6,16), labels = function(x) ifelse(x>0, paste0("+", x), x)) +
  theme(text = element_text(size=30)) +
  scale_fill_gradient(low = "lightblue1", high= "midnightblue") +
  scale_x_reverse() +
  coord_flip()

oxygen_iso_20<- ggplot(data= LN.13.20, aes(x=Layer, y=d18O)) +
  geom_point(aes(fill=d18O), pch=21, colour = "black", size=5) +
  labs(x="Layer from outside", y=expression(paste(delta^{18}, "O (\u2030 vs. PDB)"))) +
  scale_y_continuous(limits=c(-3,6), labels = function(x) ifelse(x>0, paste0("+", x), x)) +
  theme(text = element_text(size=30)) +
  scale_fill_gradient(low = "mistyrose", high= "orangered4") +
  scale_x_reverse() +
  coord_flip()

########### LN-13-28 #################
LN.13.28<-read.csv("Isotopes_LN.13.28.txt", header=TRUE, sep="\t")

carbon_iso_28<- ggplot(data= LN.13.28, aes(x=Layer, y=d13C)) +
  geom_point(aes(fill=d13C), pch=23, colour = "black", size=5) +
  labs(x="Layer from outside", y=expression(paste(delta^{13}, "C (\u2030 vs. PDB)"))) +
  scale_y_continuous(limits=c(6,20),labels = function(x) ifelse(x>0, paste0("+", x), x)) +
  theme_bw(base_size=30) +
  scale_fill_gradient(low = "lightblue1", high= "midnightblue") +
  scale_x_reverse() +
  coord_flip()

oxygen_iso_28<- ggplot(data= LN.13.28, aes(x=Layer, y=d18O)) +
  geom_point(aes(fill=d18O), pch=21, colour = "black", size=5) +
  labs(x="Layer from outside", y=expression(paste(delta^{18}, "O (\u2030 vs. PDB)"))) +
  scale_y_continuous(limits=c(0,10), labels = function(x) ifelse(x>0, paste0("+", x), x)) +
  theme_bw(base_size=30) +
  scale_fill_gradient(low = "mistyrose", high= "orangered4") +
  scale_x_reverse() +
  coord_flip()
