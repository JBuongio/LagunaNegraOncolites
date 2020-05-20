#May 2019
#Joy Buongiorno
#JBuongior21@gmail.com
#Dscript for plotting C and O isotopes of carbonate

library(ggplot2)
carbonate_iso<-read.csv("Carbonate_isotopes_for_R.csv")
carb_cols<-c("El Peinodo basin" = "black","Las Peladas basin" = "black", "San Francisco basin" = "black", "LN-13-1, this study" = "#E69F00", "LN-13-5, this study"= "#56B4E9", "LN-13-10, this study" = "#009E73", "LN-13-13, this study" = "#F0E442", "LN-13-20, this study" ="#0072B2", "LN-13-28, this study" ="#CC79A7")


head(carbonate_iso)
Crossplot<-ggplot(carbonate_iso, aes(x=Oxygen, y=Carbon, shape=Basin, color=Carbonate.source)) +
  geom_point(aes(pch=Basin, fill=Carbonate.source),colour = "black", size=5) +
  scale_shape_manual(values = c(23, 21, 24, 25)) +
  scale_colour_manual(values=carb_cols) +
  scale_fill_manual(values=carb_cols) +
  theme(text = element_text(size=20)) +
  geom_smooth(method="lm",se=FALSE) +
  labs(x=expression(paste(delta^{18}, "O (\u2030 vs. PDB)")), y=expression(paste(delta^{13}, "C (\u2030 vs. PDB)")))
                    