setwd("~/Desktop/FIELD SCIENCE/Individual Project/LABS/Lab3/Data")

library ("tidyverse")
library(tidyverse)
library(ggplot2)
library(ggpubr)
install.packages("stringr")
library(stringr)
install.packages("scales")
library(scales)
#read csv
DATA=read.csv("DaRTS_discrete_data_poster.csv")

#vvsb plot colored by year 
vvsb2=ggplot(DATA, aes(x=Bacteria_cells_mL,y=Virus_cells_mL,color=as.factor(Year))) + 
  geom_point()+
  scale_colour_manual(values=c("red","blue","green","yellow", "pink", "lightblue", "gold", "chartreuse")) +
  xlab("Bacterial Abundance (cells/mL)")+
  ylab("Viral Abundance (cells/mL)") + 
  labs(color="Year")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.88,hjust=0.9))+ylim(NA,1e+08)+xlim(NA,7.5e+06) +
  theme(axis.text.x=element_text(angle=30, vjust=0.88,hjust=0.9),
        plot.margin=margin(1,1,1,1,"cm"))

pdf("vvsbyear.pdf", width=8,height=6)
print(ggarrange(vvsb2,
                labels=c("Viral vs. Bacterial Abundance"),
                ncol=1,nrow=1))
dev.off()

#vvsb plots seperated by year 

plot2012=ggplot(DATA %>% filter(Year==2012))+geom_point(aes(x=Bacteria_cells_mL,y=Virus_cells_mL,color=X1.Month.00)) +
  labs(x="Bacterial Abundance (c/mL)", y="Viral Abundance (c/mL)",color="Month")
plot2013=ggplot(DATA %>% filter(Year==2013))+geom_point(aes(x=Bacteria_cells_mL,y=Virus_cells_mL,color=X1.Month.00)) +
  labs(x="Bacterial Abundance (c/mL)", y="Viral Abundance (c/mL)",color="Month")
plot2015=ggplot(DATA %>% filter(Year==2015))+geom_point(aes(x=Bacteria_cells_mL,y=Virus_cells_mL,color=X1.Month.00)) +
  labs(x="Bacterial Abundance (c/mL)", y="Viral Abundance (c/mL)",color="Month")
plot2016=ggplot(DATA %>% filter(Year==2016))+geom_point(aes(x=Bacteria_cells_mL,y=Virus_cells_mL,color=X1.Month.00)) +
  labs(x="Bacterial Abundance (c/mL)", y="Viral Abundance (c/mL)",color="Month")
plot2017=ggplot(DATA %>% filter(Year==2017))+geom_point(aes(x=Bacteria_cells_mL,y=Virus_cells_mL,color=X1.Month.00)) +
  labs(x="Bacterial Abundance (c/mL)", y="Viral Abundance (c/mL)",color="Month")
plot2018=ggplot(DATA %>% filter(Year==2018))+geom_point(aes(x=Bacteria_cells_mL,y=Virus_cells_mL,color=X1.Month.00)) +
  labs(x="Bacterial Abundance (c/mL)", y="Viral Abundance (c/mL)",color="Month")

#ggarrange
pdf("seasonalvvsb.pdf", width=14,height=12)
print(ggarrange(plot2012,plot2013,plot2015,plot2016,plot2017,plot2018,
                labels=c("2012","2013","2015","2016","2017","2018"),
                ncol=1,nrow=6))
dev.off()

#vvsb plot isolating outliers 
vvsb3=ggplot(DATA, aes(x=Bacteria_cells_mL,y=Virus_cells_mL,color=as.factor(Year))) + 
  geom_point()+
  scale_colour_manual(values=c("red","blue","green","yellow", "pink", "lightblue", "gold", "chartreuse")) +
  xlab("Bacterial Abundance (c/mL)")+
  ylab("Viral Abundance (c/mL)") + 
  labs(color="Year")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.88,hjust=0.9))+ylim(NA,2.5e+07)+xlim(NA,2e+6) +
  theme(axis.text.x=element_text(angle=30, vjust=0.88,hjust=0.9),
        plot.margin=margin(1,1,1,1,"cm"))+geom_smooth(method=lm,col='blue', size=1) +geom_abline(
          mapping = NULL,
          data = NULL,
          slope=10,
          intercept=0,
          na.rm = FALSE,
          show.legend = NA
        )

#R value= 0.09265205 , p-value = 0.1046
newDATA=filter(DATA,Virus_cells_mL < 2.5e+07 & Bacteria_cells_mL<2e+6)

nonansnewDATA = is.finite(DATA$Virus_cells_mL)
cnonannewDATA = DATA$Bacteria_cells_mL[nonansnewDATA]
vnonannewDATA = DATA$Virus_cells_mL[nonansnewDATA]
corValuenewDATA = cor.test(cnonannewDATA, vnonannewDATA,method="spearman")
corValuenewDATA

#equation 
model= lm(Virus_cells_mL ~ Bacteria_cells_mL, data=newDATA)
paste('y =', coef(model)[[2]], '* x', '+', coef(model)[[1]])

#new plot 
newDATA <- newDATA%>% mutate(translation = format(sci_note, scientific = FALSE, big.mark = ","))

happy=ggplot(newDATA, aes(x=Bacteria_cells_mL,y=Virus_cells_mL,color=as.factor(Year))) + 
  geom_point()+
  scale_colour_manual(values=c("red","blue","green","yellow", "pink", "lightblue", "gold", "chartreuse")) +
  xlab("Bacterial Abundance (cells/mL)")+
  ylab("Viral Abundance (cells/mL)") + 
  labs(color="Year")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.88,hjust=0.9))+ylim(NA,2.5e+07)+xlim(NA,2e+6) +
  theme(axis.text.x=element_text(angle=30, vjust=0.88,hjust=0.9),
        plot.margin=margin(1,1,1,1,"cm"))+stat_smooth(method=lm,col='blue', size=1) +geom_abline(
          mapping = NULL,
          data = NULL,
          slope=10,
          intercept=0,
          na.rm = FALSE,
          show.legend = NA
        )+annotate("text",x=500000,y= 2.5e+07,label="y = 3.92x + 4296025.89")+annotate("text",x=500000,y= 2.35e+07,label="R=0.09")+annotate("text",x=500000,y= 1.0e+05,label="10v:1b ratio",size=3)



pdf("vvsbcluster.pdf", width=8,height=6)
print(ggarrange(happy,
                labels=c("Viral vs. Bacterial Abundance"),
                ncol=1,nrow=1))
dev.off()


