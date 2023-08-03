#Dot Plot from Scratch 
library("ggpubr")
library("ggplot2")
library("dplyr")
library("tidyr")
#sewd to FeGenie
setwd("/Users/kennethlai/Desktop/Sea Change Semester/INDEP. RES./DATA ANALYSIS/Codes/FeGenie")
#read csv 
FeGenie_heatmap_data_organized=read.csv("FeGenie_Percents.csv", header = TRUE)
FeGenie_heatmap_data_organized

#Organize Data 
DATA=FeGenie_heatmap_data_organized %>% 
  pivot_longer(cols=c(OKSP6,OKSWS1,TFSP3,TFSWS10),names_to="site",values_to="rel_abundance")

?pivot_longer()

#Dot Plot w/ Color
dotplot=ggplot(DATA %>% filter(rel_abundance>0))+geom_point(aes(x=site,y=Iron_Category,size=rel_abundance,color=Iron_Category)) +
  labs(x="Sample Site", y="Iron Gene Category",size="Percent of Metagenome (%)",color="Iron Gene Category") +
  scale_y_discrete(labels=c("probable_iron_reduction"="Probable iron reduction",
                            "possible_iron_oxidation_and_possible_iron_reduction"="Possible iron redox",
                            "magnetosome_formation"="Magnetosome formation",
                            "iron_aquisition-iron_transport"="Iron aquisition-iron transport",
                            "iron_aquisition-heme_oxygenase"="Iron aquisition-heme oxygenase",
                            "iron_aquisition-iron_uptake" = "Iron uptake", 
                            "iron_aquisition-heme_uptake" = "Heme uptake", 
                            "iron_aquisition-heme_lyase" = "Heme lyase", 
                            "iron_aquisition-siderophore_synthesis" = "Siderophore synthesis",
                            "iron_aquisition-siderophore_transport_potential" = "Siderophore transport potential",
                            "iron_aquisition-siderophore_transport" = "Siderophore transport",
                            "iron_gene_regulation" = "Iron gene regulation", 
                            "iron_oxidation" = "Iron oxidation", 
                            "iron_reduction" = "Iron reduction", 
                            "iron_storage" = "Iron storage"))+
  theme_linedraw() +
  theme(axis.text.x=element_text(angle=25, vjust=0.88,hjust=0.9),
        plot.margin=margin(0.8,0.8,0.8,0.8,"cm"))


#Dot Plot 
ggplot(DATA %>% filter(rel_abundance>0))+geom_point(aes(x=site,y=Iron_Category,size=rel_abundance)) +
  labs(x="Sample Site", y="Iron Gene Category",size="Percent of Metagenome (%)") +
  scale_y_discrete(labels=c("probable_iron_reduction"="Probable iron reduction",
                            "possible_iron_oxidation_and_possible_iron_reduction"="Possible iron redox",
                            "magnetosome_formation"="Magnetosome formation",
                            "iron_aquisition-iron_transport"="Iron aquisition-iron transport",
                            "iron_aquisition-heme_oxygenase"="Iron aquisition-heme oxygenase",
    "iron_aquisition-iron_uptake" = "Iron uptake", 
                            "iron_aquisition-heme_uptake" = "Heme uptake", 
                            "iron_aquisition-heme_lyase" = "Heme lyase", 
                            "iron_aquisition-siderophore_synthesis" = "Siderophore synthesis",
                            "iron_aquisition-siderophore_transport_potential" = "Siderophore transport potential",
                            "iron_aquisition-siderophore_transport" = "Siderophore transport",
                            "iron_gene_regulation" = "Iron gene regulation", 
                            "iron_oxidation" = "Iron oxidation", 
                            "iron_reduction" = "Iron reduction", 
                            "iron_storage" = "Iron storage"))+
  theme(axis.text.x=element_text(angle=25, vjust=0.88,hjust=0.9),
        plot.margin=margin(0.8,0.8,0.8,0.8,"cm"))

#Grouped Bar Graph 
ggplot(DATA, aes(fill=site, y=rel_abundance, x=Iron_Category)) + 
  geom_bar(position="dodge", stat="identity")+
  ylim(0,0.25)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=55, vjust=0.88,hjust=0.9),
        plot.margin=margin(0.8,0.8,0.8,1,"cm"))+
  labs(x="Iron Gene Category", y="Percent of Metagenome",fill="Site") +
  scale_x_discrete(labels=c("probable_iron_reduction"="Probable iron reduction",
                            "possible_iron_oxidation_and_possible_iron_reduction"="Possible iron redox",
                            "magnetosome_formation"="Magnetosome formation",
                            "iron_aquisition-iron_transport"="Iron aquisition-iron transport",
                            "iron_aquisition-heme_oxygenase"="Iron aquisition-heme oxygenase",
                            "iron_aquisition-iron_uptake" = "Iron uptake", 
                            "iron_aquisition-heme_uptake" = "Heme uptake", 
                            "iron_aquisition-heme_lyase" = "Heme lyase", 
                            "iron_aquisition-siderophore_synthesis" = "Siderophore synthesis",
                            "iron_aquisition-siderophore_transport_potential" = "Siderophore transport potential",
                            "iron_aquisition-siderophore_transport" = "Siderophore transport",
                            "iron_gene_regulation" = "Iron gene regulation", 
                            "iron_oxidation" = "Iron oxidation", 
                            "iron_reduction" = "Iron reduction", 
                            "iron_storage" = "Iron storage"))+coord_flip()
#stacked bar plot 
ggplot(DATA, aes(fill=site, y=rel_abundance, x=Iron_Category)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=55, vjust=0.88,hjust=0.9),
        plot.margin=margin(0.8,0.8,0.8,1,"cm"))+
  labs(x="Iron Gene Category", y="Percent of Metagenome",fill="Site") +
  scale_x_discrete(labels=c("probable_iron_reduction"="Probable iron reduction",
                            "possible_iron_oxidation_and_possible_iron_reduction"="Possible iron redox",
                            "magnetosome_formation"="Magnetosome formation",
                            "iron_aquisition-iron_transport"="Iron aquisition-iron transport",
                            "iron_aquisition-heme_oxygenase"="Iron aquisition-heme oxygenase",
                            "iron_aquisition-iron_uptake" = "Iron uptake", 
                            "iron_aquisition-heme_uptake" = "Heme uptake", 
                            "iron_aquisition-heme_lyase" = "Heme lyase", 
                            "iron_aquisition-siderophore_synthesis" = "Siderophore synthesis",
                            "iron_aquisition-siderophore_transport_potential" = "Siderophore transport potential",
                            "iron_aquisition-siderophore_transport" = "Siderophore transport",
                            "iron_gene_regulation" = "Iron gene regulation", 
                            "iron_oxidation" = "Iron oxidation", 
                            "iron_reduction" = "Iron reduction", 
                            "iron_storage" = "Iron storage"))
   
#Dot Plot w/ Color 2# 
ggplot(DATA %>% filter(rel_abundance>0))+geom_point(aes(x=site,y=Iron_Category,size=rel_abundance,color=rel_abundance)) +
  labs(x="Sample Site", y="Iron Gene Category",color="Percent of Metagenome (%)",size="Percent of Metagenome (%)") +
  scale_y_discrete(labels=c("probable_iron_reduction"="Probable iron reduction",
                            "possible_iron_oxidation_and_possible_iron_reduction"="Possible iron redox",
                            "magnetosome_formation"="Magnetosome formation",
                            "iron_aquisition-iron_transport"="Iron aquisition-iron transport",
                            "iron_aquisition-heme_oxygenase"="Iron aquisition-heme oxygenase",
                            "iron_aquisition-iron_uptake" = "Iron uptake", 
                            "iron_aquisition-heme_uptake" = "Heme uptake", 
                            "iron_aquisition-heme_lyase" = "Heme lyase", 
                            "iron_aquisition-siderophore_synthesis" = "Siderophore synthesis",
                            "iron_aquisition-siderophore_transport_potential" = "Siderophore transport potential",
                            "iron_aquisition-siderophore_transport" = "Siderophore transport",
                            "iron_gene_regulation" = "Iron gene regulation", 
                            "iron_oxidation" = "Iron oxidation", 
                            "iron_reduction" = "Iron reduction", 
                            "iron_storage" = "Iron storage"))+
  theme_linedraw() +
  theme(axis.text.x=element_text(angle=25, vjust=0.88,hjust=0.9),
        plot.margin=margin(0.8,0.8,0.8,0.8,"cm"))


#heat map 
oldheatmap=ggplot(DATA)+ geom_tile(aes(x=site,y=Iron_Category,fill=rel_abundance))+  scale_colour_gradient2(
  low ="pink",
  mid = "black",
  high = "lightblue",
  midpoint = .08,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill")+
  labs(x="Sample Site", y="Iron Gene Category",fill="Percent of Metagenome (%)") +
  scale_y_discrete(labels=c("probable_iron_reduction"="Probable iron reduction",
                            "possible_iron_oxidation_and_possible_iron_reduction"="Possible iron redox",
                            "magnetosome_formation"="Magnetosome formation",
                            "iron_aquisition-iron_transport"="Iron aquisition-iron transport",
                            "iron_aquisition-heme_oxygenase"="Iron aquisition-heme oxygenase",
                            "iron_aquisition-iron_uptake" = "Iron uptake", 
                            "iron_aquisition-heme_uptake" = "Heme uptake", 
                            "iron_aquisition-heme_lyase" = "Heme lyase", 
                            "iron_aquisition-siderophore_synthesis" = "Siderophore synthesis",
                            "iron_aquisition-siderophore_transport_potential" = "Siderophore transport potential",
                            "iron_aquisition-siderophore_transport" = "Siderophore transport",
                            "iron_gene_regulation" = "Iron gene regulation", 
                            "iron_oxidation" = "Iron oxidation", 
                            "iron_reduction" = "Iron reduction", 
                            "iron_storage" = "Iron storage"))+
  theme(axis.text.x=element_text(angle=0, vjust=0.88,hjust=0.9),
        plot.margin=margin(0.8,0.8,0.8,0.8,"cm"))

#trying reorder y 
categories=unique(DATA[c("Iron_Category")])
str(categories)
categories %>% mutate(across(where(is.factor), as.character))

x=ggplot(DATA)+ geom_tile(aes(x=site,y=factor(Iron_Category,levels=categories),fill=rel_abundance))+  scale_colour_gradient2(
  low ="pink",
  mid = "black",
  high = "lightblue",
  midpoint = .08,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill")+
  labs(x="Sample Site", y="Iron Gene Category",fill="Percent of Metagenome (%)") +
  scale_y_discrete(labels=c("probable_iron_reduction"="Probable iron reduction",
                            "possible_iron_oxidation_and_possible_iron_reduction"="Possible iron redox",
                            "magnetosome_formation"="Magnetosome formation",
                            "iron_aquisition-iron_transport"="Iron aquisition-iron transport",
                            "iron_aquisition-heme_oxygenase"="Iron aquisition-heme oxygenase",
                            "iron_aquisition-iron_uptake" = "Iron uptake", 
                            "iron_aquisition-heme_uptake" = "Heme uptake", 
                            "iron_aquisition-heme_lyase" = "Heme lyase", 
                            "iron_aquisition-siderophore_synthesis" = "Siderophore synthesis",
                            "iron_aquisition-siderophore_transport_potential" = "Siderophore transport potential",
                            "iron_aquisition-siderophore_transport" = "Siderophore transport",
                            "iron_gene_regulation" = "Iron gene regulation", 
                            "iron_oxidation" = "Iron oxidation", 
                            "iron_reduction" = "Iron reduction", 
                            "iron_storage" = "Iron storage"))+
  theme(axis.text.x=element_text(angle=0, vjust=0.88,hjust=0.9),
        plot.margin=margin(0.8,0.8,0.8,0.8,"cm"))

#comparing graphs 
ggarrange(heatmap, dotplot,
          labels=c("A","B"),
          ncol=2,nrow=1,legend = NULL,
          common.legend = TRUE,
          legend.grob = NULL)




#NEW OFFICIAL HEATMAP
newDATA=subset(DATA, DATA$Iron_Category !="iron_gene_regulation" & DATA$Iron_Category !="iron_aquisition-siderophore_transport_potential"& DATA$Iron_Category !="iron_aquisition-siderophore_transport") 

heatmap=ggplot(newDATA)+ geom_tile(aes(x=site,y=factor(Iron_Category,level=c("iron_storage",
                            "magnetosome_formation",
                            "iron_aquisition-iron_transport",
                            "iron_aquisition-heme_oxygenase",
                            "iron_aquisition-iron_uptake", 
                            "iron_aquisition-heme_uptake", 
                            "iron_aquisition-heme_lyase", 
                            "iron_aquisition-heme_transport",
                            "iron_aquisition-siderophore_synthesis",
                            "iron_aquisition-siderophore_transport_potential",
                            "iron_aquisition-siderophore_transport",
                            "iron_gene_regulation", 
                            "iron_oxidation", 
                            "iron_reduction",
                            "probable_iron_reduction",
                            "possible_iron_oxidation_and_possible_iron_reduction")),fill=rel_abundance))+  scale_colour_gradient2(
  low ="pink",
  mid = "black",
  high = "lightblue",
  midpoint = .02,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill")+
  labs(x="Sample Site", y="Iron Gene Category",fill="Percent of Metagenome (%)") +
  scale_y_discrete(labels=c("probable_iron_reduction"="Probable iron reduction",
                            "possible_iron_oxidation_and_possible_iron_reduction"="Possible iron redox",
                            "magnetosome_formation"="Magnetosome formation",
                            "iron_aquisition-iron_transport"="Iron aquisition-iron transport",
                            "iron_aquisition-heme_oxygenase"="Iron aquisition-heme oxygenase",
                            "iron_aquisition-iron_uptake" = "Iron uptake", 
                            "iron_aquisition-heme_uptake" = "Heme uptake", 
                            "iron_aquisition-heme_lyase" = "Heme lyase", 
                            "iron_aquisition-siderophore_synthesis" = "Siderophore synthesis",
                            "iron_aquisition-siderophore_transport_potential" = "Siderophore transport potential",
                            "iron_aquisition-siderophore_transport" = "Siderophore transport",
                            "iron_gene_regulation" = "Iron gene regulation", 
                            "iron_oxidation" = "Iron oxidation", 
                            "iron_reduction" = "Iron reduction", 
                            "iron_storage" = "Iron storage",
                            "iron_aquisition-heme_transport"="Iron acquisition-iron transport"))+scale_x_discrete(labels=c("OKSP6"="Pond6","OKSWS1"="WetSedge1","TFSP3"="Pond3","TFSWS10"="WetSedge10"))+
  theme(axis.text.x=element_text(angle=30,size=14, vjust=0.88,hjust=0.9),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(size=18),
        axis.title.y = element_text(size=18),
        plot.margin=margin(0.8,0.8,0.8,0.8,"cm"),legend.position="right")
#printing the graph 
pdf("FeGenie Heat Map.pdf", width=10,height=8)
print(heatmap,
                labels=c("Iron Gene Percents of Arctic Tundra Metagenomes"),
                ncol=1,nrow=1,legend = NULL,
                common.legend = TRUE,
                legend.grob = NULL)
dev.off()

#comparison bar graph 
redoxdata=subset(DATA, DATA$Iron_Category !="iron_gene_regulation" & DATA$Iron_Category !="iron_aquisition-siderophore_transport_potential"& DATA$Iron_Category !="iron_aquisition-siderophore_transport" 
                    & DATA$Iron_Category !="iron_aquisition-iron_transport" 
                    & DATA$Iron_Category !="iron_aquisition-heme_transport" 
                    & DATA$Iron_Category !="iron_aquisition-heme_oxygenase" 
                    & DATA$Iron_Category !="iron_aquisition-siderophore_synthesis" 
                    & DATA$Iron_Category !="possible_iron_oxidation_and_possible_iron_reduction" 
                    & DATA$Iron_Category !="probable_iron_reduction" 
                    & DATA$Iron_Category !="iron_storage" 
                    & DATA$Iron_Category !="magnetosome_formation") 

redox=ggplot(redoxdata, aes(fill=site, y=rel_abundance, x=Iron_Category)) + 
  geom_bar(position="dodge", stat="identity")+
  ylim(0,0.03)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=55,size=30 ,vjust=0.88,hjust=0.9),
        axis.text.y=element_text(size=30),
        axis.title.x=element_text(size=40),
        axis.title.y = element_text(size=40),
        plot.margin=margin(0.8,0.8,0.8,1,"cm"))+
  labs(x="Iron Gene Category", y="Percent of Metagenome",fill="Site") +
  scale_x_discrete(labels=c("probable_iron_reduction"="Probable iron reduction",
                            "possible_iron_oxidation_and_possible_iron_reduction"="Possible iron redox",
                            "magnetosome_formation"="Magnetosome formation",
                            "iron_aquisition-iron_transport"="Iron aquisition-iron transport",
                            "iron_aquisition-heme_oxygenase"="Iron aquisition-heme oxygenase",
                            "iron_aquisition-iron_uptake" = "Iron uptake", 
                            "iron_aquisition-heme_uptake" = "Heme uptake", 
                            "iron_aquisition-heme_lyase" = "Heme lyase", 
                            "iron_aquisition-siderophore_synthesis" = "Siderophore synthesis",
                            "iron_aquisition-siderophore_transport_potential" = "Siderophore transport potential",
                            "iron_aquisition-siderophore_transport" = "Siderophore transport",
                            "iron_gene_regulation" = "Iron gene regulation", 
                            "iron_oxidation" = "Iron oxidation", 
                            "iron_reduction" = "Iron reduction", 
                            "iron_storage" = "Iron storage"))+coord_flip()

pdf("Fe Redox Comparison Bar Graphs.pdf", width=10,height=8)
print(redox,
      labels=c("Iron Gene Percents for Fe Redox"),
      ncol=1,nrow=1,legend = NULL,
      common.legend = TRUE,
      legend.grob = NULL)
dev.off()
