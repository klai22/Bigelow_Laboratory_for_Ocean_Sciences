#importing packages
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggpubr)

#importing file, giving it a name "pmoa_c"
pmoa_c=read.csv("pmoa_c.csv", header=TRUE)

#seeing how data was structured when read, changing data from int.-->Date
str(pmoa_c)

pmoa_c$Date=ymd(pmoa_c$Date)

#count (include 3 factors) 
n=pmoa_c%>%
  count(Gene_Name,Site,Genus)

#categorizing top hits for pmoa:isolated rows for only pmoa + group genus types 
pmoa=filter(pmoa_c,Gene_Name=="pmoA_amoA") %>% 
  group_by(Genus,Site) %>%
  summarize(Genus_count=n())

#plotting: plotting the top hits from what we filtered 
pmoaplot=ggplot(n%>%filter(Gene_Name=="pmoA_amoA"),aes(x=Genus, y=n,fill=Site)) + 
  geom_col() + 
  xlab(NULL)+
  ylab("pmoA Gene Count") +
  ylim(0,50) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, vjust=0.88,hjust=0.9),
        plot.margin=margin(0,0,0,0.8,"cm"))

#categorizing top hits for pmob:isolated rows for only pmob + group genus types 
pmob=filter(pmoa_c,Gene_Name=="pmoB_amoB") %>% 
  group_by(Genus) %>%
  summarize(Genus_count=n(),Site=Site)

#plotting: plotting the top hits from what we filtered 
pmobplot=ggplot(n%>%filter(Gene_Name=="pmoB_amoB"),aes(x=Genus, y=n,fill=Site)) + 
  geom_col() + 
  xlab(NULL)+
  ylab("pmoB Gene Count")+
  ylim(0,50) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, vjust=0.88,hjust=0.9),
        plot.margin=margin(0,0,0,0.8,"cm"))

#categorizing top hits for pmoc:isolated rows for only pmoc + group genus types 

pmoc=filter(pmoa_c,Gene_Name=="pmoC_amoC") %>% 
  group_by(Genus) %>%
  summarize(Genus_count=n(),Site=Site)


#plotting: plotting the top hits from what we filtered 
pmocplot=ggplot(n%>%filter(Gene_Name=="pmoC_amoC"),aes(x=Genus, y=n,fill=Site)) + 
  geom_col() + 
  xlab("Genera")+
  ylab("pmoC Gene Count")+ 
  ylim(0,50) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, vjust=0.88,hjust=0.9),
        plot.margin=margin(0,0,0,0.8,"cm"))

?theme()


# showing pmoA-C plots together in one figure 


pdf("pmoa_cplot.pdf", width=6,height=12)
print(ggarrange(pmoaplot,pmobplot,pmocplot,
                labels=c("A","B","C"),
                ncol=1,nrow=3,legend = NULL,
                common.legend = TRUE,
                legend.grob = NULL))
dev.off()


#categorizing top hits according to site:isolated rows for each site + their group genus types 

pond3=filter(pmoa_c,Site=="Pond3") %>% 
  group_by(Genus) %>%
  summarize(Genus_count=n())

pond6=filter(pmoa_c,Site=="Pond6") %>% 
  group_by(Genus) %>%
  summarize(Genus_count=n())

wetsedge1=filter(pmoa_c,Site=="WetSedge1") %>% 
  group_by(Genus) %>%
  summarize(Genus_count=n())

wetsedge10=filter(pmoa_c,Site=="WetSedge10") %>% 
  group_by(Genus) %>%
  summarize(Genus_count=n())

#plotting: plotting the top hits from what we filtered 
pond3plot=ggplot(pond3,aes(x=Genus, y=Genus_count)) + 
  geom_col() + 
  xlab(NULL)+
  ylab("pond3 Gene Count")+ 
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, vjust=0.88,hjust=0.9))+ylim(NA,60)

pond6plot=ggplot(pond6,aes(x=Genus, y=Genus_count)) + 
  geom_col() + 
  xlab(NULL)+
  ylab("pond6 Gene Count")+ 
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, vjust=0.88,hjust=0.9))+ylim(NA,60)

wetsedge1plot=ggplot(wetsedge1,aes(x=Genus, y=Genus_count)) + 
  geom_col() + 
  xlab(NULL)+
  ylab("wetsedge1 Gene Count")+ 
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, vjust=0.88,hjust=0.9))+ylim(NA,60)

wetsedge10plot=ggplot(wetsedge10,aes(x=Genus, y=Genus_count)) + 
  geom_col() + 
  xlab(NULL)+
  ylab("wetsedge10 Gene Count")+ 
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, vjust=0.88,hjust=0.9))+ylim(NA,60)

# showing pmoA-C plots together in one figure 
pdf("sitediversityplot.pdf", width=6,height=12)
print(ggarrange(pond3plot,pond6plot,wetsedge1plot,wetsedge10plot,
                labels=c("A","B","C","D"),
                ncol=1,nrow=4))

dev.off()

#creating genera vs. presence of pmoA-C table 

#categorizing top hits by site, and presence of pmoa-c 
pmoatable=pmoa_c%>% 
  filter(Gene_Name=="pmoA_amoA") %>%
  group_by(Site,Genus,Gene_Name) %>%
  summarize(Genus_count=n())
pmobtable=pmoa_c%>% 
  filter(Gene_Name=="pmoB_amoB") %>%
  group_by(Site,Genus,Gene_Name) %>%
  summarize(Genus_count=n())
pmoctable=pmoa_c%>% 
  filter(Gene_Name=="pmoC_amoC") %>%
  group_by(Site,Genus,Gene_Name) %>%
  summarize(Genus_count=n())


maintable=(pmoa_c) %>% 
  group_by(Site,Genus,Gene_Name) %>%
  summarize(Genus_count=n())

#MAKET TABLE + combine data where it can and leave NAs in genera that were NOT detected in the pmoa-c columns 
pmoabtable=full_join(pmoatable,pmobtable,by=c("Genus"="Genus","Site"="Site")) 
pmoabctable=full_join(pmoabtable,pmoctable,by=c("Genus"="Genus","Site"="Site"))

?full_join()

#Relabeling columns of pmoabctable (so that the genus count columns --> pmo_ gene count )
columnnames=c("Site","Genus","Gene_Name1","pmoA_Gene_Count","Gene_Name2","pmoB_Gene_Count","Gene_Name3","pmoC_Gene_Count")

names(pmoabctable)=columnnames

#Deleting Gene_Name Columns

pmoabctable=select(pmoabctable,-c(Gene_Name1,Gene_Name2,Gene_Name3))

#save cleaned up table 
write.csv(pmoabctable,"Methane_Oxidizer_Gene_Counts.csv")


#MAKE SUBTABLE isolating ONLY the genuses that have ALL 3 (omitting all rows that have any NA)

viablemotable=pmoabctable
viablemotable=na.omit(viablemotable)

#save cleaned up table 
write.csv(viablemotable,"Viable_Methane_Oxidizers.csv")

#comparison bar graph 
mv=read.csv("book2.csv", header=TRUE)

hehehe=ggplot(mv, aes(fill=Gene_Type, y=Gene.count, x=Site)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=55,size=14 ,vjust=0.88,hjust=0.9),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(size=20),
        axis.title.y = element_text(size=20),
        plot.margin=margin(0.8,0.8,0.8,1,"cm"))+
  labs(x="Site", y="Gene Count",fill="Gene Type")+scale_x_discrete(labels=c("WetSedge10"="TFSWS10",
                                                                            "WetSedge1"="OKSWS1",
                                                                            "Pond6"="OKSP6",
                                                                            "Pond3"="TFSP3"))+coord_flip()



pdf("pmoa_ccomparisonplot.pdf", width=6,height=3)
print(ggarrange(hehehe))

dev.off()