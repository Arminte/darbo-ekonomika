Sys.setlocale("LC_ALL","Lithuanian")
library(ggplot2)
library(tidyverse)
df <- read.csv("suicide-deaths-by-age.csv")
df <- df[,-1]
colnames(df)<-c("Code","Metai","70+","50-69","15-49","5-14")
df <- df %>% filter(Code=="USA",Metai=="2017") 

df <- df %>% gather(group,value,3:6)

ggplot(df,aes(reorder(group,value),value))+
geom_bar(stat="identity",fill="orange")+
  labs(title="Savižudybių skaičius JAV 2017 metais",
       subtitle="Šaltinis:ourworldindata.org, skaičiavimai Armintės Globytės",
       x="Amžiaus grupė",
       y="Savižudybių skaičius")

#########

df <- read.csv("suicide-deaths-by-age.csv")
df <- df[,-1]
colnames(df)<-c("Code","Metai","70+","50-69","15-49","5-14")
df <- df %>% filter(Code=="USA") 

df$suma <- df$`70+`+df$`50-69` + df$`15-49` + df$`5-14`

df <- df %>% gather(group,value,3:7)

df <- df %>% filter(group=="suma")

df2 <- read.csv("unemployment-rate.csv")
df2 <- df2[,-1]
colnames(df2)<-c("Code","Metai","NL")
df2 <- df2 %>% filter(Code=="USA")


dft <- merge(df,df2,by="Metai")
dft <- dft[-5]
dft <- dft[-3]

library(gtable)
library(grid)

sr <- read.csv("suicide_rate.csv")
sr <- sr[-(2:5)]
sr <- sr[-4]
colnames(sr)<-c("Location","Metai","SL")
sr$Location
sr <- filter(sr,sr$Location=="USA")
sr <- sr[-1]

dft <- merge(dft,sr,by="Metai")
dft <- filter(dft,Metai<="2010")


p1 <- ggplot(dft,aes(Metai,SL))+
  geom_line(stat = "identity")+
  labs(x="Metai",
       y="Savižudybių lygis")+
  scale_y_continuous(breaks = "1 year")
  

p2 <- ggplot(dft,aes(Metai,NL))+
  geom_line(stat = "identity")+
  labs(x="Metai",
       y="Nedarbo lygis")+
  scale_y_continuous(breaks = "1 year")


g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g <- rbind(g1, g2, size="first")
g$widths <- unit.pmax(g1$widths, g2$widths) 
g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
grid.newpage()
grid.draw(g)
grid.arrange(g,ncol=1, top = "Savižudybių ir nedarbo lygiai JAV",bottom = textGrob(
  "Šaltinis:data.oecd.org, skaičiavimai Armintės Globytės",
  gp = gpar(fontface = 3, fontsize = 11),
  hjust = 1,
  x = 1))

model <- lm(dft$Rate~dft$SR)
summary(model)

###########

bar <- read.csv("unemployment.csv",sep=",",header = T,row.names = NULL)
bar <- bar[-c(1,2,3,5)]
colnames(bar) <- c("Country","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
bar <- bar %>% gather(Metai,"Nedarbo.lygis",2:21)
bar <- spread(bar,Country,Nedarbo.lygis)
colnames(bar) <-c("Metai","EU","JAV","Pasaulis")

colors <- c("EU"="green","JAV"="blue","Pasaulis"="red")
  
ggplot(bar,aes(x=Metai,group = 1))+ 
  geom_line(aes(y=EU,color="EU"),size=0.8)+ 
  geom_line(aes(y=Pasaulis,color="Pasaulis"),size=0.8)+
  geom_line(aes(y=JAV,color="JAV"),size=0.8)+
  scale_color_manual(values=colors)+
  theme(legend.position="right",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Nedarbo lygis JAV, Europos Sąjungoje ir pasaulyje",
       subtitle="Šaltinis:databank.worldbank.org, skaičiavimai Armintės Globytės",
       x="Metai",
       y="Nedarbo lygis (%)",
       color=" ")
