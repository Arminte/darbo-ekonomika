#Paketų instaliavimas
if(!require(eurostat)) install.packages("eurostat");require(eurostat)
if(!require(tidyverse)) install.packages("tidyverse");require(tidyverse)
library(eurostat)
#kad grafike veiktų lietuviškos raidės
Sys.setlocale("LC_ALL","Lithuanian")

#Pirmas grafikas
tax <- get_eurostat("gov_10a_taxag",stringsAsFactors = FALSE)
tax <- tax %>% filter(unit=="PC_GDP",
                      sector=="S13_S212",
                      time>="2005-01-01",
                      na_item=="D2_D5_D91_D61_M_D995")
  
tax_pt <- tax %>% filter(geo %in% c("PT","EU27_2020"))
colnames(tax_pt)[4] <- "Vieta"


ggplot(tax_pt,aes(x=time,y=values))+
  geom_line(aes(col=Vieta),size=1)+
  scale_x_date(date_labels = "%Y",
               date_breaks = "1 year")+
  labs(title = "Mokesčiai nuo BVP (%)",
       subtitle = "Šaltinis:Eurostat(gov_10a_taxag), sudaryta Armintės Globytės ir Martyno Žegunio",
       x="Laikotarpis",
       y="Mokesčių dalis (%)")+
  theme(axis.text.x=element_text(angle = 45))

tax_2019 <- tax %>% filter(time>="2019-01-01",
                           geo!="EU27_2020")
tax_2019 <- tax_2019 %>% filter(geo!="EU28")
tax_2019 <- tax_2019 %>% filter(geo!="EA19")

ggplot(tax_2019,aes(x=reorder(geo,values),y=values))+
  geom_bar(stat="identity",
           fill="steelblue")+
  geom_text(aes(label=values),vjust=-0.3,size=2.6)+
  labs(title = "Mokesčiai nuo BVP 2019 metais",
       subtitle = "Šaltinis:Eurostat(gov_10a_taxag), sudaryta Armintės Globytės ir Martyno Žegunio",
       x="Šalys",
       y="Mokesčių dalis (%)")

  