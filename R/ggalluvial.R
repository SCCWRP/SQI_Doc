library(tidyverse)
library(ggalluvial)
site <- "403BA0027"

mydf1 <- read.csv("raw/ggalluvialex.csv", stringsAsFactors = F) %>% 
  mutate(
    mysite = ifelse(MasterID %in% site, 1, 0), 
    mysite = factor(mysite)
  )

# relative severity levels
l1 <- '#008000' #'Green'
l2 <- '#90EE90' #'LightGreen'
l3 <- '#FFB6C1' #'LightPink'
l4 <- '#DC143C' #'Crimson'

ggplot(mydf1, aes(x=Year, 
                  stratum=Class, fill=Class, label=Class,
                  alluvium=MasterID))+
  scale_fill_manual(values = c(l1, l2, l3, l4))+
  geom_flow(stat="alluvium", 
            lode.guidance="rightleft",
            color="darkgrey")+
  geom_stratum()+
  scale_x_continuous(breaks=c(2014:2016))+
  ylab("# sites")+
  theme_bw(base_size = 24) + 
  theme(
    axis.title.x = element_blank(),
  )
