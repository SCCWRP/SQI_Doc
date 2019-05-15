library(tidyverse)
library(readxl)
library(lubridate)
library(randomForest)
library(SQI)
# devtools::load_all('../SQI/.')
library(sf)
library(ASCI)
library(PHAB)
library(PHABMetrics)

# numeric variable names
numnms <- c('CSCI', 'ASCI', 'TN', 'TP', 'Cond', 'indexscore_cram', 'IPI', 'blc', 'bs', 'ps', 'hy', 'PCT_SAFN', 'H_AqHab', 'H_SubNat',  'Ev_FlowHab', 'XCMG')
numlab <- c('CSCI', 'ASCI', 'Total nitrogen', 'Total phosphorus', 'Conductivity', 'CRAM', 'IPI', 'Buffer and\nlandscape', 'Biotic\nstructure', 'Physical\nstructure', 'Hydrologic\ncondition', '% sands\nand fines', 'Diversity of\nhabitat', 'Diversity of\nsubstrate', 'Evenness of\nflow habitat', 'Riparian\nveg. cover')
names(numnms) <- numlab

# color lims
cscilim <- c(0, 0.63, 0.79, 1)
ascilim <- c(0, 0.70, 0.83, 1)
tnlim <- c(2, 1, 0.5, 0)
tplim <- c(0.2, 0.1, 0.05, 0)
condlim <- c(2000, 1200, 600, 0)
cramlim <- c(0, 63, 72, 100)
ipilim <- c(0, 0.71, 0.84, 1)
blclim <- c(0, 72, 82, 100)
bslim <- c(0, 38, 54, 100)
pslim <- c(0, 44, 60, 100)
hylim <- c(0, 51, 64, 100)
safnlim <- c(0, 0.16, 0.32, 1)
aqhablim <- c(0, 0.16, 0.32, 1)
subnatlim <- c(0, 0.16, 0.32, 1)
flowhablim <- c(0, 0.16, 0.32, 1)
veglim <- c(0, 0.16, 0.32, 1)

# for boxplot color limits
collims <- list(cscilim, ascilim, tnlim, tplim, condlim, cramlim, ipilim, blclim, bslim, pslim, hylim, safnlim, aqhablim, subnatlim, flowhablim, veglim)
names(collims) <- numlab
collims <- enframe(collims, 'var', 'lims') %>% 
  mutate(
    varlab = var, 
    var = factor(var, levels = numlab, labels = numnms) 
  )

# gauge cols, red, orange, green
gauge_col <- c('#a9d70b', '#f9c802', '#ff0000') %>% rev

alldat <- bind_rows(csci, asci, chem, cram, ipidat) %>% 
  mutate(
    grp = case_when(
      var %in% c('Cond', 'TN', 'TP') ~ 'Chemistry', 
      var %in% c('CSCI', 'ASCI') ~ 'Biology',
      T ~ 'Physical habitat'
    )
  ) %>% 
  group_by(var) %>% 
  nest %>% 
  full_join(collims, by = 'var') %>% 
  mutate(
    col = purrr::pmap(list(data, lims), function(data, lims){
      
      lbs <- gauge_col
      brks <- c(-Inf, lims[2], lims[3], Inf)
      if(lims[1] > lims[2]){
        lbs <- rev(lbs)
        brks <- c(Inf, lims[2], lims[3], -Inf) 
      }
      
      out <- cut(data$val, breaks = brks, labels = lbs, right = F)
      out <- as.character(out)
      
      return(out)
      
    })
  ) %>% 
  select(-lims) %>% 
  unnest


toplo1 <- alldat %>% 
  filter(grp %in% 'Biology')
toplo1lb <- toplo1 %>% 
  arrange(var, yr) %>% 
  filter(!duplicated(var))
toplo2 <- alldat %>% 
  filter(grp %in% 'Chemistry')
toplo3 <- alldat %>% 
  filter(grp %in% 'Physical habitat') %>% 
  mutate(habtyp = case_when(
    var %in% c('indexscore_cram', 'blc', 'bs', 'hy', 'ps') ~ 'CRAM', 
    T ~ 'IPI'
  ))
toplo3a <- toplo3 %>% 
  filter(habtyp %in% 'CRAM') %>% 
  mutate(varlab = factor(varlab, levels = c('CRAM', 'Buffer and\nlandscape', 'Biotic\nstructure', 'Physical\nstructure', 'Hydrologic\ncondition')))
toplo3b <-  toplo3 %>% 
  filter(habtyp %in% 'IPI') %>% 
  mutate(varlab = factor(varlab, 
                         levels = c('IPI', '% sands\nand fines', 'Diversity of\nhabitat', 'Diversity of\nsubstrate', 'Evenness of\nflow habitat', 'Riparian\nveg. cover'),
                         labels = c('IPI', '% sands\n& fines', 'Div. of\nhabitat', 'Div. of\nsubstrate', 'Ev. of\nflow habitat', 'Rip. veg.\ncover')))

legdat <- data.frame(
  x = c(1:3), 
  y = c(1:3), 
  fac = c('lo', 'md', 'hi')
)
legdat$fac <- factor(legdat$fac, levels = c('hi', 'md', 'lo'))
pleg <- ggplot(legdat, aes(x, y, fill = fac)) + 
  geom_point(pch = 21, colour = 'black', size = 4) +
  scale_fill_manual(values = gauge_col) + 
  pthm + 
  theme(legend.position = 'right') +
  facet_wrap(~fac, strip.position = 'left', scales = 'free_y', ncol = 1) 
pleg <- g_legend(pleg)



pthm <- theme_bw(base_family = 'serif') +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1), 
    panel.grid.minor.x = element_blank(), 
    panel.grid = element_blank(),
    legend.position = 'top', 
    legend.title = element_blank(), 
    strip.background = element_blank(), 
    strip.placement = 'outside', 
    axis.title.y = element_blank()
  ) 

p1 <- ggplot(toplo1, aes(x = yr, y = val, group = var)) + 
  geom_line() +
  # geom_text_repel(data = toplo1lb, aes(label = varlab), 
  #                 family = 'serif', fontface = 'italic', point.padding = 0.5, min.segment.length = 0, box.padding = 2, segment.colour = 'darkgrey') +
  geom_point(pch = 21, fill = toplo1$col, colour = 'black', size = 4, alpha = 0.8) +
  scale_y_continuous('Index score', limits = c(0, 1.1)) + 
  scale_x_continuous(breaks = seq(2008, 2017)) +
  facet_wrap(~varlab, strip.position = 'left', scales = 'free_y', ncol = 1) + 
  geom_vline(xintercept = 2009.7, linetype = 'dotted') +
  ggtitle('(a) Biological response') +
  pthm   

p2 <- ggplot(toplo2, aes(x = yr, y = val, group = var)) + 
  geom_line() +
  # geom_text_repel(data = toplo2lb, aes(label = varlab),
  #                 family = 'serif', fontface = 'italic', point.padding = 0.5, min.segment.length = 0, box.padding = 2, segment.colour = 'darkgrey') +
  geom_point(pch = 21, fill = toplo2$col, colour = 'black', size = 3, alpha = 0.8) +
  # scale_y_continuous('Index score') + 
  scale_x_continuous(breaks = seq(2008, 2017), limits = c(2008, 2017)) +
  geom_vline(xintercept = 2009.7, linetype = 'dotted') + 
  facet_wrap(~varlab, strip.position = 'left', scales = 'free_y', ncol = 1) + 
  ggtitle('(b) Water chemistry') +
  pthm +
  theme(axis.title.y = element_blank())

p3a <- ggplot(toplo3a, aes(x = yr, y = val, group = var)) + 
  geom_line() +
  geom_point(pch = 21, fill = toplo3a$col, colour = 'black', size = 3, alpha = 0.8) +
  # scale_y_continuous('Index score') + 
  scale_x_continuous(breaks = seq(2008, 2017), limits = c(2008, 2017)) +
  geom_vline(xintercept = 2009.7, linetype = 'dotted') + 
  facet_wrap(~varlab, strip.position = 'left', ncol = 1) + 
  ggtitle('(c) CRAM scores') +
  pthm +
  theme(axis.title.y = element_blank())

p3b <- ggplot(toplo3b, aes(x = yr, y = val, group = var)) + 
  geom_line() +
  geom_point(pch = 21, fill = toplo3b$col, colour = 'black', size = 3, alpha = 0.8) +
  # scale_y_continuous('Index score') + 
  scale_x_continuous(breaks = seq(2008, 2017), limits = c(2008, 2017)) +
  geom_vline(xintercept = 2009.7, linetype = 'dotted') + 
  facet_wrap(~varlab, strip.position = 'left', ncol = 1) + 
  ggtitle('(d) IPI scores') +
  pthm +
  theme(axis.title.y = element_blank())

png('figs/fire_plo.png', height = 8, width = 7, units = 'in', res = 500)
((p1 + (p2 + p3a + p3b + plot_layout(ncol = 3)) + plot_layout(ncol = 1, heights = c(0.8, 1))) | wrap_elements(pleg)) + plot_layout(ncol = 2, widths = c(1, 0.05))
dev.off()