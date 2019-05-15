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

library(tidyverse)
library(lubridate)
library(readxl)
library(ASCI)
library(PHAB)
library(PHABMetrics)

# data compile for SGR case study SGUR00428, SMC00428

# data from search, not from JW -------------------------------------------

# fls <- list.files('raw/rawdig/',
#   recursive = T, full.names = T)
# 
# dat <- fls %>%
#   enframe %>%
#   group_by(value) %>%
#   mutate(
#     rawdat = purrr::map(value, function(x){
# 
#       # import, wrangle data
#       raw <- read_excel(x)
#     
#       return(raw)
#       
#     })
#   ) %>%
#   select(-name) %>%
#   ungroup %>%
#   mutate(value = basename(value))

# GIS data ----------------------------------------------------------------

gis <- read_excel('../Data/RawData/fire case study/dbo_tblGISMetrics_030519.xlsx') %>%
  mutate(
    LogWSA = log10(AREA_SQKM)
  )

# CSCI --------------------------------------------------------------------

csci <- read.csv('../Data/RawData/fire case study/Post Fire Site - CSCI.csv', stringsAsFactors = F) %>%
  dplyr::select(Station.Code, Year, Parameter, Result) %>%
  filter(Parameter %in% 'CSCI') %>%
  rename(
    StationCode = Station.Code,
    yr = Year,
    var = Parameter,
    val = Result
  ) %>%
  group_by(StationCode, yr, var) %>%
  summarise(val = mean(val, na.rm = T))

# ASCI --------------------------------------------------------------------

rawtax <- read.csv('../Data/RawData/fire case study/SGRRMP Algal Taxa List.csv', stringsAsFactors = F) %>%
  rename(
    StationCode = Station.Code,
    Result = Results,
    SampleTypeCode = SampleType,
    SampleDate = Column1
  ) %>%
  mutate(Replicate = 1) %>%
  dplyr::select(StationCode, SampleDate, Replicate, SampleTypeCode, BAResult, Result, FinalID)

ascigis <- rawtax %>%
  dplyr::select(StationCode, Replicate, SampleDate) %>%
  unique %>%
  left_join(gis, by = 'StationCode') %>%
  mutate(
    DayOfYear = mdy(SampleDate) %>% yday(),
    LogWSA = log(AREA_SQKM)
  )

# # check inputs
# ASCI::chkinp(rawtax, ascigis)

# clean up raw tax based on checks
rawtax <- rawtax %>%
  filter(!FinalID %in% c("Navicula aitchelbee", "Sellaphora atomoides", "Sellaphora nigri",
                         "No_Orgs_Pres"))

# asci scores
asci <- ASCI(rawtax, ascigis) %>%
  scores %>%
  filter(taxa %in% 'hybrid') %>%
  separate(SampleID, c('StationCode', 'SampleDate', 'Replicate'), sep = '_') %>%
  mutate(
    yr = mdy(SampleDate) %>% year()
  ) %>%
  group_by(StationCode, yr) %>%
  summarise(val = mean(MMI)) %>%
  mutate(var = 'ASCI') %>%
  dplyr::select(StationCode, yr, var, val)

# water chem --------------------------------------------------------------

# cond <- dat %>%
#   filter(grepl('^SpecCond', value)) %>%
#   pull(rawdat) %>%
#   .[[1]] %>%
#   select(StationCode, SampleDate, Result) %>%
#   mutate(
#     yr = year(SampleDate),
#     Result = as.numeric(Result)
#   ) %>%
#   group_by(StationCode, yr) %>%
#   summarise(Cond = mean(Result, na.rm = T)) %>%
#   gather('var', 'val', -StationCode, -yr) %>%
#   na.omit
#
# tntpsmc <- dat %>%
#   filter(grepl('^nutrient', value)) %>%
#   pull(rawdat) %>%
#   .[[1]] %>%
#   select(stationcode, sampledate, total_n_mgl, total_p_mgl) %>%
#   mutate(
#     yr = year(sampledate)
#   ) %>%
#   rename(
#     StationCode = stationcode
#   ) %>%
#   group_by(StationCode, yr) %>%
#   summarize(
#     TN = mean(total_n_mgl, na.rm = T),
#     TP = mean(total_p_mgl, na.rm = T)
#   ) %>%
#   gather('var', 'val', -StationCode, -yr) %>%
#   na.omit
#
# tntpabc <- dat %>%
#   filter(grepl('^Compiled', value)) %>%
#   pull(rawdat) %>%
#   .[[1]] %>%
#   select(StationCode, SampleDate, AnalyteName, Result) %>%
#   filter(AnalyteName %in% c('SpecificConductivity', 'Phosphorus as P', 'Total_N_calculated', 'Total_n_calculated', 'Total_P_reported', 'Total_N_partial', 'Total_N_Partial', 'Total_P_Partial')) %>%
#   mutate(
#     AnalyteName = case_when(
#       AnalyteName %in% 'SpecificConductivity' ~ 'Cond',
#       AnalyteName %in% c('Phosphorus as P', 'Total_P_reported', 'Total_P_partial') ~ 'TP',
#       AnalyteName %in% c('Total_N_calculated', 'Total_n_calculated', 'Total_N_partial', 'Total_N_Partial') ~ 'TN'
#     ),
#     SampleDate = case_when( # ignore warnings here
#       is.na(as.numeric(SampleDate)) ~ dmy(SampleDate),
#       !is.na(as.numeric(SampleDate)) ~ as.Date(as.numeric(SampleDate), origin = '1899-12-30')
#     ),
#     yr = year(SampleDate)
#   ) %>%
#   group_by(StationCode, yr, AnalyteName) %>%
#   summarise(Result = mean(Result, na.rm = T)) %>%
#   spread(AnalyteName, Result) %>%
#   gather('var', 'val', -StationCode, -yr) %>%
#   na.omit
#
# # all nutrients
# nutdat <- rbind(cond, tntpsmc, tntpabc) %>%
#   group_by(StationCode, yr, var) %>%
#   summarise(val = mean(val, na.rm = T))
#
# chemours <- nutdat %>%
#   filter(StationCode %in% c('SMC00428', 'SGUR00428'))

# from Josh
nuts <- read_excel('../Data/RawData/fire case study/SGRRMP Chemistry.xlsx') %>%
  rename(
    StationCode = `Station Id`,
    yr = Year,
    var = AnalyteName,
    val = Result
  ) %>%
  mutate(val = ifelse(val == -88, MDL, val)) %>%
  group_by(StationCode, yr, var) %>%
  summarise(val = mean(val, na.rm = T)) %>%
  ungroup %>%
  mutate(
    var = case_when(
      var %in% c('Nitrate as N', 'Nitrite as N', 'Total Kjeldahl Nitrogen') ~ 'TN',
      var %in% c('OrthoPhosphate as P', 'Phosphorus as P') ~ 'TP'
    )
  ) %>%
  group_by(StationCode, yr, var) %>%
  summarise(val = sum(val))

cond <- read.csv('../Data/RawData/fire case study/conductivity.csv', stringsAsFactors = F) %>%
  mutate(var = 'Cond') %>%
  rename(val = Result) %>%
  group_by(StationCode, yr, var) %>%
  summarise(val = mean(val)) %>%
  dplyr::select(StationCode, yr, var, val)

chem <- rbind(nuts, cond)

# habitat data ------------------------------------------------------------

# cram
cram <- read.csv('../Data/RawData/fire case study/CRAMData201941814222.csv', stringsAsFactors = F) %>%
  rename(
    StationCode = Station.Code,
    var = Attribute,
    val = Score
  ) %>%
  mutate(
    yr = mdy(Sample.Date) %>% year(),
    var = case_when(
      var %in% 'Overall Score' ~ 'indexscore_cram',
      var %in% 'Biotic Structure' ~ 'bs',
      var %in% 'Buffer and Landscape Context' ~ 'blc',
      var %in% 'Hydrology' ~ 'hy',
      var %in% 'Physical Structure' ~ 'ps'
    )
  ) %>%
  group_by(StationCode, yr, var) %>%
  summarise(val = mean(val, na.rm = T))

# ipi
metrics <- read.csv('../Data/RawData/fire case study/SMC00428_SGRU00428_Phab_metrics.csv', stringsAsFactors = F) %>%
  rename(
    StationCode = stationname,
    SampleDate = sampledate,
    Variable = variable,
    Result = result,
    Count_Calc = count_calc
  ) %>%
  dplyr::select(StationCode, SampleDate, Variable, Result, Count_Calc) %>%
  mutate(
    SampleDate = gsub('(^.*)\\s.*$', '\\1', SampleDate),
    StationCode = 'SGUR00428'
  ) %>%
  filter(!SampleDate %in% '19/7/2017')

ipidat <- IPI(gis, metrics) %>%
  mutate(
    yr = dmy(SampleDate) %>% year()
  ) %>%
  gather(var, val, -StationCode, -yr) %>%
  filter(grepl('^IPI$|\\_score$', var)) %>%
  mutate(
    val = as.numeric(val),
    var = gsub('\\_score$', '', var)
  )

# join all data -----------------------------------------------------------

alldat <- bind_rows(csci, asci, chem, cram, ipidat)

# plot --------------------------------------------------------------------

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

plodat <- alldat %>% 
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


toplo1 <- plodat %>% 
  filter(grp %in% 'Biology')
toplo1lb <- toplo1 %>% 
  arrange(var, yr) %>% 
  filter(!duplicated(var))
toplo2 <- plodat %>% 
  filter(grp %in% 'Chemistry')
toplo3 <- plodat %>% 
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