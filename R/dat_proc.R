library(tidyverse)
library(readxl)
library(lubridate)
library(randomForest)
library(SQI)
# devtools::load_all('../SQI/.')
library(sf)

prj <- 4326 # geographic wgs84

# import all raw data for combining for sqi -------------------------------

fls <- list.files('raw/rawdig/',
                  recursive = T, full.names = T)

dat <- fls %>% 
  enframe %>% 
  group_by(value) %>% 
  mutate(
    rawdat = purrr::map(value, function(x){
      
      # import, wrangle data
      raw <- read_excel(x)
      
      return(raw)
      
    })
  ) %>% 
  select(-name) %>% 
  ungroup %>% 
  mutate(value = basename(value))

# masterid link with StationCode ------------------------------------------------------

mastid <- dat %>% 
  filter(grepl('^luStation', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(StationID, MasterID, Latitude, Longitude) %>% 
  rename(StationCode = StationID) %>% 
  group_by(StationCode, MasterID) %>% 
  summarize(
    Latitude = mean(Latitude, na.rm = T), 
    Longitude = mean(Longitude, na.rm = T)
  )

# ASCI data -------------------------------------------------------------

# original 
ascidat <- dat %>% 
  filter(grepl('^asci\\_scrs', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  filter(ind %in% 'MMI') %>% 
  filter(tax %in% 'Hybrid') %>% 
  rename(
    asci_mean = scr
  ) %>% 
  select(sampleid, asci_mean) %>% 
  mutate(
    StationCode = gsub('^(.*)_.*_.*$', '\\1', sampleid),
    date = gsub('^.*_(.*)_.*$', '\\1', sampleid), 
    smps = gsub('^.*_.*_(.*)$', '\\1', sampleid),
    date = mdy(date),
    yr = year(date),
    asci_mean = as.numeric(asci_mean)
  ) %>% 
  group_by(StationCode, yr) %>% 
  summarize(asci_mean = mean(asci_mean, na.rm = T)) 

# smc
ascismcdat <- dat %>% 
  filter(grepl('^ASCI\\_SMC', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  dplyr::select(X__1, hybrid_MMI) %>% 
  rename(
    sampleid = X__1,
    asci_mean = hybrid_MMI
  ) %>% 
  mutate(
    StationCode = gsub('^(.*)_.*_.*$', '\\1', sampleid),
    date = gsub('^.*_(.*)_.*$', '\\1', sampleid), 
    smps = gsub('^.*_.*_(.*)$', '\\1', sampleid),
    date = mdy(date),
    yr = year(date),
    asci_mean = as.numeric(asci_mean)
  ) %>% 
  group_by(StationCode, yr) %>% 
  summarize(asci_mean = mean(asci_mean, na.rm = T)) 

# swamp
asciswampdat <- dat %>% 
  filter(grepl('^ASCI\\_SWAMP', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  dplyr::select(X__1, hybrid_MMI) %>% 
  rename(
    sampleid = X__1,
    asci_mean = hybrid_MMI
  ) %>% 
  mutate(
    StationCode = gsub('^(.*)_.*_.*$', '\\1', sampleid),
    date = gsub('^.*_(.*)_.*$', '\\1', sampleid), 
    smps = gsub('^.*_.*_(.*)$', '\\1', sampleid),
    date = mdy(date),
    yr = year(date),
    asci_mean = as.numeric(asci_mean)
  ) %>% 
  group_by(StationCode, yr) %>% 
  summarize(asci_mean = mean(asci_mean, na.rm = T)) 

# combine all asci
ascidat <- ascidat %>% 
  bind_rows(ascismcdat) %>% 
  bind_rows(asciswampdat) %>% 
  group_by(StationCode, yr) %>% 
  summarize(asci_mean = mean(asci_mean, na.rm = T)) %>% 
  ungroup %>% 
  gather('var', 'val', -StationCode, -yr)

# CSCI data ---------------------------------------------------------------

cscidat <- dat %>% 
  filter(grepl('^CSCI', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(StationCode, SampleYear, CSCI) %>% 
  rename(yr = SampleYear) %>% 
  group_by(StationCode, yr) %>% 
  summarize(csci_mean = mean(CSCI, na.rm = T)) %>% 
  gather('var', 'val', -StationCode, -yr)


# CRAM data ----------------------------------------------------------------

# orig cram data
cramdat <- dat %>% 
  filter(grepl('^CRAM', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(StationCode, visitdate, indexscore, bs, blc, hy, ps) %>% 
  mutate(yr = year(visitdate)) %>% 
  group_by(StationCode, yr) %>% 
  summarize(
    indexscore_cram = mean(indexscore, na.rm = T),
    bs = mean(bs, na.rm = T),
    blc = mean(blc, na.rm = T),
    hy = mean(hy, na.rm = T),
    ps = mean(ps, na.rm = T)
  )

# ecram
cramdate <- dat %>% 
  filter(grepl('^Missing\\sCRAM', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  dplyr::select(MasterID, Year, indexscore, buffer_landscape_cont, hydrology, physical_structure, biotic_structure) %>% 
  rename(
    yr = Year, 
    indexscore_cram = indexscore, 
    bs = biotic_structure,
    blc = buffer_landscape_cont,
    hy = hydrology, 
    ps = physical_structure
  ) %>% 
  left_join(mastid, by = 'MasterID') %>% # this is usually done when combining all, but need to get stationcode here to combine with cram above
  select(StationCode, yr, indexscore_cram, bs, blc, hy, ps) 

# combine original with ecram
cramdat <- cramdat %>% 
  bind_rows(cramdate) %>% 
  group_by(StationCode, yr) %>% 
  summarize(
    indexscore_cram = mean(indexscore_cram, na.rm = T),
    bs = mean(bs, na.rm = T),
    blc = mean(blc, na.rm = T),
    hy = mean(hy, na.rm = T),
    ps = mean(ps, na.rm = T)
  ) %>% 
  ungroup %>% 
  gather('var', 'val', -StationCode, -yr)

# IPI data -----------------------------------------------------------------

##
# new ipi data
ipifeb <- dat %>% 
  filter(grepl('^IPI\\s', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  dplyr::select(StationCode, SampleDate, IPI, Ev_FlowHab_score, H_AqHab_score, H_SubNat_score, PCT_SAFN_score, XCMG_score) %>% 
  mutate(
    yr = year(SampleDate)
  ) %>% 
  dplyr::select(-SampleDate) %>% 
  gather('var', 'val', -StationCode, -yr) %>% 
  mutate(
    var = gsub('\\_score$', '', var)
  ) %>% 
  spread(var, val) %>% 
  gather('var', 'val', -StationCode, -yr)

##
# old ipi data
ipidec <- dat %>% 
  filter(grepl('^ipiscr\\.', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  dplyr::select(StationCode, SampleDate, IPI, Ev_FlowHab_score, H_AqHab_score, H_SubNat_score, PCT_SAFN_score, XCMG_score) %>% 
  mutate(
    yr = year(SampleDate)
  ) %>% 
  dplyr::select(-SampleDate) %>% 
  gather('var', 'val', -StationCode, -yr) %>% 
  mutate(
    var = gsub('\\_score$', '', var)
  ) 

##
# ipi data from march 2019, includes some additional scores with updated gis data
# also many duplicates from the previous files
# created in R/db_queries.R
ipimar <- dat %>% 
  filter(grepl('^ipiscrmar\\.', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  dplyr::select(StationCode, SampleDate, IPI, Ev_FlowHab_score, H_AqHab_score, H_SubNat_score, PCT_SAFN_score, XCMG_score) %>% 
  mutate(
    yr = year(SampleDate)
  ) %>% 
  dplyr::select(-SampleDate) %>% 
  gather('var', 'val', -StationCode, -yr) %>% 
  mutate(
    var = gsub('\\_score$', '', var)
  ) 

# all ipi
ipidat <- rbind(ipifeb, ipidec, ipimar) %>% 
  group_by(StationCode, yr, var) %>% 
  summarise(val = mean(val, na.rm = T)) %>% 
  ungroup %>% 
  mutate(
    yr = case_when(
      StationCode %in% c('412M08599', '405PS0030') ~ 2015, # these stations had year incorrectly labelled as 2020
      T ~ yr
    )
  )

# nutrients TN, TP, Cond -----------------------------------------------------------

cond <- dat %>% 
  filter(grepl('^SpecCond', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(StationCode, SampleDate, Result) %>% 
  mutate(
    yr = year(SampleDate), 
    Result = as.numeric(Result)
  ) %>% 
  group_by(StationCode, yr) %>% 
  summarise(Cond = mean(Result, na.rm = T)) %>% 
  gather('var', 'val', -StationCode, -yr) %>% 
  na.omit

tntpsmc <- dat %>% 
  filter(grepl('^nutrient', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(stationcode, sampledate, total_n_mgl, total_p_mgl) %>% 
  mutate(
    yr = year(sampledate)
  ) %>% 
  rename(
    StationCode = stationcode
  ) %>% 
  group_by(StationCode, yr) %>% 
  summarize(
    TN = mean(total_n_mgl, na.rm = T),
    TP = mean(total_p_mgl, na.rm = T)
  ) %>% 
  gather('var', 'val', -StationCode, -yr) %>% 
  na.omit

tntpabc <- dat %>% 
  filter(grepl('^Compiled', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(StationCode, SampleDate, AnalyteName, Result) %>% 
  filter(AnalyteName %in% c('SpecificConductivity', 'Phosphorus as P', 'Total_N_calculated', 'Total_n_calculated', 'Total_P_reported', 'Total_N_partial', 'Total_N_Partial', 'Total_P_Partial')) %>% 
  mutate(
    AnalyteName = case_when(
      AnalyteName %in% 'SpecificConductivity' ~ 'Cond', 
      AnalyteName %in% c('Phosphorus as P', 'Total_P_reported', 'Total_P_partial') ~ 'TP', 
      AnalyteName %in% c('Total_N_calculated', 'Total_n_calculated', 'Total_N_partial', 'Total_N_Partial') ~ 'TN'
    ), 
    SampleDate = case_when( # ignore warnings here
      is.na(as.numeric(SampleDate)) ~ dmy(SampleDate), 
      !is.na(as.numeric(SampleDate)) ~ as.Date(as.numeric(SampleDate), origin = '1899-12-30')
    ), 
    yr = year(SampleDate)
  ) %>% 
  group_by(StationCode, yr, AnalyteName) %>% 
  summarise(Result = mean(Result, na.rm = T)) %>% 
  spread(AnalyteName, Result) %>% 
  gather('var', 'val', -StationCode, -yr) %>% 
  na.omit

# all nutrients
nutdat <- rbind(cond, tntpsmc, tntpabc) %>% 
  group_by(StationCode, yr, var) %>% 
  summarise(val = mean(val, na.rm = T))

# combine all -------------------------------------------------------------
sqidat <- bind_rows(cscidat, ascidat, ipidat, nutdat, cramdat) %>%
  ungroup %>%
  left_join(mastid, by  = 'StationCode') %>%
  select(-StationCode, -Latitude, -Longitude) %>%
  na.omit %>% 
  group_by(MasterID, yr, var) %>%
  summarize(
    val = mean(val, na.rm = T)
  ) %>%
  spread(var, val) %>% 
  na.omit

latlon <- mastid %>% 
  filter(MasterID %in% sqidat$MasterID) %>% 
  group_by(MasterID) %>% 
  summarise(
    Latitude = mean(Latitude, na.rm = T), 
    Longitude = mean(Longitude, na.rm = T)
  )

sqidat <- sqidat %>% 
  left_join(latlon, by = 'MasterID')

# dim(sqidat)
sqidatinp <- sqidat
save(sqidatinp, file = 'data/sqidatinp.RData', compress = 'xz')

# create sqi mods ---------------------------------------------------------

data(sqidatinp)

# lookup table of bio BCG class, corresponding score, and combined categorical score
xwalk <- read.csv('raw/scoring_xwalkrc.csv', stringsAsFactors = F)

# create bio categories for fail/pass combos
# for BCG, CSCI 2, 3, 4, 5, 6 is 1.03, 0.83, 0.63, 0.33, ASCI 2, 3, 4, 5, 6 is 1.23, 0.97, 0.67, 0.3
# for reference dist thresholds, CSCI li, pa, la, vla is 0.92, 0.79, 0.63, ASCI li, pa, la, vla is 0.93, 0.83, 0.7 
sqidat <- sqidatinp %>%  
  mutate(
    CSCI_rc = cut(csci_mean, breaks = c(-Inf, 0.63, 0.79, 0.92, Inf), labels = c('vla', 'la', 'pa', 'li')), 
    CSCI_rc = as.character(CSCI_rc),
    ASCI_rc = cut(asci_mean, breaks = c(-Inf, 0.70, 0.83, 0.93, Inf), labels = c('vla', 'la', 'pa', 'li')),
    ASCI_rc = as.character(ASCI_rc)
  ) %>% 
  left_join(xwalk, by = c('CSCI_rc', 'ASCI_rc')) %>% 
  select(-CSCI_score, -ASCI_score) %>% 
  mutate(
    bio_fp = ifelse(Bio_BPJ < 0, 1, 0)
  ) %>% 
  ungroup

# get calibration/validation datasets
set.seed(500)
mydf.t<- sqidat %>% group_by(bio_fp)
my.sites <- unique(mydf.t[, c('MasterID', 'bio_fp')])
sites.cal <- sample_frac(my.sites, 0.75, replace = F) %>% 
  group_by('bio_fp')
mydf.t <- mydf.t %>% 
  mutate(
    SiteSet = ifelse(MasterID %in% sites.cal$MasterID, 'Cal', 'Val')
  ) %>% 
  select(MasterID, yr, SiteSet)
sqidat <- sqidat %>% 
  left_join(mydf.t, by = c('MasterID', 'yr', 'bio_fp'))

# separate cal, val data
caldat <- sqidat %>% 
  filter(SiteSet %in% 'Cal')
valdat <- sqidat %>% 
  filter(SiteSet %in% 'Val')

# models, glm
wqglm <- glm(bio_fp ~ log10(1 + TN) + log10(1 + TP) + Cond,
             family = binomial('logit'), data = caldat)

habglm <- glm(bio_fp ~ indexscore_cram + IPI,
              family = binomial('logit'), data = caldat)

# save to package 
save(wqglm, file = '../SQI/data/wqglm.RData', compress = 'xz')
save(habglm, file = '../SQI/data/habglm.RData', compress = 'xz')

# sample data for package
sampdat <- sqidat %>% 
  select(MasterID, yr, csci_mean, asci_mean,IPI, PCT_SAFN, H_AqHab, H_SubNat, Ev_FlowHab, XCMG, IPI, indexscore_cram, Cond, TN, TP, SiteSet) %>% 
  rename(
    CSCI = csci_mean, 
    ASCI = asci_mean
  )

save(sampdat, file = '../SQI/data/sampdat.RData', compress = 'xz')

# get SQI model results from combined data ----------------------------------

sqidat <- sqidat %>% 
  rename(
    CSCI = csci_mean, 
    ASCI = asci_mean
  ) %>% 
  sqi %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj) 

save(sqidat, file = 'data/sqidat.RData', compress = 'xz')
save(sqidat, file = '../SQI_shiny/data/sqidat.RData', compress = 'xz')
