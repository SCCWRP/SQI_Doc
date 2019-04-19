
data(sqidatinp)
strcol <- getdsccol(palfac = 'OverallStressCondition_detail')
hi <- 0.5

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



toplo <- sqidat %>% 
  rename(
    CSCI = csci_mean, 
    ASCI = asci_mean
  ) %>% 
  sqi(hithrsh = hi) %>% 
  select(pChem, pHab, pChemHab, OverallStressCondition_detail) %>% 
  mutate(OverallStressCondition_detail = factor(OverallStressCondition_detail,
                                                levels = c("Low stress", "Stressed by chemistry degradation", "Stressed by habitat degradation", "Stressed by chemistry and habitat degradation", "Stressed by low levels of chemistry or habitat degradation"),
                                                labels = c("Low stress", "Stressed by chemistry", "Stressed by habitat", "Stressed by chemistry and habitat", "Stressed by low levels of both"
                                                )))

xgrid <- seq(min(toplo$pHab, na.rm = T), max(toplo$pHab, na.rm = T), length = 100)
ygrid <- seq(min(toplo$pChem, na.rm = T), max(toplo$pChem, na.rm = T), length = 100)

mtrxd <- crossing(
  pHab = xgrid, 
  pChem = ygrid
) %>% 
  mutate(
    pChemHab = 1 - ((1 - pHab) * (1 - pChem))
  )

xin <- 0.4
labs <- tibble(
  x = c(0.3, hi, xin),
  y = c(hi, 0.6, ((xin - hi)/(xin - 1))),
  lb = paste0('italic(', c('"pHab"', '"pChem"', '"pOverall"'), ')')      
)
cols <- getdsccol(palfac = 'OverallStressCondition')$col

p2 <- ggplot(mtrxd) + 
  stat_contour(aes(x = pChem, y = pHab, z = pChemHab), breaks = c(hi), colour = 'black', linetype = 'dotted', size = 1) + 
  geom_point(data = toplo, aes(x = pChem, y = pHab, fill = OverallStressCondition_detail), pch = 21, colour = 'lightgrey', size = 4) +
  geom_vline(xintercept = hi, linetype = 'dotted', size = 1) + 
  geom_hline(yintercept = hi, linetype = 'dotted', size = 1) +
  geom_label_repel(data = labs, aes(x = x, y = y, label = lb), box.padding = 2.5, parse = T, size = 2.5, alpha = 0.75) +
  theme_bw(base_family= 'serif') + 
  theme(
    legend.position = 'top',
    legend.title = element_text(face = 'italic'), 
    axis.title = element_text(face = 'italic')
  ) + 
  scale_colour_gradientn('pOverall', colours = cols, guide = F) +
  scale_fill_manual('Stress condition', values = strcol$col) + 
  guides(fill = guide_legend(title.position = 'top', title.hjust = 0.5, ncol = 2))
pleg2 <- g_legend(p2)
p2 <- p2 + theme(legend.position = 'none')


png('C:/Users/Marcus.SCCWRP2K/Desktop/tmp4.png', height = 4.5, width = 4, family = 'serif', res = 400, units = 'in')
wrap_elements(pleg2) + p2 + plot_layout(ncol = 1, heights = c(0.25, 1))
dev.off()

