# descriptor colors for sqi
getdsccol <- function(dscin = NULL, palout = F, palfac = NULL){
  
  if(is.factor(dscin))
    stop('dscin must be character')
  
  # relative severity levels
  l1 <- '#008000' #'Green'
  l2 <- '#90EE90' #'LightGreen'
  l3 <- '#FFB6C1' #'LightPink'
  l4 <- '#DC143C' #'Crimson'
  
  # additional cols for specific sqi categories
  l5 <- '#4F94CD' #'steelblue3'
  l6 <- '#00C5CD' #'turquoise3'
  l7 <- '#8B0000' #'red4'
  
  if(palout) return(c(l1, l2, l3, l4, l5, l6, l7))
  
  #  color categories
  StreamHealthIndex <- list(
    'Healthy and unstressed' = l1, 
    'Healthy and resilient' = l2,
    'Impacted by unknown stress' = l3,
    'Impacted and stressed' = l4
  )                                   
  BiologicalCondition <- list(
    'Healthy' = l1,
    'Impacted for CSCI' = l5,
    'Impacted for ASCI' = l6,
    'Impacted for CSCI and ASCI' = l4
  )
  OverallStressCondition <- list(
    'Low' = l1, 
    'Moderate' = l5,
    'Severe' = l4
  )
  OverallStressCondition_detail <- list(
    'Low stress' = l1, 
    'Stressed by chemistry degradation' = l5,
    'Stressed by habitat degradation' = l6,
    'Stressed by chemistry and habitat degradation' = l4, 
    'Stressed by low levels of chemistry or habitat degradation' = l7
  )
  
  # return individual palette with label is provided
  if(!is.null(palfac)){
    out <- get(palfac) %>% 
      unlist %>% 
      data.frame(nms = names(.), col = ., stringsAsFactors = F)
    return(out)
  }
  
  # get all
  allcol <- c(StreamHealthIndex, BiologicalCondition, OverallStressCondition, OverallStressCondition_detail)
  allcol <- unlist(allcol)
  
  # select colors
  out <- allcol[dscin]
  
  return(out)
  
}

######
# get legend from an existing ggplot object
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# function for formatting p-values in tables
p_ast <- function(x){
  
  sig_cats <- c('*p* < 0.001', '*p* < 0.01', '*p* < 0.05', 'ns')
  sig_vals <- c(-Inf, 0.001, 0.01, 0.05, Inf)
  
  out <- cut(x, breaks = sig_vals, labels = sig_cats, right = FALSE)
  out <- as.character(out)
  
  return(out)
  
}

strs_surf <- function(xvar, mod = c('hab_mod', 'wq_mod'), mod_in = NULL, title = TRUE, lenv = 200, opt_vrs = NULL, low = "#2c7bb6", mid = "#ffffbf", high = "#d7191c"){

  # get mod arg
  mod <- match.arg(mod)

  # hab and wq vars
  hab_vrs <- c('hy', 'PCT_SAFN', 'XCMG')
  wq_vrs <- c('TN', 'TP', 'Cond')

  # rng and avgs for habitat/wq variables
  # averages from calibration data, all stations/dates
  rng_vrs <- tibble::tibble( 
    var = c(hab_vrs, wq_vrs),
    minv = c(25, 0, 0, 0, 0, 0),
    avev = c(76.1, 0.62, 0.53, 1.92, 0.232, 1615),
    maxv = c(100, 1, 1, 1.5, 0.3, 2000),
    modv = c('hab_mod', 'hab_mod', 'hab_mod', 'wq_mod', 'wq_mod', 'wq_mod')
  ) %>% 
    gather('rng', 'val', minv, avev, maxv)

  ## sanity checks
  # habitat
  if(mod == 'hab_mod'){

    # chk xvar and yvar are in hab_vrs
    chk <- any(!c(xvar) %in% hab_vrs)
    if(chk)
      stop('xvar and yvar must be one of ', paste(hab_vrs, collapse = ', '))

    # check the optional variables if provided
    if(!is.null(opt_vrs)){

      # check if names are right
      chk <- any(!names(opt_vrs) %in% hab_vrs)
      if(chk)
        stop('Names in opt_vrs must match those in ', paste(hab_vrs, collapse = ', '))

    }

    # water quality
  } else {

    # chk xvar and yvar are in wq_vrs
    chk <- any(!c(xvar) %in% wq_vrs)
    if(chk)
      stop('xvar and yvar must be one of ', paste(wq_vrs, collapse = ', '))

    # check the optional variables if provided
    if(!is.null(opt_vrs)){

      # check if names are right
      chk <- any(!names(opt_vrs) %in% wq_vrs)
      if(chk)
        stop('Names in opt_vrs must match those in ', paste(wq_vrs, collapse = ', '))

    }

  }

  # replace values in rng_vrs with those in opt_vrs
  # get probabilty from model for point
  if(!is.null(opt_vrs)){

    # opt_vrs in correct format
    opt_vrs <- opt_vrs %>%
      enframe('var', 'avev') %>%
      unnest() %>%
      gather('rng', 'val', avev)

    # join rng_vars with opt_vrs and replace
    rng_vrs <- rng_vrs %>%
      left_join(opt_vrs, by = c('var', 'rng')) %>%
      mutate(val = ifelse(is.na(val.y), val.x, val.y)) %>%
      dplyr::select(-val.x, -val.y)

    # data from opt_vrs to plot as single point
    # ceiling and floor by ranges in rng_vars
    toprd <- rng_vrs %>%
      spread(rng, val) %>%
      mutate(
        avev = pmin(avev, maxv),
        avev = pmax(avev, minv)
      ) %>%
      filter(var %in% opt_vrs$var) %>%
      dplyr::select(var, avev) %>%
      spread(var, avev)

  }

  # subset correct model
  rng_vrs <- rng_vrs %>%
    filter(modv %in% mod)

  # get xy vars to plot
  xy_vrs <- rng_vrs %>%
    filter(var %in% !!xvar) %>%
    filter(!rng %in% 'avev') %>%
    group_by(var) %>%
    nest() %>%
    mutate(
      val = purrr::map(data, ~ seq(min(.$val), max(.$val), length.out = lenv))
    ) %>%
    dplyr::select(-data)

  # get constant vars
  cnt_vrs <- rng_vrs %>%
    filter(!var %in% !!xvar) %>%
    filter(rng %in% 'avev') %>%
    dplyr::select(-modv, -rng)

  # combined data to pred
  prd_vrs <- rbind(xy_vrs, cnt_vrs) %>%
    deframe() %>%
    expand.grid

  # modelled response surface
  rspse <- paste0('predict(', mod_in, ', newdata = prd_vrs, type = "response", se = T)')
  rspse <- eval(parse(text = rspse))

  # combined predictation data and response
  toplo <- prd_vrs %>%
    mutate(
      `fit` = rspse$fit,
      `fitse` = rspse$se
    )

  return(toplo)

}

#' get sqi tally of combos based on threshold for good/bad combined bio
#' 
#' @param sqidat input sqi data without sqi calculated, sqidatinp
#' @param biocut numeric for value defining limit of healthy/impacted biology for combined asci/csci categories (in xwalk), values less than this thresold are impacted 
#' @param hithrsh numeric indicating upper threshold for high stress
#' @param lothrsh numeric indicating lower threshold for low stress
#' @param talvals logical if tallys as counts for each category or the raw data are returned
#' 
sqibiosens <- function(sqidat, biocut = 0, lothrsh = 0.1, hithrsh = 0.9, talvals = TRUE){
  # lookup table of bio BCG class, corresponding score, and combined categorical score
  xwalk <- read.csv('raw/scoring_xwalkrc.csv', stringsAsFactors = F)
  
  # create bio categories for fail/pass combos
  # for BCG, CSCI 2, 3, 4, 5, 6 is 1.03, 0.83, 0.63, 0.33, ASCI 2, 3, 4, 5, 6 is 1.23, 0.97, 0.67, 0.3
  # for reference dist thresholds, CSCI li, pa, la, vla is 0.92, 0.79, 0.63, ASCI li, pa, la, vla is 0.93, 0.83, 0.7 
  sqidat <- sqidat %>%  
    mutate(
      CSCI_rc = cut(csci_mean, breaks = c(-Inf, 0.63, 0.79, 0.92, Inf), labels = c('vla', 'la', 'pa', 'li')), 
      CSCI_rc = as.character(CSCI_rc),
      ASCI_rc = cut(asci_mean, breaks = c(-Inf, 0.70, 0.83, 0.93, Inf), labels = c('vla', 'la', 'pa', 'li')),
      ASCI_rc = as.character(ASCI_rc)
    ) %>% 
    left_join(xwalk, by = c('CSCI_rc', 'ASCI_rc')) %>% 
    select(-CSCI_score, -ASCI_score) %>% 
    mutate(
      bio_fp = ifelse(Bio_BPJ < biocut, 1, 0)
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
  
  datin <- sqidat %>% 
    rename(
      CSCI = csci_mean, 
      ASCI = asci_mean
    ) 
  
  ##
  # probability of stress, chem, hab, and overall
  # model predicts probably of low stress
  
  # glm predictions
  datin$pChem <- predict(wqglm, newdata = datin, type = "response")
  datin$pHab <- predict(habglm, newdata = datin, type = "response")
  
  # combo stress estimate
  datin$pChemHab<- 1 - ((1 - datin$pChem) * (1 - datin$pHab))

  out <- datin %>%
    dplyr::mutate(
      BiologicalCondition = ifelse(CSCI>=0.79 & ASCI>=0.83,"Healthy",
                                   ifelse(CSCI<0.79 & ASCI<0.83,"Impacted for CSCI and ASCI",
                                          ifelse(CSCI<0.79 & ASCI>=0.83,"Impacted for CSCI",
                                                 ifelse(CSCI>=0.79 & ASCI<0.83,"Impacted for ASCI", "XXXXX"
                                                 )))
      ),
      WaterChemistryCondition = cut(pChem,
                                    breaks = c(-Inf, lothrsh, hithrsh, Inf),
                                    labels = c('Low', 'Moderate', 'Severe'),
                                    right = F
      ),
      HabitatCondition = cut(pHab,
                             breaks = c(-Inf, lothrsh, hithrsh, Inf),
                             labels = c('Low', 'Moderate', 'Severe'),
                             right = F
      ),
      OverallStressCondition = cut(pChemHab,
                                   breaks = c(-Inf, lothrsh, hithrsh, Inf),
                                   labels = c('Low', 'Moderate', 'Severe'),
                                   right = F
      ),
      OverallStressCondition_detail = ifelse(pChemHab<hithrsh,"Low stress",
                                             ifelse(pChem>=hithrsh & pHab>=hithrsh, "Stressed by chemistry and habitat degradation",
                                                    ifelse(pChem>=hithrsh & pHab<hithrsh, "Stressed by chemistry degradation",
                                                           ifelse(pChem<hithrsh & pHab>hithrsh, "Stressed by habitat degradation",
                                                                  ifelse(pChem<hithrsh & pHab<hithrsh, "Stressed by low levels of chemistry or habitat degradation",
                                                                         "XXXXX"))))
      ),
      StreamHealthIndex = ifelse(BiologicalCondition=="Healthy" & OverallStressCondition!="Severe","Healthy and unstressed",
                                 ifelse(BiologicalCondition=="Healthy" & OverallStressCondition=="Severe","Healthy and resilient",
                                        ifelse(BiologicalCondition!="Healthy" & OverallStressCondition!="Severe","Impacted by unknown stress",
                                               ifelse(BiologicalCondition!="Healthy" & OverallStressCondition=="Severe","Impacted and stressed",
                                                      "XXXXX")))
      )
    ) %>% 
    dplyr::mutate_if(is.factor, as.character)

  # summarize output
  if(talvals){
    
    sums <- out %>% 
      dplyr::select(BiologicalCondition, WaterChemistryCondition, HabitatCondition, OverallStressCondition, OverallStressCondition_detail, StreamHealthIndex) %>% 
      gather('var', 'val', everything()) %>% 
      group_by(var, val) %>% 
      tally 

  # otherwise returned site cateogories without tallys     
  } else {
    
    sums <- out %>% 
      dplyr::select(MasterID, yr, BiologicalCondition, WaterChemistryCondition, HabitatCondition, OverallStressCondition, OverallStressCondition_detail, StreamHealthIndex) %>% 
      gather('var', 'val', -MasterID, -yr) 
    
  }
  
  return(sums)
  
}

#####
# vif function
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

######
# get suffix for percentiles
perc_suff <- function(x){
  
  x <- x %>% round(2) %>% `*`(100)
  
  suff <- case_when(x %in% c(11,12,13) ~ "th",
                    x %% 10 == 1 ~ 'st',
                    x %% 10 == 2 ~ 'nd',
                    x %% 10 == 3 ~'rd',
                    TRUE ~ "th")

  out <- paste0(x, suff)
  
  return(out)
  
}