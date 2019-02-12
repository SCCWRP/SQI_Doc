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

strs_surf <- function(xvar, mod = c('hab_mod', 'wq_mod'), mod_in = NULL, title = TRUE, lenv = 200, opt_vrs = NULL, low = "#2c7bb6", mid = "#ffffbf", high = "#d7191c"){
  
  # get mod arg
  mod <- match.arg(mod)
  
  # hab and wq vars
  hab_vrs <- c('indexscore_cram', 'PCT_SAFN', 'H_AqHab', 'H_SubNat', 'Ev_FlowHab', 'XCMG')
  wq_vrs <- c('TN2', 'TP', 'Cond')
  
  # rng and avgs for habitat/wq variables
  # averages from calibration data, all stations/dates
  rng_vrs <- tibble::tibble( 
    var = c(hab_vrs, wq_vrs),
    minv = c(24, 0, 0, 0, 0, 0, 0, 0, 0),
    avev = c(69.3, 38, 1.33, 1.3, 0.548, 108, 1.92, 0.232, 1615),
    maxv = c(100, 100, 2.5, 2.5, 1, 264, 1.5, 1, 2000),
    modv = c('hab_mod', 'hab_mod', 'hab_mod', 'hab_mod', 'hab_mod', 'hab_mod', 'wq_mod', 'wq_mod', 'wq_mod')
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
      `fit` = 1 - rspse$fit, 
      `fitse` = rspse$se
    )
  
  return(toplo)
  
}