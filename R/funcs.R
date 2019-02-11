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