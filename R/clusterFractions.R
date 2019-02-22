#' @title ClusterFractions
#' 
#' @description Using a Seurat object, calculate the fraction of a population 
#'(e.g. a disease class or treatment group) that can be characterized by 
#'   some factor (e.g. cell types)
#'
#' @param object A Seurat object with a population to be characterized
#' @param numerator 
#' @param denominator 
#' 
#' @importFrom dplyr group_by summarise inner_join mutate
#' @importFrom ggplot2 ggplot aes
#' @importFrom magrittr %>% %<>%
#'
#' @return
#' @export
#'
#' @examples
clusterFractions <- function(object, numerator, denominator){
  numerator = enquo(numerator)
  denominator = enquo(denominator)
  
  totals <- object@meta.data %>% 
    group_by(!! denominator, !! numerator) %>% 
    summarise(count = n())
  
  totals %<>% 
    group_by(!! denominator) %>% 
    summarise(totals = sum(count)) %>% 
    inner_join(totals, by = quo_name(denominator))
  
  totals %<>% 
    mutate(fraction = (count/totals))
  
  g <- totals %>%
    ggplot(aes(x = as.factor(!! denominator),
               y = fraction,
               group = as.factor(!! numerator),
               fill = as.factor(!! numerator)))
  
  return(g)
}