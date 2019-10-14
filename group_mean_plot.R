library(tidyverse)
group_mean_plot <- function(y,x,data){
  x <- enquo(x)
  y <- enquo(y)
  data %>% 
    filter(! (!!(x) %in% c("NA", ""))) %>%
    dplyr::group_by(!! x) %>%
    dplyr::summarise(mean_y = mean(!! y, na.rm =TRUE),
                     sd_y = sd(!! y, na.rm =TRUE),
                     n = n(),
                     ll = mean_y - qt(0.975,n-1)*sd_y/sqrt(n),
                     ul = mean_y + qt(0.975,n-1)*sd_y/sqrt(n)
                     ) %>% 
    ggplot(aes(x =!!  x,y = mean_y)) + 
    geom_point() +
    geom_errorbar(aes(max = ul, min = ll), width = 0.1)
}
