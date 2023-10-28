#' @title  Permutação: Covariância e Correlação de Pearson | R
#' @author Luiz Paulo Tavares 

base::rm(list = ls()) # Limpando a base 
graphics.off()
set.seed(5841) # Semente geradora 

# Dependências 
pacman::p_load(tidyverse, GGally, knitr)

# Métricas robustas de centralidade e permutação -------------------------------
# Base de dados pseudo-aleatórios 
ps_sample <- function(n = as.integer()){
  
  data_raw = base::data.frame(value = rnorm(n, mean = 0, sd = 1), 
                              ids = rep(c("Group 1", "Group 2"), c(n/2, n/2))) %>% 
             dplyr::relocate(ids, .after = NULL)
  
  return(data_raw)
  
}

data_raw = ps_sample(n = 1000)

# Métricas ---------------------------------------------------------------------

metrics <- data_raw %>% 
           dplyr::group_by(ids) %>% 
                  summarise(mean_value = mean(value), 
                            median_value = median(value), 
                            desvio = sd(value), 
                            var = var(value)) %>% print()

# Diferença absoluta entre as médias -------------------------------------------

dif_absoluta <- base::abs(diff(as.vector(metrics$mean_value))) %>% print()

## Repetir a operação n vezes --------------------------------------------------

results <- c() # Para Preenchimento 
j = 10000

for(i in 1:j){
  
  results[i] <- base::abs(diff(tapply(sample(data_raw$value), data_raw$ids, mean)))
  
}

data_base = data.frame(difs = unlist(results))

# Número de diferenças absolutas  ----------------------------------------------
sum(results >= dif_absoluta)

# Visualizando ----------------------------------------------------------------- 

ggplot(data_base, aes(x = difs)) +
  geom_histogram(fill = "orange", 
                 alpha = 0.7) +
  geom_vline(color = "black",
             lwd = 2,
             lty=1,
             xintercept = dif_absoluta) +
  annotate("text", x = dif_absoluta, y = Inf, label = "Diferença", vjust = 1, hjust = 0)+
  labs(title = paste0("Diferença absoluta entre as médias com ",j, " reamostragens"), 
       caption = "Fonte: elaboração de Luiz Paulo Tavares", 
       y = "Contagem", 
       x = "Diferença absoluta das médias")+
  theme(plot.caption = element_text(hjust = 0))+
  theme_bw()
%>% 