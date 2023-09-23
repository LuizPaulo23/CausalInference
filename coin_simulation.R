#' @title Simulação de lançamento de moedas 
#' @author Luiz Paulo T. Gonçalves 

rm(list = ls())
graphics.off()

# Dependências -----------------------------------------------------------------
# library(cowplot)
pacman::p_load(tidyverse,cowplot)

# Simule o lançamento de uma moeda (0 = Cara, 1 = Coroa)
coin_simulation <- function(releases = as.integer()){
  
  coin_sample = base::sample(0:1, 
                            size = releases, replace = T) %>% 
                base::table() %>% data.frame() %>% 
                # Calculando as proporções 
                dplyr::mutate(pct = (Freq/releases)*100)
  
  return(coin_sample)
  # cat("Proporção de Caras (CA):", coin_sample$pct[1], "\n")
  # cat("Proporção de Coroas (CO):", coin_sample$pct[2], "\n")     
  
}

# coin_simulation(releases = 100000)
# Vetor de n simulações --------------------------------------------------------
n <- c(1:100000)

results <- data.frame(releases = integer(),
                      pct_cara = numeric(),
                      pct_coroa = numeric())

# Realizando a simulação -------------------------------------------------------

for (i in n) {
  coin_data <- coin_simulation(i)
  results <- dplyr::bind_rows(results,
                              base::data.frame(releases = i,
                                               pct_cara = coin_data$pct[1],
                                               pct_coroa = coin_data$pct[2]))
}

# ------------------------------------------------------------------------------
# Plote com a simulação --------------------------------------------------------
ggplot2::ggplot(results, aes(x = releases)) +
         geom_line(aes(y = pct_cara,
                       color = "Cara"),
                       linetype = "solid") +
        geom_line(aes(y = pct_coroa, 
                      color = "Coroa"),
                      linetype = "solid") +
        scale_color_manual(values = c("Cara" = "black", "Coroa" = "red")) +
        labs(x = "", y = "") +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        labs(color = "Lado da Moeda", 
             title = "Simulando o lançamento de uma moeda: n = 100.000") +
        theme_bw()+
        theme(legend.title = element_blank(), 
              legend.position = "bottom")

# n_1000 <- ggplot2::ggplot(results, aes(x = releases)) +
#   geom_line(aes(y = pct_cara,
#                 color = "Cara"),
#             linetype = "solid") +
#   geom_line(aes(y = pct_coroa, 
#                 color = "Coroa"),
#             linetype = "solid") +
#   scale_color_manual(values = c("Cara" = "black", "Coroa" = "red")) +
#   labs(x = "Número de Lançamentos", y = "") +
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#   labs(color = "Lado da Moeda", 
#        title = "n = 1000", 
#        caption = "Fonte: elaborado por Luiz Paulo T. Gonçalves") +
#   theme_bw()+
#   theme(legend.title = element_blank(), 
#         legend.position = "bottom")
# 
# plot_grid(n_100, n_1000, nrow = 2)
