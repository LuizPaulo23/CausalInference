#' @title Simulação: probabilidade e convergência 
#' @author Luiz Paulo T. Gonçalves 

rm(list = ls())
graphics.off()
set.seed(123)

# Dependências 
pacman::p_load(tidyverse,cowplot)

# Simule o lançamento de uma moeda (0 = Cara, 1 = Coroa)
# releases = 1000

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

# Testando o simulador de lançamentos ------------------------------------------

test_coin = coin_simulation(releases = 1000)

# Vetor de n simulações --------------------------------------------------------
# n <- c(1:1000)

n_releases <- function(n = as.integer()){

n <- c(1:n)  
results <- data.frame(releases = integer(),
                      pct_cara = numeric(),
                      pct_coroa = numeric())

# Realizando a simulação -------------------------------------------------------

for (i in n) {
  coin_data <- coin_simulation(releases =  i)
  results <- dplyr::bind_rows(results,
                              base::data.frame(releases = i,
                                               pct_cara = coin_data$pct[1],
                                               pct_coroa = coin_data$pct[2]))
}
  
return(results)

}

# Testando coin_simulation

results <- n_releases(n = 1000)

# Representação gráfica da simulação --------------------------------------------------------

ggplot2::ggplot(results, aes(x = releases)) +
  geom_line(aes(y = pct_cara,
                color = "Cara"),
            linetype = "solid") +
  geom_line(aes(y = pct_coroa, 
                color = "Coroa"),
            linetype = "solid") +
  scale_color_manual(values = c("Cara" = "black", "Coroa" = "darkorange")) +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  ylim(0,100)+
  labs(color = "Lado da Moeda", 
       title = "Simulando o lançamento de uma moeda", 
       subtitle = "n = 1000", 
       caption = "Fonte: elaboração de Luiz Paulo Tavares Gonçalves") +
  theme_bw()+
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom")

# hist(results$pct_cara, col = "black")
# hist(results$pct_coroa, col = "red")

# Média ------------------------------------------------------------------------

# simulate_average <- function(n_simulations, n_launches) {
#   
#   results <- data.frame(simulation = integer(),
#                         mean_pct_cara = numeric(),
#                         mean_pct_coroa = numeric())
#   
#   for (sim in 1:n_simulations) {
#     sim_data <- n_releases(n = n_launches)
#     mean_data <- colMeans(sim_data[, c("pct_cara", "pct_coroa")], na.rm = TRUE)
#     
#     results <- rbind(results, data.frame(simulation = sim,
#                                          mean_pct_cara = mean_data[1],
#                                          mean_pct_coroa = mean_data[2]))
#   }
#   
#   return(results)
# }
# 
# 
# test_mean = simulate_average(n_simulations = 1000, n_launches = 100)
# 


n_medias <- function(n_lancamentos = as.integer(), n_vezes = as.integer()) {
  
  medias_cara <- numeric()
  medias_coroa <- numeric()
  
  for (i in 1:n_vezes) {
    resultados <- n_releases(n = n_lancamentos)
    media_cara <- mean(resultados$pct_cara, na.rm = TRUE) 
    media_coroa <- mean(resultados$pct_coroa, na.rm = TRUE) 
    medias_cara <- c(medias_cara, media_cara) 
    medias_coroa <- c(medias_coroa, media_coroa) 
  }
  
  df_medias <- data.frame(
    lancamentos = rep(n_lancamentos, n_vezes),
    media_cara = medias_cara,
    media_coroa = medias_coroa
  )
  
  return(df_medias)
}

mean_coin = n_medias(n_lancamentos = 100,n_vezes = 1000)

data_coin = mean_coin %>% 
            dplyr::rename(Cara = media_cara, 
                          Coroa = media_coroa) %>% 
            tidyr::pivot_longer(cols = Cara:Coroa)


media_cara <- mean(data_coin$value[data_coin$name == "Cara"])
media_coroa <- mean(data_coin$value[data_coin$name == "Coroa"])
shapiro.test(mean_coin$media_cara)
shapiro.test(mean_coin$media_coroa)

# Criar o gráfico de barras empilhadas (histograma) com linhas verticais nas médias
ggplot(data_coin, aes(x = value, fill = name)) +
  geom_histogram(position = "identity", 
                 alpha = 0.7, bins = 50) +
  scale_fill_manual(values = c("Cara" = "black", 
                                "Coroa" = "orange"))+
  geom_vline(xintercept = c(media_cara, media_coroa), 
             color = c("black", "orange"), 
             linetype = "dashed")+
  labs(title = "Simulação: a média de 100 lançamentos - 1000 vezes",
       subtitle = "Shapiro-Wilk: p-valor 0.74 e 0.33, coroa e cara, respectivamente.",
       caption = "Fonte: elaboração de Luiz Paulo Tavares Gonçalves",
       y = "Contagem",
       x = "%", fill = "")+
  theme_bw()+theme(plot.caption = element_text(hjust = 0),
                  legend.position = "bottom")

