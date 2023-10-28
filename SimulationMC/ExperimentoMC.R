#' @title Simulações - Monte Carlo 
#' @author Luiz Paulo 

base::rm(list = ls())

# Dependências 
pacman::p_load(tidyverse)

# Monte Carlo ------------------------------------------------------------------
MCSimulation <- function(intercept, beta, n, repetitions, mu, sigma){
  intercept_mc = numeric(repetitions) 
  beta_mc = numeric(repetitions) 
  intervalo_97.5 = numeric(repetitions)
  intervalo_2.5 = numeric(repetitions)
  
  for (i in 1:repetitions) {
    
    # x = stats::rnorm(n) 
    # y <- intercept + beta * x + stats::rnorm(length(x))
    x = rnorm(n = n,mean = mu,sd = sigma)
    y <- intercept + beta *x + rnorm(n = n,mean = 0,sd = sigma)
    # y <- intercept + beta * x + 0 # considerando resíduo = zero  
    model_lmMC <- stats::lm(y ~ x)
    intercept_mc[[i]] <- base::as.vector(model_lmMC$coefficients[1])
    beta_mc[[i]] <- base::as.vector(model_lmMC$coefficients[2])
    intervalo_2.5[[i]] <- base::as.vector(stats::confint(model_lmMC)[2])
    intervalo_97.5[[i]] <- base::as.vector(stats::confint(model_lmMC)[4])
    
    
  }
  
  MonteCarlo = data.frame(target = y,
                          observations = x,
                          intercept = intercept_mc, 
                          beta = beta_mc, 
                          intervalo_2.5 = intervalo_2.5, 
                          intervalo_97.5 = intervalo_97.5)
  
  return(MonteCarlo)
  
}

# Estimando --------------------------------------------------------------------

data_raw <- datasets::iris %>% 
            janitor::clean_names() %>% glimpse()

model_lm <- stats::lm(formula = sepal_length ~ petal_length, 
                      data = data_raw)
summary(model_lm)
stats::shapiro.test(model_lm$residuals)
confint(model_lm)
inter_2.5 = stats::confint(model_lm)[2]
inter_9.7 = stats::confint(model_lm)[4]

# Visualizando a relação -------------------------------------------------------

ggplot2::ggplot(data = data_raw)+
         aes(y = sepal_length, x = petal_length)+
         geom_point(pch = 19)+
         geom_smooth(method = "loess")+
         theme_bw()

intercept_model = model_lm$coefficients[1] %>% print()
beta_model = model_lm$coefficients[2] %>% print()

# in_hand = intercept_model + beta_model*1.6
# print(in_hand)

# Usando simulação de Monte Carlo 

model_mc = MCSimulation(intercept = intercept_model,
                        mu = mean(model_lm$model$petal_length),
                        sigma = summary(model_lm)$sigma,
                        #sd(model_lm$model$petal_length),
                        beta = beta_model, 
                        repetitions = 1000,
                        n = 100)

# Estatísticas gerais ----------------------------------------------------------
media_2.5 = mean(model_mc$intervalo_2.5)
media_97.5 = mean(model_mc$intervalo_97.5)

minimo <- min(model_mc$beta) %>% print()
media <- base::mean(model_mc$beta) %>% print()
maximo <- max(model_mc$beta) %>% print()
desvio_padrao <- stats::sd(model_mc$beta) %>% print()

ggplot() +
  geom_histogram(data = data.frame(x = model_mc[[4]]), aes(x = x),
                 fill = "darkorange", 
                 position = "identity",
                 color = "black",
                 bins = 30, 
                 alpha = 0.8) +
  geom_vline(xintercept = minimo,
             color = "red",
             linetype = "dashed",
             size = 1)+
  geom_vline(xintercept = media,
             color = "black",
             linetype = "dashed",
             size = 1) +
  geom_vline(xintercept = maximo,
             color = "red",
             linetype = "dashed",
             size = 1) +
  labs(x = "Beta (β1)", 
       y = "Frequency",
       title = "Monte Carlo Simulation - beta (β1)",
       subtitle = paste("Mean:", round(media, 4), "| Standard deviation:", round(desvio_padrao, 4))) +
  theme_bw()

# Intervalos - teste 

intervalos <- model_mc %>% 
              dplyr::select(intervalo_2.5, 
                     intervalo_97.5) %>% 
                     dplyr::mutate(interval = (intervalo_2.5 + intervalo_97.5)/2) %>% 
                     rename("2.5%" = "intervalo_2.5", 
                            "97.5%" = "intervalo_97.5", 
                            "Média do intervalo entre 2.5% e 97.5% simulado" = "interval") %>% 
              tidyr::pivot_longer(cols = `2.5%`:`Média do intervalo entre 2.5% e 97.5% simulado`) %>% 
              filter(name == "Média do intervalo entre 2.5% e 97.5% simulado")
              
              

ggplot(intervalos, aes(x = value, fill = name)) +
  geom_histogram(position = "identity", 
                 alpha = 0.3, bins = 50) +
  scale_fill_manual(values = c(
    # "2.5%" = "black", 
    #                            "97.5%" = "orange", 
     "Média do intervalo entre 2.5% e 97.5% simulado" = "blue"))+
  geom_vline(xintercept = c(beta_model),
             color = c("red"),
             linetype = "dashed")+
  geom_vline(xintercept = c(inter_2.5),
             color = c("black"),
             linetype = "dashed")+
  geom_vline(xintercept = c(inter_9.7),
             color = c("black"),
             linetype = "dashed")+
  # geom_vline(xintercept = c(media_2.5),
  #            color = c("gray"),
  #            linetype = "dashed")+
  # geom_vline(xintercept = c(media_97.5),
  #            color = c("gray"),
  #            linetype = "dashed")+
  annotate("text", x = beta_model, y = Inf, label = "β1", vjust = 1, hjust = 0.5)+
  labs(title = "Simulação Monte Carlo: Intervalos de confiança do beta (β1)",
       caption = "Fonte: elaboração de Luiz Paulo Tavares Gonçalves",
       y = "Contagem",
       x = "β1", fill = "")+
  theme_bw()+theme(plot.caption = element_text(hjust = 0),
                   legend.position = "bottom")

