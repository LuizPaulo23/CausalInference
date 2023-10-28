#' @title Teorema de Glivenko Cantelli 
#' @author Luiz Paulo 

rm(list = ls())
set.seed(123)
par(mfrow = c(1, 2))

# Simulando dados  pseudo-aleatórios -------------------------------------------
n_sample = 1000 
norm_random = stats::rnorm(n = n_sample, mean = 600, sd = 50)

# Função empírica 
Fn = stats::ecdf(norm_random)
plot(Fn, main = "Convergência com o teorema Glivenko-Cantelli",
     sub = "n = 1000",
     #Convergência com o teorema Glivenko-Cantelli
     xlab = "n")
# probabilidade acumulada da normal 
curve(pnorm(x, mean = 600, sd = 50), 
      add = T, col = "red")


