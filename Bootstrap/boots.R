#' @title  Bootstrap
#' @author Luiz Paulo Tavares 

base::rm(list = ls()) # Limpando a base 
graphics.off()
set.seed(123) # Semente geradora 

# Dependências -----------------------------------------------------------------
pacman::p_load(boot, tidyverse, ISLR, tidymodels)

# FUN bootstrap 
# data(Auto)

fun_boot <- function(data, index){
  
  return(coef(stats::lm(formula = mpg ~ horsepower, 
                        data = data, subset = index)))
  
}

# Teste com a base inteira -----------------------------------------------------
test_boot <- fun_boot(data = Auto, index = 1:392)
print(test_boot)

# 2 reamostragens com repetição 

test_boot_repet <- fun_boot(data = Auto, 
                            index = sample(392, 392, replace = T))
print(test_boot_repet)


bootstrap = boot::boot(Auto, fun_boot, 10) %>% print()
boot::boot.ci(boot.out = bootstrap, conf = 0.95, type = "all")

# Testes com base de car =======================================================

rm(list = ls())
model_linear <- stats::lm(formula = Sepal.Length ~ Petal.Length, data = iris)
summary(model_linear)
confint(model_linear)

model_boot <- car::Boot(model_linear, R = 1000)

betas_boots <- data.frame(model_boot[["t"]]) %>% 
               janitor::clean_names()
mean(betas_boots$petal_length)
summary(model_boot)

print(confint(model_boot, level=.95, type = "norm"))
hist(model_boot)

# ------------------------------------------------------------------------------
# Com tidymodels

ggplot(mtcars, aes(mpg, wt)) + 
  geom_point()


# nlsfit <- nls(mpg ~ k / wt + b, mtcars, start = list(k = 1, b = 0))
# summary(nlsfit)

mlfit <- stats::lm(formula = mpg ~ wt, data = mtcars)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  #geom_line(aes(y = predict(mlfit)))
geom_smooth(method = "lm")


set.seed(123)
boots <- rsample::bootstraps(mtcars, times = 2000, apparent = TRUE)
boots

fit_nls_on_bootstrap <- function(split) {
  nls(mpg ~ k / wt + b, analysis(split), start = list(k = 1, b = 0))
}

boot_models <-
  boots %>% 
  mutate(model = map(splits, fit_nls_on_bootstrap),
         coef_info = map(model, tidy))

boot_coefs <- 
  boot_models %>% 
  unnest(coef_info)

boot_coefs

# ==============================================================================

iris %>% 
  ggplot(aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(colour = "orange", alpha = 0.6) +
  geom_smooth(method = "lm") +
  #stat_cor(method = "pearson", cor.coef.name = "r", p.accuracy = 0.001, r.accuracy = 0.01) +
  labs(x = "Sepal length", y = "Petal length") +
  theme_bw()

point <- iris %>%
  nest(data = -c(Species)) %>% 
  mutate(model = map(data, ~lm(Sepal.Length ~ Petal.Length, data = .x)), 
         coefs = map(model, tidy)) %>% 
  unnest(coefs) %>% 
  select(-data, -model)

point


iris %>% 
  ggplot(aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  #stat_cor(method = "pearson", cor.coef.name = "r", p.accuracy = 0.001, r.accuracy = 0.01) +
  labs(x = "Sepal length", y = "Petal length") +
  theme_test()
###############################################################################
rm(list = ls())
# install.packages("ggResidpanel", dependencies = TRUE)
model_linear <- stats::lm(formula = Sepal.Length ~ Sepal.Width, data = iris)
ggResidpanel::resid_panel(model_linear)

set.seed(08182007)
library(abd)
data("LionNoses")
nboot <- 10000 # number of bootstrap samples
nobs <- nrow(LionNoses)
bootcoefs <- matrix(NA, nboot, 2)
for(i in 1:nboot){
  # Create bootstrap data set by sampling original observations w/ replacement  
  bootdat <- LionNoses[sample(1:nobs, nobs, replace=TRUE),] 
  # Calculate bootstrap statistic
  lmboot <- lm(age ~ proportion.black, data = bootdat)
  bootcoefs[i,] <- coef(lmboot)
}
par(mfrow = c(1, 2))
hist(bootcoefs[,1], main = expression(paste("Bootstrap distribution of ", hat(beta)[0])), xlab = "")
hist(bootcoefs[,2], main = expression(paste("Bootstrap distribution of ", hat(beta)[1])), xlab = "")












## =============================================================================
rm(list = ls())
data_base = datasets::mtcars

test_model = stats::lm(formula =  mpg ~ wt, data = data_base)
summary(test_model)

boots <- rsample::bootstraps(data = data_base, 
                             times = 1000, apparent = TRUE)
boots %>% print()

model_bootstrap <- function(split) {
 stats::lm(formula =  mpg ~ wt,analysis(split))
}



boot_models <- boots %>% 
               dplyr::mutate(model = purrr::map(splits, model_bootstrap),
                             coef_info = map(model, tidy))

boot_coefs <- boot_models %>% unnest(coef_info)

percentile_intervals <- int_pctl(boot_models, coef_info)
percentile_intervals

ggplot(boot_coefs, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap( ~ term, scales = "free") +
  labs(title="", subtitle = "mpg ~ wt - Linear Regression Bootstrap Resampling") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(caption = "95% Confidence Interval Parameter Estimates, Intercept + Estimate") +
  geom_vline(aes(xintercept = .lower), data = percentile_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = percentile_intervals, col = "blue")

boot_aug <- boot_models %>% 
  sample_n(50) %>% 
  mutate(augmented = map(model, augment)) %>% 
  unnest(augmented)

ggplot(boot_aug, aes(wt, mpg)) +
  geom_line(aes(y = .fitted, group = id), alpha = .3, col = "blue") +
  geom_point(alpha = 0.005) +
  # ylim(5,25) +
  labs(title="", subtitle = "mpg ~ wt \n Linear Regression Bootstrap Resampling") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(caption = "coefficient stability testing") 

# ------------------------------------------------------------------------------
rm(list = ls())
# install.packages("nlraa")
data(barley, package = "nlraa")
pacman::p_load(tidyverse)
## Quick visualization
ggplot(data = barley, aes(x = NF, y = yield)) + geom_point()

## Linear-plateau model
## The function SSlinp is in the 'nlraa' package
fit.lp <- nls(yield ~ nlraa::SSlinp(NF, a, b, xs), data = barley)

## Visualize data and fit
ggplot(data = barley, aes(x = NF, y = yield)) + 
  geom_point() + 
  geom_line(aes(y = fitted(fit.lp)))

summary(fit.lp)
confint(fit.lp)
plot(profile(fit.lp, "a"))
## This one is fairly symetric and the normal approximation is reasonable
plot(profile(fit.lp, "b"))
data_base = mtcars
test_model = stats::lm(formula =  mpg ~ wt, data = data_base)
summary(test_model)
confint(test_model)
plot(profile(test_model, "wt"))


