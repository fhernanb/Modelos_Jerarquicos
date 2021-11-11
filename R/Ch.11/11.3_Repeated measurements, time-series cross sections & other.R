# ------------------------------------------------------------------------------
# Archivo modificado a partir del original -------------------------------------
# ------------------------------------------------------------------------------

library(dplyr)
library(magrittr)
library(ggplot2)

datos <- read.table(file="Datos/smoking/smoke_pub.dat", header=TRUE, sep="\t")
head(datos)

# Vamos a modificar el nombre de las variables
names(datos) <- c("id", "female", "parents_smoke", "wave", "y")
head(datos)

# Vamos a crear la variable sexo con sus dos niveles
datos %<>% mutate(sex = ifelse(female == 1, "female", "male"))
head(datos)

# Para crear figura 11.5

datos %>% 
  group_by(wave, sex) %>% 
  summarize(prop_smokers = mean(y)) -> datos_fig_11_5

ggplot(data = datos_fig_11_5) + 
  geom_line(mapping = aes(x = wave, y = prop_smokers, color = sex)) +
  ylim(0.0, 0.2) +
  scale_x_continuous(breaks = 1:6)

# Cuantos hombres y cuantas mujeres participaron
datos %>% 
  group_by(id) %>% 
  dplyr::select(sex) %>% 
  unique() %>% 
  ungroup() %>% 
  dplyr::select(sex) %>% 
  table()                        # Yo creo que se puede mejorar este codigo

datos %>%
  dplyr::select(id, sex) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  ungroup() %>% 
  dplyr::select(sex) %>% 
  table()                        # No me convence


# Cuantas obs tiene cada persona
datos %>% 
  group_by(id) %>% 
  summarize(num_obs = n())

# Para ajustar el modelo usando el paquete lme4
library(lme4)

mod1 <- glmer(y ~ 1 + parents_smoke + female + wave + (1 | id), 
              data=datos, 
              family = binomial(link="logit"))

summary(mod1)

# Para ajustar el modelo usando el paquete MASS
library(MASS)

mod2 <- glmmPQL(y ~ 1 + parents_smoke + female + wave,
                random = ~ 1 | id, 
                data = datos,
                family = binomial(link="logit"),
                verbose = FALSE)

summary(mod2)

# Para ajustar el modelo usando el paquete brms
library(brms)

bprior1 <- prior(student_t(5, 0, 10), class = b) +
  prior(cauchy(0, 2), class = sd)

mod3 <- brm(y ~ 1 + parents_smoke + female + wave + (1 | id), 
            data=datos, 
            family=bernoulli(link = "logit"),
            prior=bprior1)


