# ------------------------------------------------------------------------------
# Archivo modificado a partir del original -------------------------------------
# ------------------------------------------------------------------------------

library("arm")
library(haven)

kidiq <- read_dta("Datos/child.iq/kidiq.dta")

# attach(kidiq) # Oculte el attach porque NO ME GUSTA

## Plot Figure 3.1
library(ggplot2)

ggplot(kidiq, aes(x=mom_hs, y=kid_score)) +
  geom_jitter(width = 0.02)

## Plot Figure 3.2
ggplot(kidiq, aes(x=mom_iq, y=kid_score)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

fit1 <- lm (kid_score ~ mom_iq, data=kidiq)
summary(fit1)
