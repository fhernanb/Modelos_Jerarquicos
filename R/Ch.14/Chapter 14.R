# ------------------------------------------------------------------------------
# Archivo modificado a partir del original -------------------------------------
# ------------------------------------------------------------------------------

# Ejemplo sobre votaciones

library (foreign)
url <- 'Datos/election88/polls.dta'
datos <- read.dta(file=url)

# Usando la inf del survey = 9158
datis <- subset(datos, survey == 9158)

library(lme4)
M1 <- glmer(bush ~ female + black + (1 | state), 
            data=datis, family=binomial(link="logit"))
summary(M1)

# Other information
coef(M1)
ranef(M1)
fixef(M1)

