library(tidyverse)
library(lme4)
library(lmerTest)
library(readxl)
library(jtools)
library(sjPlot)
library(broom)
library(readstata13)
library(haven)

gpa <- read_dta("UNIVERSIDAD/SEMESTRES/SEPTIMO SEMESTRE/ECONOMETRIA/SEGUNDO EXAMEN/gpa.dta")
View(gpa)
gpa <- as_tibble(gpa)

n1 <- gpa %>%
  group_by(student) %>%
  summarise(ocasiones = n()) %>%
  ungroup() %>%
  group_by(ocasiones) %>%
  summarise(frecuencia = n()) %>%
  ungroup() %>%
  mutate(porcentaje = round(100 * frecuencia / sum(frecuencia), 2))

knitr::kable(n1, align = "c")

# Modelo nulo  (Intercepto alateorio)
M0 <- lmer(gpa ~ 1 + (1|student), REML = FALSE, data = gpa)
summary(M0)
summ(M0)

# M1 : Tiempo  (Intercepto alateorio)
M1 <- lmer(gpa ~ time + (1|student), REML = FALSE, data = gpa)
summary(M1)

#M2 : Tiempo + Horas de trabajo (Intercepto alateorio)
M2 <- lmer(gpa ~ time + job+ (1|student), REML = FALSE, data = gpa)
summary(M2)

#M3 : Tiempo + Highschool + Genero (Intercepto alateorio)
M3 <- lmer(gpa ~ time + highgpa + sex + (1|student), REML = FALSE, data = gpa)
summary(M3)

# Comparacion de todos los modelos 
tab_model(M0, M1, M2, M3, dv.labels = c("Modelo 0", "Modelo 1", "Modelo 2", "Modelo 3"))


#M5
M5 <- lmer(gpa ~ time + job + highgpa + sex + (1+time|student), REML = FALSE, data = gpa)
summary(M5)


#M6
M6 <- lmer(gpa ~ time + job + highgpa + sex + sex*highgpa + (1+time|student), REML = FALSE, data = gpa)
summary(M6) 

# Comparacion de todos los modelos 
tab_model(M5, M6, dv.labels = c("Modelo 5", "Modelo 6"))


gpa %>% 
  mutate(Pred2 = 2.579e+00 + 1.040e-0*time - 1.199e-01*job + 7.299e-0*highgpa + 2.176e-02*sex + 3.166e-02*highgpa*sex) %>% 
  group_by(time) %>% 
  summarise(media = mean(Pred2), 
            lim_sup = media + sd(Pred2), 
            lim_inf = media - sd(Pred2)) %>% 
  ungroup() %>% 
  ggplot(aes(x=time)) +
  geom_line(aes(y=media), linewidth=1, col="steelblue") +
  geom_line(aes(y=lim_sup), linewidth=0.9, linetype=2, col="brown2") +
  geom_line(aes(y=lim_inf), linewidth=0.9, linetype=2, col="brown2") +
  scale_y_continuous(breaks = seq(0, 16, 1)) +
  labs(y="Desempeño", x="Tiempo") +
  theme_nice()
