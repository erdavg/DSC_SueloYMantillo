library(tidyverse)
library(skimr)
library(car)
library(FactoMineR)
library(ggfortify)
library(plotly)
library(factoextra)
library(GGally)

getwd()
setwd("D:\\Documentos\\Biologia UAM X\\SERVICIO\\SuelosBD\\Mantillo")
list.files()
data <- read.csv("MuestrasBosqueMaduro.csv")

skim(data)


data %>% 
  colnames()
## C SUELO
cTotalSuelo <- data %>% 
  slice(1:9) %>%
  select(X..C.total.1) %>%
  unlist() %>%
  matrix(ncol = 3) %>%
  as.data.frame() 

colnames(cTotalSuelo) <- c("ML", "MK", "MP")

cTotalSuelo <- cTotalSuelo %>%
  gather(grupo, C_Total) %>%
  arrange(grupo) %>%
  mutate(grupo = factor(grupo, levels = c("ML", "MK", "MP")))

## C MANTILLO
cTotalMantillo <- data %>% 
  slice(15:23) %>%
  select(X..C.total.1) %>%
  unlist() %>%
  matrix(ncol = 3) %>%
  as.data.frame() 

colnames(cTotalMantillo) <- c("ML", "MK", "MP")

cTotalMantillo <- cTotalMantillo %>%
  gather(grupo, C_Total) %>%
  arrange(grupo) %>%
  mutate(grupo = factor(grupo, levels = c("ML", "MK", "MP")))

## N SUELO
nTotalSuelo <- data %>% 
  slice(1:9) %>%
  select(X..N.total.1) %>%
  unlist() %>%
  matrix(ncol = 3) %>%
  as.data.frame() 

colnames(nTotalSuelo) <- c("ML", "MK", "MP")

nTotalSuelo <- nTotalSuelo %>%
  gather(grupo, N_Total) %>%
  arrange(grupo) %>%
  mutate(grupo = factor(grupo, levels = c("ML", "MK", "MP")))

## N MANTILLO
nTotalMantillo <- data %>% 
  slice(15:23) %>%
  select(X..N.total.1) %>%
  unlist() %>%
  matrix(ncol = 3) %>%
  as.data.frame() 

colnames(nTotalMantillo) <- c("ML", "MK", "MP")

nTotalMantillo <- nTotalMantillo %>%
  gather(grupo, N_Total) %>%
  arrange(grupo) %>%
  mutate(grupo = factor(grupo, levels = c("ML", "MK", "MP")))
## CN SUELO

CN_TotalSuelo <- data %>% 
  slice(1:9) %>%
  select(C.N) %>%
  unlist() %>%
  unlist() %>%
  matrix(ncol = 3) %>%
  as.data.frame() 

colnames(CN_TotalSuelo) <- c("ML", "MK", "MP")

CN_TotalSuelo <- CN_TotalSuelo %>%
  gather(grupo, CN) %>%
  arrange(grupo) %>%
  mutate(grupo = factor(grupo, levels = c("ML", "MK", "MP")))

## CN MANTILLO
CN_TotalMantillo <- data %>%
  slice(15:23) %>%
  select(P.disp.Bray.1) %>%
  unlist() %>%
  matrix(ncol = 3) %>%
  as.data.frame() 
 
colnames(CN_TotalMantillo) <- c("ML", "MK", "MP")

CN_TotalMantillo <- CN_TotalMantillo %>%
  gather(grupo, CN) %>%
  arrange(grupo) %>%
  mutate(grupo = factor(grupo, levels = c("ML", "MK", "MP")))


summary(cTotalSuelo)

cTotalMantillo


## graficas de barras ## graficas de barras ## graficas de barras 

### C TOTAL SUELO

# Crear un factor ordenado para los grupos
cTotalSuelo$grupo <- factor(cTotalSuelo$grupo, levels = c("MK", "ML", "MP"))

# Crear la gráfica de cajas
ggplot(cTotalSuelo, aes(x = grupo, y = C_Total, group = grupo)) +
  geom_boxplot() +
  labs(x = "Grupo", y = "C_Total") +
  ggtitle("C Total de Suelo")
## Prueba 
anova_Csuelo <- aov(cTotalSuelo$C_Total~cTotalSuelo$grupo)
summary(anova_Csuelo)  
TukeyHSD(anova_Csuelo)


### C TOTAL MANTILLO

# Crear un factor ordenado para los grupos
cTotalMantillo$grupo <- factor(cTotalMantillo$grupo, levels = c("MK", "ML", "MP"))

# Crear la gráfica de cajas
ggplot(cTotalMantillo, aes(x = grupo, y = C_Total, group = grupo)) +
  geom_boxplot() +
  labs(x = "Grupo", y = "C_Total") +
  ggtitle("C Total de Mantillo")
## Prueba 
anova_Cmantillo <- aov(cTotalMantillo$C_Total~cTotalMantillo$grupo)
summary(anova_Cmantillo)  
TukeyHSD(anova_Cmantillo)

### N TOTAL SUELO

# Crear un factor ordenado para los grupos
nTotalSuelo$grupo <- factor(nTotalSuelo$grupo, levels = c("MK", "ML", "MP"))

# Crear la gráfica de cajas
ggplot(nTotalSuelo, aes(x = grupo, y = N_Total, group = grupo)) +
  geom_boxplot() +
  labs(x = "Grupo", y = "N_Total") +
  ggtitle("N Total de Suelo")

## Prueba 
anova_Nsuelo <- aov(nTotalSuelo$N_Total~nTotalSuelo$grupo)
summary(anova_Nsuelo)  
TukeyHSD(anova_Nsuelo)


### N TOTAL MANTILLO

# Crear un factor ordenado para los grupos
nTotalMantillo$grupo <- factor(nTotalMantillo$grupo, levels = c("MK", "ML", "MP"))

# Crear la gráfica de cajas
ggplot(nTotalMantillo, aes(x = grupo, y = N_Total, group = grupo)) +
  geom_boxplot() +
  labs(x = "Grupo", y = "N Total") +
  ggtitle("N Total de Mantillo")

## Prueba 
anova_Nmantillo <- aov(nTotalMantillo$N_Total~nTotalMantillo$grupo)
summary(anova_Nmantillo)  
TukeyHSD(anova_Nmantillo)


### CN TOTAL SUELO

# Crear un factor ordenado para los grupos
CN_TotalSuelo$grupo <- factor(CN_TotalSuelo$grupo, levels = c("MK", "ML", "MP"))

# Crear la gráfica de cajas
ggplot(CN_TotalSuelo, aes(x = grupo, y = CN, group = grupo)) +
  geom_boxplot() +
  labs(x = "Grupo", y = "C:N Total") +
  ggtitle("C:N Total de Suelo")

## Prueba
anova_CNsuelo <- aov(CN_TotalSuelo$CN~CN_TotalSuelo$grupo)
summary(anova_Nsuelo)  
TukeyHSD(anova_CNsuelo)

### N TOTAL MANTILLO

# Crear un factor ordenado para los grupos
CN_TotalMantillo$grupo <- factor(CN_TotalMantillo$grupo, levels = c("MK", "ML", "MP"))

# Crear la gráfica de cajas
ggplot(CN_TotalMantillo, aes(x = grupo, y = CN, group = grupo)) +
  geom_boxplot() +
  labs(x = "Grupo", y = "C:N Total") +
  ggtitle("C:N Total de Mantillo")

## Prueba
anova_CNmantillo <- aov(CN_TotalMantillo$CN~CN_TotalMantillo$grupo)
summary(anova_CNmantillo)  
TukeyHSD(anova_CNmantillo)