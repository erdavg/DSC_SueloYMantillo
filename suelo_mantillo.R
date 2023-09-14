library(ggplot2)
library(dplyr)
library(ggprism) ## minor thicks
library(FactoMineR)
library(ggfortify)
library(plotly)
library(factoextra)
library(GGally)
library(skimr)
library(car)
library(tidyr)
library(CCA)
library(magrittr)
### ANOVA Y MANOVA
library(reshape)
library(multcomp)
library(WRS2)
library(WRS)
library(pastecs)
library(plotly)
install.packages(c("MASS", "akima", "robustbase"))
install.packages("WRS", repos = "http://R-Forge.R-project.org")
install.packages("WRS", repos="http://R-Forge.R-project.org")

## Directorio
setwd("D:\\Documentos\\Biologia UAM X\\SERVICIO\\SuelosBD\\cvs")



list.files()

mk1 <- read.csv("MK1.csv")
mk2 <- read.csv("MK2.csv")
mk3 <- read.csv("MK3.csv")
mk3 <- read.csv("MK3.csv")
ml1 <- read.csv("ML1.csv")
ml2 <- read.csv("ML2.csv")
ml3 <- read.csv("ML3.csv")
mp1 <- read.csv("MP1.csv")
mp2 <- read.csv("MP2.csv")
mp3 <- read.csv("MP3.csv")

sk1 <- read.csv("SK1.csv")
sk2 <- read.csv("SK2.csv")
sk3 <- read.csv("SK3.csv")
sk3 <- read.csv("SK3.csv")
sl1 <- read.csv("SL1.csv")
sl2 <- read.csv("SL2.csv")
sl3 <- read.csv("SL3.csv")
sp1 <- read.csv("SP1.csv")
sp2 <- read.csv("SP2.csv")
sp3 <- read.csv("SP3.csv")

############# CCA #################### CCA #################

## Df todas

## Mantillo
df_mq <- data.frame(
  Total_OM_loss = c(mk1$Total.OM.loss....[1], mk2$Total.OM.loss....[1], mk3$Total.OM.loss....[1], ml1$Total.OM.loss....[1],
                    ml2$Total.OM.loss....[1], ml3$Total.OM.loss....[1], mp1$Total.OM.loss....[1], mp2$Total.OM.loss....[1], mp3$Total.OM.loss....[1]),
  QReleased = c(mk1$Total.Q.released..J.g.1.[1],mk2$Total.Q.released..J.g.1.[1], mk3$Total.Q.released..J.g.1.[1],
                ml1$Total.Q.released..J.g.1.[1], ml2$Total.Q.released..J.g.1.[1], ml3$Total.Q.released..J.g.1.[1],
                mp1$Total.Q.released..J.g.1.[1], mp2$Total.Q.released..J.g.1.[1], mp3$Total.Q.released..J.g.1.[1]),
  Q = c(mk1$Q....J.mg.1.OM.[1], mk2$Q....J.mg.1.OM.[1], mk3$Q....J.mg.1.OM.[1], ml1$Q....J.mg.1.OM.[1],
        ml2$Q....J.mg.1.OM.[1], ml3$Q....J.mg.1.OM.[1], mp1$Q....J.mg.1.OM.[1], mp2$Q....J.mg.1.OM.[1], mp3$Q....J.mg.1.OM.[1]),
  Q50 = c(mk1$Q50..J.g.1.[1], mk2$Q50..J.g.1.[1], mk3$Q50..J.g.1.[1], ml1$Q50..J.g.1.[1],
          ml2$Q50..J.g.1.[1], ml3$Q50..J.g.1.[1], mp1$Q50..J.g.1.[1], mp2$Q50..J.g.1.[1], mp3$Q50..J.g.1.[1]),
  T50 = c(mk1$T50Q..oC.[1], mk2$T50Q..oC.[1], mk3$T50Q..oC.[1], ml1$T50Q..oC.[1],
          ml2$T50Q..oC.[1], ml3$T50Q..oC.[1], mp1$T50Q..oC.[1], mp2$T50Q..oC.[1], mp3$T50Q..oC.[1]),
  Q1 = c(mk1$Q1....[1], mk2$Q1....[1], mk3$Q1....[1], ml1$Q1....[1],
         ml2$Q1....[1], ml3$Q1....[1], mp1$Q1....[1], mp2$Q1....[1], mp3$Q1....[1]),
  Q2= c(mk1$Q2....[1], mk2$Q2....[1], mk3$Q2....[1], ml1$Q2....[1], 
        ml2$Q2....[1], ml3$Q2....[1], mp1$Q2....[1], mp2$Q2....[1], mp3$Q2....[1]),
  Q3= c(mk1$Q3....[1], mk2$Q3....[1], mk3$Q3....[1], ml1$Q3....[1], 
        ml2$Q3....[1], ml3$Q3....[1], mp1$Q3....[1], mp2$Q3....[1], mp3$Q3....[1]),
  T1= c(mk1$T1Q...oC.[1], mk2$T1Q...oC.[1], mk3$T1Q...oC.[1], ml1$T1Q...oC.[1], 
        ml2$T1Q...oC.[1], ml3$T1Q...oC.[1], mp1$T1Q...oC.[1], mp2$T1Q...oC.[1], mp3$T1Q...oC.[1]),
  T2= c(mk1$T2Q...oC.[1], mk2$T2Q...oC.[1], mk3$T2Q...oC.[1], ml1$T2Q...oC.[1], 
        ml2$T2Q...oC.[1], ml3$T2Q...oC.[1], mp1$T2Q...oC.[1], mp2$T2Q...oC.[1], mp3$T2Q...oC.[1]),
  T3= c(mk1$T3Q...oC.[1], mk2$T3Q...oC.[1], mk3$T3Q...oC.[1], ml1$T3Q...oC.[1], 
        ml2$T3Q...oC.[1], ml3$T3Q...oC.[1], mp1$T3Q...oC.[1], mp2$T3Q...oC.[1], mp3$T3Q...oC.[1]),
  Group = rep(c("MK", "ML", "MP"))
)


## Suelo
df_sq <- data.frame(
  Total_OM_loss = c(sk1$Total.OM.loss....[1], sk2$Total.OM.loss....[1], sk3$Total.OM.loss....[1], sl1$Total.OM.loss....[1],
                    sl2$Total.OM.loss....[1], sl3$Total.OM.loss....[1], sp1$Total.OM.loss....[1], sp2$Total.OM.loss....[1], sp3$Total.OM.loss....[1]),
  QReleased = c(sk1$Total.Q.released..J.g.1.[1],sk2$Total.Q.released..J.g.1.[1], sk3$Total.Q.released..J.g.1.[1],
                sl1$Total.Q.released..J.g.1.[1], sl2$Total.Q.released..J.g.1.[1], sl3$Total.Q.released..J.g.1.[1],
                sp1$Total.Q.released..J.g.1.[1], sp2$Total.Q.released..J.g.1.[1], sp3$Total.Q.released..J.g.1.[1]),
  Q = c(sk1$Q....J.mg.1.OM.[1], sk2$Q....J.mg.1.OM.[1], sk3$Q....J.mg.1.OM.[1], sl1$Q....J.mg.1.OM.[1],
        sl2$Q....J.mg.1.OM.[1], sl3$Q....J.mg.1.OM.[1], sp1$Q....J.mg.1.OM.[1], sp2$Q....J.mg.1.OM.[1], sp3$Q....J.mg.1.OM.[1]),
  Q50 = c(sk1$Q50..J.g.1.[1], sk2$Q50..J.g.1.[1], sk3$Q50..J.g.1.[1], sl1$Q50..J.g.1.[1],
          sl2$Q50..J.g.1.[1], sl3$Q50..J.g.1.[1], sp1$Q50..J.g.1.[1], sp2$Q50..J.g.1.[1], sp3$Q50..J.g.1.[1]),
  T50 = c(sk1$T50Q..oC.[1], sk2$T50Q..oC.[1], sk3$T50Q..oC.[1], sl1$T50Q..oC.[1],
          sl2$T50Q..oC.[1], sl3$T50Q..oC.[1], sp1$T50Q..oC.[1], sp2$T50Q..oC.[1], sp3$T50Q..oC.[1]),
  Q1 = c(sk1$Q1....[1], sk2$Q1....[1], sk3$Q1....[1], sl1$Q1....[1],
         sl2$Q1....[1], sl3$Q1....[1], sp1$Q1....[1], sp2$Q1....[1], sp3$Q1....[1]),
  Q2= c(sk1$Q2....[1], sk2$Q2....[1], sk3$Q2....[1], sl1$Q2....[1], 
        sl2$Q2....[1], sl3$Q2....[1], sp1$Q2....[1], sp2$Q2....[1], sp3$Q2....[1]),
  Q3= c(sk1$Q3....[1], sk2$Q3....[1], sk3$Q3....[1], sl1$Q3....[1], 
        sl2$Q3....[1], sl3$Q3....[1], sp1$Q3....[1], sp2$Q3....[1], sp3$Q3....[1]),
  T1= c(sk1$T1Q...oC.[1], sk2$T1Q...oC.[1], sk3$T1Q...oC.[1], sl1$T1Q...oC.[1], 
        sl2$T1Q...oC.[1], sl3$T1Q...oC.[1], sp1$T1Q...oC.[1], sp2$T1Q...oC.[1], sp3$T1Q...oC.[1]),
  T2= c(sk1$T2Q...oC.[1], sk2$T2Q...oC.[1], sk3$T2Q...oC.[1], sl1$T2Q...oC.[1], 
        sl2$T2Q...oC.[1], sl3$T2Q...oC.[1], sp1$T2Q...oC.[1], sp2$T2Q...oC.[1], sp3$T2Q...oC.[1]),
  T3= c(sk1$T3Q...oC.[1], sk2$T3Q...oC.[1], sk3$T3Q...oC.[1], sl1$T3Q...oC.[1], 
        sl2$T3Q...oC.[1], sl3$T3Q...oC.[1], sp1$T3Q...oC.[1], sp2$T3Q...oC.[1], sp3$T3Q...oC.[1]),
  Group = rep(c("SK", "SL", "SP"))
)

######### Suma de Q2 y Q3


## Nuevo df Mantillo
df_mq_2 <- df_mq %>% 
  mutate(Q2_Q3 = df_mq$Q2 + df_mq$Q3)

head(df_mq_2)
View(df_mq_2)

df_mq_3 <- df_mq_2 %>% 
  dplyr::select(Total_OM_loss, Q50, T50, Q1, Q2_Q3, Group)

head(df_mq_3)
View(df_mq_3)
## Nuevo df Suelo
df_sq_2 <- df_sq %>% 
  mutate(Q2_Q3 = df_sq$Q2 + df_sq$Q3)

head(df_sq_2)
View(df_sq_2)

df_sq_3 <- df_sq_2 %>% 
  dplyr::select(Total_OM_loss, Q50, T50, Q1, Q2_Q3, Group)

head(df_sq_3)



## Variables cpnvertidas a numericas de df_mq y df_sq
df_mq$Total_OM_loss <- as.numeric(df_mq$Total_OM_loss)
df_mq$QReleased <- as.numeric(df_mq$QReleased)
df_mq$Q <- as.numeric(df_mq$Q)
df_mq$Q50 <- as.numeric(df_mq$Q50)
df_mq$T50 <- as.numeric(df_mq$T50)
df_mq$Q1 <- as.numeric(df_mq$Q1)
df_mq$Q2 <- as.numeric(df_mq$Q2)
df_mq$Q3 <- as.numeric(df_mq$Q3)
df_mq$T1 <- as.numeric(df_mq$T1)
df_mq$T2 <- as.numeric(df_mq$T2)
df_mq$T3 <- as.numeric(df_mq$T3)


df_sq$Total_OM_loss <- as.numeric(df_sq$Total_OM_loss)
df_sq$QReleased <- as.numeric(df_sq$QReleased)
df_sq$Q <- as.numeric(df_sq$Q)
df_sq$Q50 <- as.numeric(df_sq$Q50)
df_sq$T50 <- as.numeric(df_sq$T50)
df_sq$Q1 <- as.numeric(df_sq$Q1)
df_sq$Q2 <- as.numeric(df_sq$Q2)
df_sq$Q3 <- as.numeric(df_sq$Q3)
df_sq$T1 <- as.numeric(df_sq$T1)
df_sq$T2 <- as.numeric(df_sq$T2)
df_sq$T3 <- as.numeric(df_sq$T3)

str(df_mq)
View(df_mq)

colnames(df_mq)
## Escalado
df_mq_scale <- df_mq %>% dplyr::select(Total_OM_loss, QReleased, Q, 
                                Q50, T50, Q1, Q2, Q3, 
                                T1, T2, T3) %>% 
  scale()

View(df_mq_scale)

############################ PCA ################## PCA #####################

## DF Mantillo Q

##Q1
# Combine the values from different data frames
group_mkq1 <- c(mk1$Q1....[1], mk2$Q1....[1], mk3$Q1....[1])
group_mlq1 <- c(ml1$Q1....[1], ml2$Q1....[1], ml3$Q1....[1])
group_mpq1 <- c(mp1$Q1....[1], mp2$Q1....[1], mp3$Q1....[1])



# Create a data frame
data_mq1 <- data.frame(
  Value = c(group_mkq1, group_mlq1, group_mpq1),
  Group = rep(c("MK", "ML", "MP"), each = length(group_mkq1))
)

head(data_mq1)

##Q2
# Combine the values from different data frames
group_mkq2 <- c(mk1$Q2....[1], mk2$Q2....[1], mk3$Q2....[1])
group_mlq2 <- c(ml1$Q2....[1], ml2$Q2....[1], ml3$Q2....[1])
group_mpq2 <- c(mp1$Q2....[1], mp2$Q2....[1], mp3$Q2....[1])

# Create a data frame
data_mq2 <- data.frame(
  Value = c(group_mkq2, group_mlq2, group_mpq2),
  Group = rep(c("MK", "ML", "MP"), each = length(group_mkq2))
)

##Q3
# Combine the values from different data frames
group_mkq3 <- c(mk1$Q3....[1], mk2$Q3....[1], mk3$Q3....[1])
group_mlq3 <- c(ml1$Q3....[1], ml2$Q3....[1], ml3$Q3....[1])
group_mpq3 <- c(mp1$Q3....[1], mp2$Q3....[1], mp3$Q3....[1])

# Create a data frame
data_mq3 <- data.frame(
  Value = c(group_mkq3, group_mlq3, group_mpq3),
  Group = rep(c("MK", "ML", "MP"), each = length(group_mkq3))
)

## DF Suelo Q

##Q1
# Combine the values from different data frames
group_skq1 <- c(sk1$Q1....[1], sk2$Q1....[1], sk3$Q1....[1])
group_slq1 <- c(sl1$Q1....[1], sl2$Q1....[1], sl3$Q1....[1])
group_spq1 <- c(sp1$Q1....[1], sp2$Q1....[1], sp3$Q1....[1])

# Create a data frame
data_sq1 <- data.frame(
  Value = c(group_skq1, group_slq1, group_spq1),
  Group = rep(c("MK", "ML", "MP"), each = length(group_skq1))
)


## Q2
# Combine the values from different data frames
group_skq2 <- c(sk1$Q2....[1], sk2$Q2....[1], sk3$Q2....[1])
group_slq2 <- c(sl1$Q2....[1], sl2$Q2....[1], sl3$Q2....[1])
group_spq2 <- c(sp1$Q2....[1], sp2$Q2....[1], sp3$Q2....[1])

# Create a data frame
data_sq2 <- data.frame(
  Value = c(group_skq2, group_slq2, group_spq2),
  Group = rep(c("MK", "ML", "MP"), each = length(group_skq2))
)

##Q3
# Combine the values from different data frames
group_skq3 <- c(sk1$Q3....[1], sk2$Q3....[1], sk3$Q3....[1])
group_slq3 <- c(sl1$Q3....[1], sl2$Q3....[1], sl3$Q3....[1])
group_spq3 <- c(sp1$Q3....[1], sp2$Q3....[1], sp3$Q3....[1])


# Create a data frame
data_sq3 <- data.frame(
  Value = c(group_skq3, group_slq3, group_spq3),
  Group = rep(c("MK", "ML", "MP"), each = length(group_skq3))
)

head(data_sq3)

### Data type

class(sk1$Q3....)



##### MANTILLO BARRAS

## MK

q_Total_mkq <- data.frame(
  Grupo = rep(c("MK1", "MK2", "MK3"), each = 1),
  Q1 = c(mk1$Q1....[1], mk2$Q1....[1], mk3$Q1....[1]),
  Q2= c(mk1$Q2....[1], mk2$Q2....[1], mk3$Q2....[1]),
  Q3= c(mk1$Q3....[1], mk2$Q3....[1], mk3$Q3....[1])
)

q_Total_mkq_long <- q_Total_mkq %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Variable", values_to = "Valor")

ggplot(q_Total_mkq_long, aes(x = Grupo, y = Valor, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_grey(start = 0.5, end = 1) +
  labs(title = "Total de Energía de Muestras de MK",
       x = "Grupo",
       y = "Valor",
       fill = "Variable") +
  theme_classic()


## ML

q_Total_mlq <- data.frame(
  Grupo = rep(c("ML1", "ML2", "ML3"), each = 1),
  Q1 = c(ml1$Q1....[1], ml2$Q1....[1], ml3$Q1....[1]),
  Q2= c(ml1$Q2....[1], ml2$Q2....[1], ml3$Q2....[1]),
  Q3= c(ml1$Q3....[1], ml2$Q3....[1], ml3$Q3....[1])
)

q_Total_mlq_long <- q_Total_mlq %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Variable", values_to = "Valor")

ggplot(q_Total_mlq_long, aes(x = Grupo, y = Valor, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_grey(start = 0.5, end = 1) +
  labs(title = "Total de Energía de Muestras de ML",
       x = "Grupo",
       y = "Valor",
       fill = "Variable")+
  theme_classic()


## MP

q_Total_mpq <- data.frame(
  Grupo = rep(c("MP1", "MP2", "MP3"), each = 1),
  Q1 = c(mp1$Q1....[1], mp2$Q1....[1], mp3$Q1....[1]),
  Q2= c(mp1$Q2....[1], mp2$Q2....[1], mp3$Q2....[1]),
  Q3= c(mp1$Q3....[1], mp2$Q3....[1], mp3$Q3....[1])
)

q_Total_mpq_long <- q_Total_mpq %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Variable", values_to = "Valor")

ggplot(q_Total_mpq_long, aes(x = Grupo, y = Valor, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_grey(start = 0.5, end = 1) +
  labs(title = "Total de Energía de Muestras de MP",
       x = "Grupo",
       y = "Valor",
       fill = "Variable")+
  theme_classic()





###### SUELOS BARRAS

###SK
q_Total_skq <- data.frame(
  Grupo = rep(c("SK1", "SK2", "SK3"), each = 1),
  Q1 = c(sk1$Q1....[1], sk2$Q1....[1], sk3$Q1....[1]),
  Q2= c(sk1$Q2....[1], sk2$Q2....[1], sk3$Q2....[1]),
  Q3= c(sk1$Q3....[1], sk2$Q3....[1], sk3$Q3....[1])
)

q_Total_skq_long <- q_Total_skq %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Variable", values_to = "Valor")

ggplot(q_Total_skq_long, aes(x = Grupo, y = Valor, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_grey(start = 0.5, end = 1) +
  labs(title = "Total de Energía de Muestras de SK",
       x = "Grupo",
       y = "Valor",
       fill = "Variable")+
  theme_classic()

## SL

q_Total_slq <- data.frame(
  Grupo = rep(c("SL1", "SL2", "SL3"), each = 1),
  Q1 = c(sl1$Q1....[1], sl2$Q1....[1], sl3$Q1....[1]),
  Q2= c(sl1$Q2....[1], sl2$Q2....[1], sl3$Q2....[1]),
  Q3= c(sl1$Q3....[1], sl2$Q3....[1], sl3$Q3....[1])
)

q_Total_slq_long <- q_Total_slq %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Variable", values_to = "Valor")

ggplot(q_Total_slq_long, aes(x = Grupo, y = Valor, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_grey(start = 0.5, end = 1) +
  labs(title = "Total de Energía de Muestras de SL",
       x = "Grupo",
       y = "Valor",
       fill = "Variable")+
  theme_classic()

## SP

q_Total_spq <- data.frame(
  Grupo = rep(c("SP1", "SP2", "SP3"), each = 1),
  Q1 = c(sp1$Q1....[1], sp2$Q1....[1], sp3$Q1....[1]),
  Q2= c(sp1$Q2....[1], sp2$Q2....[1], sp3$Q2....[1]),
  Q3= c(sp1$Q3....[1], sp2$Q3....[1], sp3$Q3....[1])
)


q_Total_spq_long <- q_Total_spq %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Variable", values_to = "Valor")

ggplot(q_Total_spq_long, aes(x = Grupo, y = Valor, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_grey(start = 0.5, end = 1) +
  labs(title = "Total de Energía de Muestras de SP",
       x = "Grupo",
       y = "Valor",
       fill = "Variable")+
  theme_classic()



## Covarianza de la matriz
cov(df_mq_3[,1:5])

## Correlacion de la matriz
cor(df_mq_3[,1:5])

### Grafica
ggpairs(df_mq_3, columns = 2:5,
        aes(color = Group),
        upper = list(continuous = wrap("cor", size = 5))) +
  theme(
    text = element_text(size = 11) 
  )

### MANTILLO PCA

pc_mq <- df_mq_3 %>% dplyr::select(-Group) %>% as.matrix() %>% prcomp()
summary(pc_mq)
head(pc_mq)
## Scree plot para seleccion de dimensiones
plot(pc_mq, type = "line")

str(pc_mq)

## Grafico de dos dimensiones con ggplot
dfmq_projected <- as_tibble(pc_mq$x) %>% tibble::add_column(Group = df_mq_3$Group)
ggplot(dfmq_projected, aes(x = PC1, y = PC2, color = Group)) + 
  geom_point()


## Removemos la variable "Species"
dfmq_pca <- PCA(df_mq_3[, -6], graph=F)
###PRUEBA

dfmq_pca_active <- df_mq_3[, -6]
fviz_pca_ind(dfmq_pca, geom.ind = "point", col.ind = df_mq_3$Group,
             palette = c("green", "purple", "blue"),
             pointsize.ind = 6)  # Ajusta el valor para cambiar el tamaño




             
## Grafica de factores individuales
fviz_pca_ind(dfmq_pca, label="none")

## Agregamos colores para ver los grupos
fviz_pca_ind(dfmq_pca,  label="none", habillage = as.factor(df_mq_3$Group))


class(df_mq$Group)

## Agregamos elipses a los puntos de concentracion
fviz_pca_ind(dfmq_pca,  label="none", habillage = as.factor(df_mq_3$Group))

## Ahora vamos a :
## - hacer un biplot de individuos y variables
## - cambiar el color de los individuos por grupos
## - cambiar la transparencia de los colores variables por sus 
##   valores de contribución
## - mostrar solo las etiquetas de las variables

fviz_pca_biplot(dfmq_pca, 
                habillage = as.factor(df_mq$Group), addEllipses = TRUE,
                col.var = "red", alpha.var ="cos2",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_classic()

##### MANOVA DE PCA
head(dfmq_pca)

## Agrego componentes principales al df_mq_3

componentes_principales_mq <- pc_mq$x
# Crear un nuevo data frame para los componentes principales
pc_mq_2 <- data.frame(
  Group = df_mq_3$Group,
  PC1 = componentes_principales_mq[, 1],
  PC2 = componentes_principales_mq[, 2]
  # Agrega más componentes principales según sea necesario
) 

print(pc_mq_2)

attach(pc_mq_2)

names(pc_mq_2)

## Grafica puntos dispersion
ggplot(pc_mq_2, aes(PC1, PC2, color = Group))+
  geom_point()

## Grafica de cajas
Boxplot(PC1 ~ Group, data = pc_mq_2)

Boxplot(PC2 ~ Group, data = pc_mq_2)

ggplot(pc_mq_2, aes(PC1, PC2))+
  geom_point()+labs(x= "PC1", y= "PC2")+
  geom_smooth(method = "lm", color="red")

# Realizar el ANOVA de dos vías considerando PC1 y PC2

outcome <- cbind(pc_mq_2$PC1, pc_mq_2$PC2)

modelo1 <- manova(outcome~Group, data=pc_mq_2)
## SI hay diferencias entre los grupos
summary(modelo1, intercept = TRUE) ## pillai

summary(modelo1, intercept = TRUE, test = "Wilks")

## Anova univariado
summary.aov(modelo1)


## Suelo PCA
ggpairs(df_sq_3, columns = 1:5)

ggpairs(df_sq_3, columns = 1:5,
        ggplot2::aes(color=Group))

ggcorr(df_sq_3,
       method = c("everything", "pearson"),
       label=T,
       label_alpha = T)

## Covarianza de la matriz
cov(df_sq_3[,1:5])

## Correlacion de la matriz
cor(df_sq_3[,1:5])


###PCA

pc_sq <- df_sq_3 %>% dplyr::select(-Group) %>% as.matrix() %>% prcomp()
summary(pc_sq)

## Scree plot para seleccion de dimensiones
plot(pc_sq, type = "line")

str(pc_sq)

## Grafico de dos dimensiones con ggplot
dfsq_projected <- as_tibble(pc_sq$x) %>% tibble::add_column(Group = df_sq_3$Group)
ggplot(dfsq_projected, aes(x = PC1, y = PC2, color = Group)) + 
  geom_point()

## Removemos la variable "Species"
dfsq_pca <- PCA(df_sq_3[, -6], graph=F)

## Grafica de factores individuales
fviz_pca_ind(dfsq_pca, label="none")

## Agregamos colores para ver los grupos
fviz_pca_ind(dfsq_pca,  label="none", habillage = as.factor(df_sq_3$Group))

class(df_sq_3$Group)

## Agregamos elipses a los puntos de concentracion
fviz_pca_ind(dfsq_pca,  label="none", habillage = as.factor(df_sq_3$Group))

## Ahora vamos a :
## - hacer un biplot de individuos y variables
## - cambiar el color de los individuos por grupos
## - cambiar la transparencia de los colores variables por sus 
##   valores de contribución
## - mostrar solo las etiquetas de las variables

fviz_pca_biplot(dfsq_pca, 
                habillage = as.factor(df_sq_3$Group), addEllipses = TRUE,
                col.var = "red", alpha.var ="cos2",
                label = "var",
                pointsize.ind = 3) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic()

##### MANOVA DE PCA
head(dfsq_pca)

## Agrego componentes principales al df_mq_3

componentes_principales_sq <- pc_sq$x
# Crear un nuevo data frame para los componentes principales
pc_sq_2 <- data.frame(
  Group = df_sq_3$Group,
  PC1 = componentes_principales_sq[, 1],
  PC2 = componentes_principales_sq[, 2]
  # Agrega más componentes principales según sea necesario
) 

print(pc_sq_2)

attach(pc_sq_2)

names(pc_sq_2)

## Grafica puntos dispersion
ggplot(pc_sq_2, aes(PC1, PC2, color = Group))+
  geom_point()

## Grafica de cajas
Boxplot(PC1 ~ Group, data = pc_sq_2)

Boxplot(PC2 ~ Group, data = pc_sq_2)

ggplot(pc_mq_2, aes(PC1, PC2))+
  geom_point()+labs(x= "PC1", y= "PC2")+
  geom_smooth(method = "lm", color="red")

# Realizar el ANOVA de dos vías considerando PC1 y PC2

outcome <- cbind(pc_sq_2$PC1, pc_sq_2$PC2)

modelo2 <- manova(outcome~Group, data=pc_sq_2)
## SI hay diferencias entre los grupos
summary(modelo2, intercept = TRUE) ## pillai

summary(modelo2, intercept = TRUE, test = "Wilks")



#################### DSC ######################
levels(mk1)
## Ts, Q.corrected

## Verificar clase de variables
class(mk1$Ts)
class(mk1$Q.corrected)

##MK1
## Cambiar variables a numericas
mk1 <- mk1 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
mk1 <- mk1 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(mk1, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("MK1 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "MK1", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "MK1"))+
  theme(legend.position = c(0.9, 0.5))

## Mk2

## Cambiar variables a numericas
mk2 <- mk2 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
mk2 <- mk2 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(mk2, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("MK2 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "MK2", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "MK2"))+
  theme(legend.position = c(0.9, 0.5))
## Mk3
mk3 <- mk3 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
mk3 <- mk3 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(mk3, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("MK3 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "MK3", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "MK3"))+
  theme(legend.position = c(0.9, 0.5))
## ML1
ml1 <- ml1 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
ml1 <- ml1 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(ml1, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("ML1 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "ML1", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "ML1"))+
  theme(legend.position = c(0.9, 0.5))
## ML2
ml2 <- ml2 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
ml2 <- ml2 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(ml2, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("ML2 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "ML2", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "ML2"))+
  theme(legend.position = c(0.9, 0.5))
## ML3

ml3 <- ml3 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
ml3 <- ml3 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(ml3, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("ML3 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "ML3", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "ML3"))+
  theme(legend.position = c(0.9, 0.5))

## MP1
mp1 <- mp1 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
mp1 <- mp1 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(mp1, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("MP1 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "MP1", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "MP1"))+
  theme(legend.position = c(0.9, 0.5))
## MP2
mp2 <- mp2 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
mp2 <- mp2 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(mp2, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("MP2 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "MP2", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "MP2"))+
  theme(legend.position = c(0.9, 0.5))
## MP3
mp3 <- mp3 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
mp3 <- mp3 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(mp3, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("MP3 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "MP3", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "MP3"))+
  theme(legend.position = c(0.9, 0.5))

############## SUELO ########################## SUELO #########

## SK1
sk1 <- sk1 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
sk1 <- sk1 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(sk1, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("SK1 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "SK1", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "SK1"))+
  theme(legend.position = c(0.9, 0.5))
## SK2
sk2 <- sk2 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
sk2 <- sk2 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(sk2, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("SK2 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "SK2", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "SK2"))+
  theme(legend.position = c(0.9, 0.5))
## SK3
sk3 <- sk3 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
sk3 <- sk3 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(sk3, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("SK3 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "SK3", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "SK3"))+
  theme(legend.position = c(0.9, 0.5))

## SL1
sl1 <- sl1 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
sl1 <- sl1 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(sl1, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("SL1 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "SL1", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "SL1"))+
  theme(legend.position = c(0.9, 0.5))
## SL2
sl2 <- sl2 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))
## Cambiar nombre a Q.corrected
sl2 <- sl2 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(sl2, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("SL2 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "SL2", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "SL2"))+
  theme(legend.position = c(0.9, 0.5))
## SL3
sl3 <- sl3 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
sl3 <- sl3 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(sl3, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("SL3 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "SL3", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "SL3"))+
  theme(legend.position = c(0.9, 0.5))
## SP1
sp1 <- sp1 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
sp1 <- sp1 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Cambiar nombre a Q.corrected
sp1 <- sp1 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(sp1, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("SP1 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "SP1", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "SP1"))+
  theme(legend.position = c(0.9, 0.5))
## SP2
sp2 <- sp2 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
sp2 <- sp2 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(sp2, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("SP2 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "SP2", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "SP2"))+
  theme(legend.position = c(0.9, 0.5))

## SP3
sp3 <- sp3 %>% 
  mutate(Ts = as.numeric(Ts),
         Q.corrected = as.numeric(Q.corrected))

## Cambiar nombre a Q.corrected
sp3 <- sp3 %>% 
  filter(Ts >= 150) %>% 
  mutate(Q_released = Q.corrected) 

## Grafica de dsc
ggplot(sp3, aes(x = Ts, y = Q_released, color = "Q_released")) +
  geom_line(linewidth = 0.8, show.legend = TRUE) +
  scale_x_continuous(limits = c(150, 600),
                     breaks = seq(150, 600, by = 50),
                     guide = guide_prism_minor()) +
  ylab(expression("Heat Flow (W/g"^-1*" sample)")) +
  xlab("Sample Temperature (ºC)") +
  ggtitle("SP3 Heat Flow") +
  theme_classic() +
  geom_vline(xintercept = 150, color = "black", linetype = "dashed") +
  geom_text(aes(x = 160, y = -Inf, label = "Labile"), size = 3.5, vjust = 1, hjust = -9.3, color = "black", angle = 90) +
  geom_vline(xintercept = 375, color = "black", linetype = "dashed") +
  geom_text(aes(x = 385, y = -Inf, label = "Recalcitrant"), size = 3.5, vjust = 1, hjust = -4.1, color = "black", angle = 90) +
  geom_vline(xintercept = 475, color = "black", linetype = "dashed") +
  geom_text(aes(x = 465, y = -Inf, label = "Extra-Recalcitrant"), size = 3.5, vjust = 4, hjust = -2.4, color = "black", angle = 90) +
  scale_color_manual(name = "SP3", values = c("Q_released" = "black")) +
  guides(color = guide_legend(title = "SP3"))+
  theme(legend.position = c(0.9, 0.5))

############### ANOVA MANTILLO ########################### ANOVA ###############

boxplot(c(mk1$Q1...., mk2$Q1...., mk3$Q1....), 
        c(ml1$Q1...., ml2$Q1...., ml3$Q1....),
        c(mp1$Q1...., mp2$Q1...., mp3$Q1....))

boxplot(c(mk1$Q2...., mk2$Q2...., mk3$Q2....), 
        c(ml1$Q2...., ml2$Q2...., ml3$Q2....),
        c(mp1$Q2...., mp2$Q2...., mp3$Q2....))

boxplot(c(mk1$Q3...., mk2$Q3...., mk3$Q3....), 
        c(ml1$Q3...., ml2$Q3...., ml3$Q3....),
        c(mp1$Q3...., mp2$Q3...., mp3$Q3....))

##Q1
# Combine the values from different data frames
group_mkq1 <- c(mk1$Q1....[1], mk2$Q1....[1], mk3$Q1....[1])
group_mlq1 <- c(ml1$Q1....[1], ml2$Q1....[1], ml3$Q1....[1])
group_mpq1 <- c(mp1$Q1....[1], mp2$Q1....[1], mp3$Q1....[1])

# Create a data frame
data_mq1 <- data.frame(
  Value = c(group_mkq1, group_mlq1, group_mpq1),
  Group = rep(c("MK", "ML", "MP"), each = length(group_mkq1))
)

data_mq1$Group <- factor(data_mq1$Group)

levels(data_mq1$Group)

ggplot(data_mq1, aes(x = Group, y = Value, fill= Group)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5, end = 1)+
  theme_classic() +
  labs(title = "Boxplot for Litter Q1", x = "Group", y = "Value")



## Prueba 
anova_mq1 <- aov(data_mq1$Value~data_mq1$Group)
summary(anova_mq1)  
TukeyHSD(anova_mq1)
##Q2
# Combine the values from different data frames
group_mkq2 <- c(mk1$Q2....[1], mk2$Q2....[1], mk3$Q2....[1])
group_mlq2 <- c(ml1$Q2....[1], ml2$Q2....[1], ml3$Q2....[1])
group_mpq2 <- c(mp1$Q2....[1], mp2$Q2....[1], mp3$Q2....[1])

# Create a data frame
data_mq2 <- data.frame(
  Value = c(group_mkq2, group_mlq2, group_mpq2),
  Group = rep(c("MK", "ML", "MP"), each = length(group_mkq2))
)

ggplot(data_mq2, aes(x = Group, y = Value, fill= Group)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5, end = 1)+
  theme_classic() +
  labs(title = "Boxplot for Litter Q2", x = "Group", y = "Value")
## Prueba 
anova_mq2 <- aov(data_mq2$Value~data_mq2$Group)
summary(anova_mq2)  
TukeyHSD(anova_mq2)

##Q3
# Combine the values from different data frames
group_mkq3 <- c(mk1$Q3....[1], mk2$Q3....[1], mk3$Q3....[1])
group_mlq3 <- c(ml1$Q3....[1], ml2$Q3....[1], ml3$Q3....[1])
group_mpq3 <- c(mp1$Q3....[1], mp2$Q3....[1], mp3$Q3....[1])

# Create a data frame
data_mq3 <- data.frame(
  Value = c(group_mkq3, group_mlq3, group_mpq3),
  Group = rep(c("MK", "ML", "MP"), each = length(group_mkq3))
)

ggplot(data_mq3, aes(x = Group, y = Value, fill= Group)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5, end = 1)+
  theme_classic() +
  labs(title = "Boxplot for Litter Q3", x = "Group", y = "Value")

## Prueba 
anova_mq3 <- aov(data_mq3$Value~data_mq3$Group)
summary(anova_mq3)  
TukeyHSD(anova_mq3)



################# ANOVA SUELOS###############



##Q1
# Combine the values from different data frames
group_skq1 <- c(sk1$Q1....[1], sk2$Q1....[1], sk3$Q1....[1])
group_slq1 <- c(sl1$Q1....[1], sl2$Q1....[1], sl3$Q1....[1])
group_spq1 <- c(sp1$Q1....[1], sp2$Q1....[1], sp3$Q1....[1])

# Create a data frame
data_sq1 <- data.frame(
  Value = c(group_skq1, group_slq1, group_spq1),
  Group = rep(c("SK", "SL", "SP"), each = length(group_skq1))
)

ggplot(data_sq1, aes(x = Group, y = Value, fill= Group)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5, end = 1)+
  theme_classic() +
  labs(title = "Boxplot for Soil Q1", x = "Group", y = "Value")

## Prueba 
anova_sq1 <- aov(data_sq1$Value~data_sq1$Group)
summary(anova_sq1)  
TukeyHSD(anova_sq1)

##Q2
# Combine the values from different data frames
group_skq2 <- c(sk1$Q2....[1], sk2$Q2....[1], sk3$Q2....[1])
group_slq2 <- c(sl1$Q2....[1], sl2$Q2....[1], sl3$Q2....[1])
group_spq2 <- c(sp1$Q2....[1], sp2$Q2....[1], sp3$Q2....[1])

# Create a data frame
data_sq2 <- data.frame(
  Value = c(group_skq2, group_slq2, group_spq2),
  Group = rep(c("SK", "SL", "SP"), each = length(group_skq2))
)

ggplot(data_sq2, aes(x = Group, y = Value, fill= Group)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5, end = 1)+
  theme_classic() +
  labs(title = "Boxplot for Soil Q2", x = "Group", y = "Value")

## Prueba 
anova_sq2 <- aov(data_sq2$Value~data_sq2$Group)
summary(anova_sq2)  
TukeyHSD(anova_sq2)

##Q3
# Combine the values from different data frames
group_skq3 <- c(sk1$Q3....[1], sk2$Q3....[1], sk3$Q3....[1])
group_slq3 <- c(sl1$Q3....[1], sl2$Q3....[1], sl3$Q3....[1])
group_spq3 <- c(sp1$Q3....[1], sp2$Q3....[1], sp3$Q3....[1])


# Create a data frame
data_sq3 <- data.frame(
  Value = c(group_skq3, group_slq3, group_spq3),
  Group = rep(c("SK", "SL", "SP"), each = length(group_skq3))
)

ggplot(data_sq3, aes(x = Group, y = Value, fill= Group)) +
  geom_boxplot() +
  scale_fill_grey(start = 0.5, end = 1)+
  theme_classic() +
  labs(title = "Boxplot for Soil Q3", x = "Group", y = "Value")

## Prueba 
anova_sq3 <- aov(data_sq3$Value~data_sq3$Group)
summary(anova_sq3)  
TukeyHSD(anova_sq3)


