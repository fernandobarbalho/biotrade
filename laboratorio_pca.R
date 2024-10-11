brazil_full <- readRDS("~/github/biotrade/brazil_full.rds")

fab<-
  brazil_full %>%
  distinct(partner, partner_label)

brazil_full %>%
  filter(str_length(product) == 3,
         between(partner,1,1399)) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands),
            .by = c(partner_label, product_label)) %>%
  pivot_wider(names_from = product_label, values_from = total) %>%
  slice_sample(n=50) %>%
  readr::write_csv("amostra_brazil_full.csv")

dados_pca<-
  brazil_full %>%
  filter(str_length(product) == 3,
         between(partner,1,1399)) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands),
            .by = c(partner_label, product_label)) %>%
  pivot_wider(names_from = product_label, values_from = total)

dados_pca<- janitor::clean_names(dados_pca)

dados_pca<-
dados_pca %>%
  replace(is.na(.), 0)

# Carregar bibliotecas necessárias
library(tidyverse)
library(factoextra)

# Carregar os dados
data <- dados_pca

# Verificar a estrutura dos dados
str(data)

# Remover a coluna de países e selecionar apenas colunas numéricas (produtos)
data_numeric <- data %>%
  select(-1) %>%
  mutate(across(everything(), as.numeric))

# Padronizar os dados (necessário para PCA)
data_scaled <- scale(data_numeric)

# Executar a PCA
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

# Resumir os resultados
summary(pca_result)
plot(pca_result)

# Visualizar variância explicada por cada componente
fviz_eig(pca_result)

# Visualizar os países no espaço dos componentes principais
fviz_pca_ind(pca_result, 
             geom.ind = c("point"),
             col.ind = "blue", 
             addEllipses = TRUE,
             ellipse.level = 0.95)


fviz_pca_ind(pca_result, 
             geom.ind = "point",
             col.ind = "blue", 
             addEllipses = TRUE,
             ellipse.level = 0.75)


paises_coordendas<-
  paises$data

# Visualizar a contribuição de cada produto para os componentes principais
fviz_pca_var(pca_result, 
             col.var = "contrib", 
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE)

# Visualizar biplot (países e produtos juntos)
fviz_pca_biplot(pca_result, 
                geom.ind = "point", 
                col.ind = "blue", 
                col.var = "red",
                repel = TRUE)

dados_dimensoes<- pca_result[["x"]]
dados_duas_dimensoes <- dados_dimensoes[,1:2]

dados_modelo<-as.tibble(dados_duas_dimensoes)  
dados_modelo$contribuicao<- paises_coordendas$contrib

dados_modelo %>%
  mutate(tipo = ifelse(PC2<0, "EUA", "China"))

library(caret)

control_dt <- trainControl(method="cv")
seed <- 1972
set.seed(seed)



dt_model <- train(  ~ x +  y ,
                  data= paises_coordendas, 
                  method="rpart", 
                  trControl=control_dt)

dt_model

boxplot(paises_coordendas$x)

boxplot(paises_coordendas$y)

summary(dados_modelo)


boxplot(paises_coordendas$contrib)
