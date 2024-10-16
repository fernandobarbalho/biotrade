brazil_full <- readRDS("~/github/biotrade/brazil_full.rds")
top_3_produtos<- c("Food and beverage", "Wood and derived products", "Natural ingredients")

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
  filter(product_label %in% top_3_produtos,
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
             geom.ind = c("text","point"),
             col.ind = "blue", 
             addEllipses = TRUE,
             ellipse.level = 0.75)


paises<-
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

# Visualizar a contribuição de cada produto para os componentes principais
produtos<-
fviz_pca_var(pca_result, 
             col.var = "contrib", 
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE)


# Visualizar biplot (países e produtos juntos)
fviz_pca_biplot(pca_result, 
                geom.ind = c("point"), 
                col.ind = "blue", 
                col.var = "red",
                repel = TRUE)

dados_dimensoes<- pca_result[["x"]]
dados_duas_dimensoes <- dados_dimensoes[,1:2]

dados_modelo<-as.tibble(dados_duas_dimensoes)  
dados_modelo$contribuicao<- paises_coordendas$contrib

dados_modelo$partner_label<- dados_pca$partner_label

dados_modelo <- janitor::clean_names(dados_modelo)

dados_modelo_eua_china_iqr<-
dados_modelo %>%
  filter(contribuicao >= quantile(paises_coordendas$contrib)[4]+1.5*IQR(paises_coordendas$contrib)) %>%
  #filter(contribuicao >= quantile(paises_coordendas$contrib)[3]
  mutate(tipo_pais = ifelse(pc2>0, "EUA","China"))


total_produtos<-
brazil_full %>%
  filter(str_length(product) == 3,
         between(partner,1,1399)) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands, na.rm= TRUE),
            .by = c( product_label))

total_pais<-
  brazil_full %>%
  filter(str_length(product) == 3,
         between(partner,1,1399)) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands, na.rm= TRUE),
            .by = c( partner_label))


total_produtos_pais<-
brazil_full %>%
  filter(str_length(product) == 3,
         between(partner,1,1399)) %>%
  summarise(total_pais = sum(us_dollars_at_current_prices_in_thousands, na.rm = TRUE),
            .by = c(partner_label, product_label)) %>%
  inner_join(total_pais) %>%
  mutate(perc_pais = (total_pais/total)*100)


dados_analise<-
  dados_modelo_eua_china_iqr %>%
  inner_join(total_produtos_pais)

dados_analise %>%
  filter(!partner_label %in% c("United States of America", "China")) %>%
  ggplot(aes(x=tipo_pais, y=perc_pais)) +
  geom_boxplot() +
  facet_wrap(product_label~., scales = "free_y")

dados_analise %>%
  filter(!partner_label %in% c("United States of America", "China")) %>%
  ggplot(aes(x=tipo_pais, y=total_pais)) +
  geom_boxplot() +
  facet_wrap(product_label~., scales = "free_y")


dados_analise %>%
  #filter(!partner_label %in% c("United States of America", "China")) %>%
  filter(product_label == "Natural ingredients") %>%
  
  mutate(partner_label = reorder(partner_label,perc_pais)) %>%
  ggplot(aes(y=partner_label, x=perc_pais)) +
  geom_col(aes(fill = tipo_pais)) 




dados_analise %>%
  filter(!partner_label %in% c("United States of America", "China")) %>%
  filter(product_label == "Natural ingredients") %>%
  mutate(partner_label = reorder(partner_label,total_pais)) %>%
  ggplot(aes(y=partner_label, x=total_pais)) +
  geom_col(aes(fill = tipo_pais)) 


dados_analise %>%
  filter(!partner_label %in% c("United States of America", "China")) %>%
  filter(product_label == "Wood and derived products") %>%
  mutate(partner_label = reorder(partner_label,perc_pais)) %>%
  ggplot(aes(y=partner_label, x=perc_pais)) +
  geom_col(aes(fill = tipo_pais)) 

dados_analise %>%
  #filter(!partner_label %in% c("United States of America", "China")) %>%
  filter(product_label == "Food and beverage") %>%
  mutate(partner_label = reorder(partner_label,perc_pais)) %>%
  ggplot(aes(y=partner_label, x=perc_pais)) +
  geom_col(aes(fill = tipo_pais)) 


dados_analise %>%
  #filter(!partner_label %in% c("United States of America", "China")) %>%
  filter(product_label == "Food and beverage") %>%
  mutate(partner_label = reorder(partner_label,total_pais)) %>%
  ggplot(aes(y=partner_label, x=total_pais)) +
  geom_col(aes(fill = tipo_pais)) 


dados_analise %>%
  mutate(product_label = ifelse(product_label %in% top_3_produtos,product_label, "Outros produtos" ) ) %>%
  filter(!partner_label %in% c("United States of America", "China")) %>%
  filter(product_label == "Outros produtos") %>%
  mutate(partner_label = reorder(partner_label,perc_pais)) %>%
  ggplot(aes(y=partner_label, x=perc_pais)) +
  geom_col(aes(fill = tipo_pais)) 

dados_analise %>%
  mutate(product_label = ifelse(product_label %in% top_3_produtos,product_label, "Outros produtos" ) ) %>%
  filter(product_label == "Outros produtos") %>%
  mutate(partner_label = reorder(partner_label,perc_pais)) %>%
  ggplot(aes(y=partner_label, x=perc_pais)) +
  geom_col(aes(fill = tipo_pais)) 



dados_analise %>%
  filter(!partner_label %in% c("United States of America", "China")) %>%
  ggplot(aes(x=tipo_pais, y=perc_pais)) +
  geom_violin() +
  facet_wrap(product_label~., scales = "free_y")

dados_analise %>%
  filter(!partner_label %in% c("United States of America", "China")) %>%
  ggplot(aes(x=tipo_pais, y=perc_pais)) +
  geom_jitter(aes(color= tipo_pais)) +
  facet_wrap(product_label~., scales = "free_y")

top_3_produtos<- c("Food and beverage", "Wood and derived products", "Natural ingredients")

dados_analise %>%
  #filter(!partner_label %in% c("United States of America", "China")) %>%
  mutate(product_label = ifelse(product_label %in% top_3_produtos,product_label, "Outros produtos" ) ) %>%
  mutate(partner_label = fct_reorder(partner_label,total_pais,sum)) %>%
  ggplot(aes(y=partner_label, x=perc_pais)) +
  geom_col(aes(fill = product_label))  +
  facet_wrap(tipo_pais ~.)


dados_analise %>%
  filter(partner_label %in% c("United States of America", "China")) %>%
  ggplot(aes(x=tipo_pais, y=perc_pais, color = tipo_pais)) +
  geom_jitter() +
  facet_wrap(product_label~.)

dados_sel<- 
  dados_analise %>%
  filter(partner_label== "India")

dados_analise %>%
  ggplot(aes(x=pc1, y=pc2)) +
  geom_point(aes(color=tipo_pais)) +
  geom_point(data = dados_sel, color ="blue")

