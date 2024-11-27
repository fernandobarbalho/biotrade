library(tidyverse)

sort(unique(unctad$partner_label))

mercado_carne_mercosul_fanca<-
  unctad %>%
  filter(economy_label %in% c("Brazil","Argentina","Uruguay", "Paraguay", "World"),
         product == "B_BA",
         partner_label == "France")

mercado_carne_mercosul_fanca %>%
  filter(flow == 2 ) %>%
  summarise(sum(us_dollars_at_current_prices_in_thousands))


#Os dados abaixo vieram do portal de dados da COMEX-stat

exp_2023 <- read_delim("EXP_2023.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE)

exp_2023 <- janitor::clean_names(exp_2023)

ncm <- read_delim("NCM.csv", delim = ";", 
                  escape_double = FALSE, locale = locale(encoding = "Latin1"), 
                  trim_ws = TRUE)

pais <- read_delim("PAIS.csv", delim = ";", 
                   escape_double = FALSE, locale = locale(encoding = "Latin1"), 
                   trim_ws = TRUE)

ncm_sh <- read_delim("NCM_SH.csv", delim = ";", 
                     escape_double = FALSE, locale = locale(encoding = "Latin1"), 
                     trim_ws = TRUE)

carne_franca_comex<-
exp_2023 %>%
  filter(co_pais == "275",
         co_ncm %in% c("02013000", "02023000"))


sum(carne_franca_comex$vl_fob)

sum(carne_franca_comex$kg_liquido)

(sum(carne_franca_comex$vl_fob)/sum(carne_franca_comex$kg_liquido))*1.5e6


