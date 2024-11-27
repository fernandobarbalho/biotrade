library(tidyverse)

#url_add<- "https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20102011"

#https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20122013

#https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20142015

#https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20162017

#https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20182019

#https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20202020

#https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20222022


url_add<-
  "https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20222022"




download.file(url_add,mode = "wb", destfile = "biotrade.7Z", extra = "-R", method = "wininet")



#Usado para fazer extração de arquivo 7z. Se for testar, descomentar
# library(archive)
# arquivo<- archive("biotrade.7z")
# 
# archive_extract(archive = arquivo)
# 
# system_com<- paste('7z e -ounctad',"biotrade.7z")
# 
# system(system_com)
# 
# fab<-
#   read.csv(url_add)
# 
# library(jsonlite)
# 
# 
# jsonlite::fromJSON(url_add)
# 
# unzip("biotrade.zip")



#Tabela muito grande. Opção por abrir usando o fread
unctad<-
  data.table::fread("biotrade/US_BiotradeMerch_20222022_20231120021515.csv")


unctad<- janitor::clean_names(unctad)

unctad<- as_tibble(unctad)

saveRDS(unctad, "unctad.RDS")




brazil_full<-
  unctad %>%
  filter(economy_label == "Brazil")

brazil_full <- as_tibble(brazil_full)

brazil_full %>% saveRDS("brazil_full.rds")



#######Investimento direto


#url_add<- "https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20102011"

url_add <- "https://unctadstat-api.unctad.org/bulkdownload/US.FdiFlowsStock/US_FdiFlowsStock"

#https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20122013

#https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20142015

#https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20162017

#https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20182019

#https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20202020


#url_add<-
#  "https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20222022"

#https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20222022


#tmp = tempfile(fileext = ".7Z")

#download.file(url_add,mode = "wb", destfile = "fdi.7Z", extra = "-R", method = "wininet")


ied_1990_2023 <- read_csv("US_FdiFlowsStock.csv/US_FdiFlowsStock.csv")


ied_1990_2023 <- janitor::clean_names(ied_1990_2023)

brazil_full %>%
  filter(partner>0 ,
         partner<1400) %>%
  distinct(partner, partner_label) %>%
  readr::write_csv("paises_base_dados.csv")

eu_countries_2021 <- read_csv("eu_countries_2021.csv")

eu_countries_2021 <-
eu_countries_2021 %>%
  bind_rows(tibble(partner = 528,  partner_label = "Netherlands (Kingdom of the)" ))

saveRDS(eu_countries_2021, "eu_countries_2021.rds")


paises_sel<- c(1:1399,3412)

mundo_bio_trade<-
  unctad %>%
  filter(str_length(product)==3,
         partner %in% paises_sel,
         economy %in% paises_sel)

mundo_bio_trade <- as_tibble(mundo_bio_trade)

saveRDS(mundo_bio_trade, "mundo_bio_trade.rds")  

economias_distintas<-
 unctad %>%
  distinct(economy, economy_label)



eua_can_mex<-
unctad %>%
  filter(product == "B_TOT",
         economy %in% c(842,124, 484 ),
         flow==2) %>%
  select(economy_label, partner_label, us_dollars_at_current_prices_in_thousands)

sum(eua_can_mex$us_dollars_at_current_prices_in_thousands, na.rm = TRUE)


north_america<-
unctad %>%
  filter(product == "B_TOT",
         economy %in% c(5210 ),
         flow==2) %>%
  select(economy_label, partner_label,us_dollars_at_current_prices_in_thousands)


sum(north_america$us_dollars_at_current_prices_in_thousands, na.rm = TRUE)

continentes_economia<- c( 5400, 5220, 5600, 5100,5210) #5702

continente_bio_trade<-
  unctad %>%
  filter(str_length(product)==3,
         partner %in% continentes_economia,
         economy %in% continentes_economia)

saveRDS(continente_bio_trade, "continente_bio_trade.rds")


fab<-
  unctad %>%
  filter(str_length(product)==3,
         #partner %in% continentes_economia,
         economy %in% continentes_economia)
