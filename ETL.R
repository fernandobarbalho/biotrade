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
