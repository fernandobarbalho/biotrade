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


