library(tidyverse)

load("unctad.RDS")

brazil_full <- readRDS("~/github/biotrade/brazil_full.rds")


parceiros<-
  brazil_full %>%
  distinct(partner, partner_label)



#### Ranking dez maiores saldos
brazil_full %>%
  filter(partner < 1400,
         partner >0,
         product == "B_TOT", 
         flow == 14) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands),
            .by =c(partner_label, partner)) %>%
  slice_max(order_by = total, n= 10)



#### Análises a partir do Ranking dos maiores produtos exportados

total_produtos_exportados<-
  (brazil_full %>%
  filter(partner < 1400,
         partner >0,
         product == "B_TOT", 
         flow == 2) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands, na.rm = TRUE)))$total 

    

brazil_full %>%
  filter(partner < 1400,
         partner >0,
         flow == 2,
         str_length(product)==3) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands, na.rm = TRUE),
            .by =c(product_label)) %>%
  mutate(product_label = reorder(product_label, total)) %>%
  ggplot(aes(x= total, y= product_label)) +
  geom_col()


brazil_full %>%
  filter(partner < 1400,
         partner >0,
         flow == 2,
         str_length(product)==3) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands, na.rm = TRUE),
            percentual = (total/total_produtos_exportados)*100,
            .by =c(product_label)) %>%
  mutate(product_label = reorder(product_label, percentual)) %>%
  ggplot(aes(x= percentual, y= product_label)) +
  geom_col()

  
product<-
brazil_full %>%
  filter(str_length(product)==3) %>%
  distinct(product, product_label) 
  

brazil_full %>%
  filter(partner < 1400,
         partner >0,
         flow == 2,
         str_length(product)==4,
         str_starts(product, "B_B")) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands, na.rm = TRUE),
            .by =c(product_label)) %>%
  mutate(product_label = reorder(product_label, total)) %>%
  ggplot(aes(x= total, y= product_label)) +
  geom_col()


product_nivel_4<-
  brazil_full %>%
  filter(str_starts(product,"B_B")) %>%
  distinct(product, product_label) 


brazil_full %>%
  filter(partner < 1400,
         partner >0,
         flow == 2,
         product == "B_BA") %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands, na.rm = TRUE),
            .by =c(partner_label)) %>%
  slice_max(order_by = total, n=10 ) %>%
  mutate(partner_label = reorder(partner_label, total)) %>%
  ggplot(aes(x= total, y= partner_label)) +
  geom_col()


#### Análises a partir do Ranking dos maiores importadores





##### Fazer análises com G20

###Exercícios Artentina

brazil_full %>%
  filter(partner == 32,
         str_length(product) == 3,
         flow == 14) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands),
            .by = product_label) %>%
  slice_min(order_by = total, n= 10)



brazil_full %>%
  filter(partner == 32,
         str_length(product) == 4,
         str_starts(product, "B_B"),
         flow == 14) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands),
            .by = c(product_label, product)) %>%
  slice_min(order_by = total, n= 10)



brazil_full %>%
  filter(partner == 32,
         str_length(product) == 6,
         str_starts(product, "B_BJ"),
         flow == 14) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands),
            .by = c(product_label, product)) %>%
  slice_min(order_by = total, n= 10)


#Exercícios China

brazil_full %>%
  filter(partner == 156,
         str_length(product) == 3,
         flow == 14) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands),
            .by =c( product_label, product)) %>%
  slice_max(order_by = total, n= 10)



brazil_full %>%
  filter(partner == 32,
         str_length(product) == 4,
         str_starts(product, "B_D"),
         flow == 14) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands),
            .by = c(product_label, product)) %>%
  slice_max(order_by = total, n= 10)



brazil_full %>%
  filter(partner == 32,
         str_length(product) == 6,
         str_starts(product, "B_DR"),
         flow == 14) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands),
            .by = c(product_label, product)) %>%
  slice_max(order_by = total, n= 10)


ingredientes_naturais_china<-
  brazil_full %>%
  filter(str_starts(product,"B_D"),
         str_length(product)==6,
         flow == 2,
         partner_label== "China") %>%
  summarise( sum(us_dollars_at_current_prices_in_thousands),
             .by = c(product, product_label))

mundo_bio_trade %>%
  filter(flow == 2,
         economy!= 3412) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands, na.rm = TRUE)/10^6,
            .by = c(economy, economy_label)) %>%
  slice_max(order_by = total, n=10)


mundo_bio_trade %>%
  filter(flow == 2) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands, na.rm = TRUE)/10^6,
            .by = c(product, product_label)) %>%
  slice_max(order_by = total, n=10)


mundo_bio_trade %>%
  filter(flow == 1,
         economy!= 3412) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands, na.rm = TRUE)/10^6,
            .by = c(economy, economy_label)) %>%
  slice_max(order_by = total, n=10)

mundo_bio_trade %>%
  filter(flow == 2,
         economy != 3412,
         partner != 3412) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands, na.rm = TRUE)/10^6,
            .by = c(economy_label, product_label, partner_label)) %>%
  slice_max(order_by = total, n=10)
