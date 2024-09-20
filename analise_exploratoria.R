summary(unctad)

unique(unctad$product_label)


unctad %>%
  slice_sample(n=10) %>%
  mutate(grupo = product)


mutate(grupo = ifelse(str_detect(product_label, "[;]", negate = TRUE), 
                      product_label, 
                      str_extract(product_label, ".+(?=[;])"))) 


glimpse(unctad)


str_extract("Vegetable saps and extracts; of ephedr", ".+(?=[;])")


str_extract("Vegetable saps and extracts; of ephedr", ".+")

str_extract("Glands and other organs; heparin and its salts; other human or animal substances prepared for therapeutic or prophylactic uses, n.e.c. in heading", 
            ".+(?=[;])")


BioTrade_Categories <- read_csv("BioTrade_Categories.csv")


categorias<- BioTrade_Categories$Category

brazil_sample<-
  unctad %>%
  filter(product_label %in% categorias,
         economy_label == "Brazil") %>%
  slice_sample(n= 20)


brazil_sample_2<-
  unctad %>%
  filter(str_sub(product,1,2)== "B_" ,
         economy_label == "Brazil") %>%
  slice_sample(n= 20)


brazil_sample_3<-
  unctad %>%
  filter(str_length(product) %in% 3:4 ,
         economy_label == "Brazil") %>%
  slice_sample(n= 20)

brazil_sample_4<-
  unctad %>%
  filter(product == "B_F" ,
         economy_label == "Brazil") %>%
  slice_sample(n= 20)


brazil_sample_5<-
  unctad %>%
  filter(str_length(product) == 3 ,
         economy_label == "Brazil") %>%
  slice_sample(n= 20)


economias_distintas<-
  unctad %>%
  filter(str_length(product) == 3 ,
         economy_label == "Brazil")  %>%
  distinct(partner, partner_label)


brazil_full<-
  unctad %>%
  filter(economy_label == "Brazil")

brazil_full <- as_tibble(brazil_full)

brazil_full %>% saveRDS("brazil_full.rds")


brazil_full %>%
  filter(partner < 1400,
         partner >0,
         product == "B_TOT", 
         flow == 14) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands),
            .by =partner_label) %>%
  slice_min(order_by = total, n= 10)


brazil_full %>%
  filter(partner < 1400,
         partner >0,
         product == "B_TOT", 
         flow == 14) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands),
            .by =c(partner_label, partner)) %>%
  slice_max(order_by = total, n= 10)


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