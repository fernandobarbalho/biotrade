load("unctad.RDS")

brazil_full <- readRDS("~/github/biotrade/brazil_full.rds")




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