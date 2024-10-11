brazil_full <- readRDS("~/github/biotrade/brazil_full.rds")

fab<-
  brazil_full %>%
  distinct(partner, partner_label)

brazil_full %>%
  filter(str_length(product) == 4,
         between(partner,1,1399)) %>%
  summarise(total = sum(us_dollars_at_current_prices_in_thousands),
            .by = c(partner_label, product_label)) %>%
  pivot_wider(names_from = product_label, values_from = total) %>%
  slice_sample()
