library(tidyverse)
library(data.table)

d <- fread("http://www5.cao.go.jp/keizai-shimon/kaigi/special/future/keizai-jinkou_data/csv/file01.csv", skip = 7) 


d <-
  d %>% 
  separate(市区町村名, into = c("pref", "cities")) %>% 
  mutate(shikuchouson = str_sub(cities, start = -1L))

# 都道府県別の市区町村数
xtabs(~pref + shikuchouson, data = d)

p <-
  d %>% 
  mutate_at(vars(ends_with("年")),
         .fun = function(x) str_replace_all(x, pattern = ",", replacement = "") %>% as.numeric()) %>% 
  group_by(pref, shikuchouson) %>% 
  summarise(FY1970 = sum(`1970年`),
         FY1975 = sum(`1975年`),
         FY1980 = sum(`1980年`),
         FY1985 = sum(`1985年`),
         FY1990 = sum(`1990年`),
         FY1995 = sum(`1995年`),
         FY2000 = sum(`2000年`),
         FY2005 = sum(`2005年`),
         FY2010 = sum(`2010年`)) %>% 
  ungroup() %>% 
  gather(FY1970:FY2010, key = year, value = pop_sum) %>% 
  mutate(cities = paste(pref, shikuchouson, sep = "_")) %>%
  ggplot(aes(x = year %>% str_replace_all("FY", "") %>% as.numeric(), 
             y = pop_sum,
             group = cities)) +
  geom_line() +
  facet_wrap(~shikuchouson, scales = "free_y") +
  labs(y = "人口", x = "調査年")
p
ggsave(filename = "都道府県市区町村別の人口の推移.png", plot = p)
pp <- plotly::ggplotly(p)
pp
htmlwidgets::saveWidget(widget = pp, file = "test.html")

