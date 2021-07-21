library(tidyverse)
library(ggplot2)
library(hrbrthemes)


urb <- read_csv("../hu4019_dev.csv")

urb$urb_yr2 <- factor(urb$urb_yr2,levels = c("1940","1950","1960", "1970", "1980", "1990", "2000","2010","2019", "2035"))

urb %>%
  count(urb_yr2)


ggplot(urb, mapping = aes(x = urb_yr2, fill = urb_yr2)) +
  geom_bar() +
  labs(x ='', y = 'Number', title = "Total Census Tracts by Urbanized Boundary") +
  theme_ipsum() +
  guides(fill = FALSE)


urb  %>%
  group_by(urb_yr2) %>%
  summarize(Tracts = n(),
            pct_tracts = Tracts/72271*100)

urb  %>%
  group_by(county) %>%
  ggplot(urb, mapping = aes(x = urb_yr2, fill = urb_yr2)) +
  geom_bar() +
  labs(x ='', y = 'Number', title = "Total Census Tracts by Urbanized Boundary") +
  theme_ipsum() +
  guides(fill = FALSE)

urb  %>%
  group_by(county, urb_yr2) %>%
  summarize(Tracts = n(),
            pct_tracts = Tracts/72271*100)
