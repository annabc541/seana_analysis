library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(janitor)
library(openair)
library(ggplot2)
library(viridis)

Sys.setenv(TZ = 'UTC')
setwd('D:/Cruise')

dat = read.csv("processed_data/SEANA_data.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(date))


# Looking into spikes on 8th June - rescue boat ---------------------------

dat_rb = dat %>% 
  filter(between(date,as.POSIXct("2022-06-08"),as.POSIXct("2022-06-09")),
         flag_all == 0,
         nox_flag == 0,
         co_flag == 0,
         nox_cal == 0,
         zero_box == 0) %>% 
  select(date,nox,co,no,no2,stack_flag)

dat_rb %>% 
  timeAverage("1 hour") %>% 
  rename('NO[2]' = no2,
         'NO[x]' = nox,
         NO = no,
         CO = co) %>%
  pivot_longer(c(CO,NO,'NO[2]','NO[x]')) %>% 
  ggplot(aes(date,value,col = name)) +
    geom_path(group = 1) +
    facet_grid(rows = vars(name),scales = "free_y",labeller = label_parsed) +
  scale_colour_manual(values = c('#EE9B00','#0A9396','#94D2BD','#9B2226')) +
  theme_bw() +
  theme(legend.position = "None") +
  labs(y = 'Mixing ratios / ppb',
       x = 'Date',
       fill = NULL,
       colour = NULL
  )

# Saving plots ------------------------------------------------------------

ggsave('8june_hourly.png',
       path = '~/SEANA/Plots/for science meeting',
       width = 31,
       height = 15,
       units = 'cm')
