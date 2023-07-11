library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(janitor)
library(openair)
library(ggplot2)

Sys.setenv(TZ = 'UTC')
setwd('D:/Cruise/processed_data')


dat = read.csv("SEANA_data.csv") %>% 
  mutate(date = ymd_hms(date))
#for colour-coding background based on station
rects <- data.frame(xstart = c('2022-05-23 00:00','2022-05-23 06:15:00','2022-05-26 00:00','2022-05-29 00:00','2022-05-31 08:00','2022-06-01 08:00','2022-06-03 07:00','2022-06-03 20:00','2022-06-04 20:00','2022-06-05 11:00','2022-06-06 10:30','2022-06-06 18:30','2022-06-12 23:55','2022-06-13 20:40'),
                    xend = c('2022-05-23 06:15:00','2022-05-26 00:00','2022-05-29 00:00','2022-05-31 08:00','2022-06-01 08:00','2022-06-03 07:00','2022-06-03 20:00','2022-06-04 20:00','2022-06-05 11:00','2022-06-06 10:30','2022-06-06 18:30','2022-06-12 23:55','2022-06-13 20:40','2022-06-16 09:00'),
                    cols = c('Sailing','East Greenland','Sailing','Nuuk','Sailing','Maniitsoq 1','Sailing','Sisimiut','Sailing','Disko Bay','Sailing','Sea ice','Sailing','Maniitsoq 2'))

dat %>%
  mutate(stack_flag = case_when(rel_ws < 3 ~ "ws",
                                TRUE ~ stack_flag),
         co = case_when(flag_all == 1 ~ NA_real_,
                        co_flag == 1 ~ NA_real_,
                        between(date,as.POSIXct('2022-06-12 23:55'),as.POSIXct('2022-06-13 21:00')) ~ NA_real_,
                        # stack_flag == "wd" ~ NA_real_,
                        # stack_flag == "ws" ~ NA_real_,
                        # stack_flag == "other_ship" ~ NA_real_,
                        # stack_flag == "nox_spike" ~ NA_real_,
                        TRUE ~ co),
         no = case_when(flag_all == 1 ~ NA_real_,
                        nox_flag == 1 ~ NA_real_,
                        nox_cal == 1 ~ NA_real_,
                        zero_box == 1 ~ NA_real_,
                        between(date,as.POSIXct('2022-06-12 23:55'),as.POSIXct('2022-06-13 21:00')) ~ NA_real_,
                        # stack_flag == "wd" ~ NA_real_,
                        # stack_flag == "ws" ~ NA_real_,
                        # stack_flag == "other_ship" ~ NA_real_,
                        # stack_flag == "nox_spike" ~ NA_real_,
                        TRUE ~ no),
         no2 = case_when(flag_all == 1 ~ NA_real_,
                        nox_flag == 1 ~ NA_real_,
                        nox_cal == 1 ~ NA_real_,
                        zero_box == 1 ~ NA_real_,
                        between(date,as.POSIXct('2022-06-12 23:55'),as.POSIXct('2022-06-13 21:00')) ~ NA_real_,
                        # stack_flag == "wd" ~ NA_real_,
                        # stack_flag == "ws" ~ NA_real_,
                        # stack_flag == "other_ship" ~ NA_real_,
                        # stack_flag == "nox_spike" ~ NA_real_,
                        TRUE ~ no2),
         noy = case_when(flag_all == 1 ~ NA_real_,
                             noy_flag == 1 ~ NA_real_,
                             noy_cal == 1 ~ NA_real_,
                             noy_zero == 1 ~ NA_real_,
                         between(date,as.POSIXct('2022-06-12 23:55'),as.POSIXct('2022-06-13 21:00')) ~ NA_real_,
                         # stack_flag == "wd" ~ NA_real_,
                         # stack_flag == "ws" ~ NA_real_,
                         # stack_flag == "other_ship" ~ NA_real_,
                         # stack_flag == "nox_spike" ~ NA_real_,
                             TRUE ~ noy),
         so2 = case_when(flag_all == 1 ~ NA_real_,
                             so2_flag == 1 ~ NA_real_,
                             so2_zero_channel == 1 ~ NA_real_,
                         between(date,as.POSIXct('2022-06-12 23:55'),as.POSIXct('2022-06-13 21:00')) ~ NA_real_,
                         # stack_flag == "wd" ~ NA_real_,
                         # stack_flag == "ws" ~ NA_real_,
                         # stack_flag == "other_ship" ~ NA_real_,
                         # stack_flag == "nox_spike" ~ NA_real_,
                             TRUE ~ so2),
         o3 = case_when(flag_all == 1 ~ NA_real_,
                             o3_flag == 1 ~ NA_real_,
                        between(date,as.POSIXct('2022-06-12 23:55'),as.POSIXct('2022-06-13 21:00')) ~ NA_real_,
                        # stack_flag == "wd" ~ NA_real_,
                        # stack_flag == "ws" ~ NA_real_,
                        # stack_flag == "other_ship" ~ NA_real_,
                        # stack_flag == "nox_spike" ~ NA_real_,
                             TRUE ~ o3),
         stack_flag = case_when(stack_flag == "no" ~ "Good data",
                                stack_flag == "wd" ~ "RWD",
                                stack_flag == "ws" ~ "RWS",
                                stack_flag == "other_ship" ~ "NOx, CO, SO2 spike",
                                stack_flag == "nox_spike" ~ "NOx only spike")
  ) %>% 
  rename('O[3]' = o3,
         'NO[2]' = no2,
         'NO[y]' = noy,
         NO = no,
         CO = co,
         'SO[2]' = so2) %>%
  pivot_longer(c(CO,NO,'O[3]','NO[y]','NO[2]','SO[2]')) %>% 
  filter(
    date < '2022-06-16 09:00',
    date > "2022-05-23",
    # date != "2022-06-13"
    # stack_flag != "wd"
  ) %>%
  ggplot(aes(date,value,col = stack_flag)) +
  # geom_rect(data = rects, aes(xmin = as.POSIXct(xstart), xmax = as.POSIXct(xend), ymin = -Inf, ymax = Inf, fill = cols), alpha = 0.4) +
  geom_path(group = 1) +
  scale_colour_manual(values = c("black",'#EE9B00','#0A9396','#94D2BD','#9B2226')) +
  # scale_fill_manual(values = c('#94D2BD','#001219','#005F73','#0A9396','#E9D8A6','#EE9B00','#CA6702','#9B2226'),
                    # breaks = c('Sailing','East Greenland','Nuuk','Maniitsoq 1','Sisimiut','Disko Bay','Sea ice','Maniitsoq 2')) +
  labs(y = 'Mixing ratios / ppb',
       x = 'Date',
       fill = NULL,
       colour = NULL
       ) +
  facet_grid(rows = vars(name),scales = 'free_y',labeller = label_parsed) +
  theme_bw() +
  theme(legend.position = 'bottom')

ggsave('flagging.png',
       path = '~/SEANA/Plots',
       width = 31,
       height = 15,
       units = 'cm')
