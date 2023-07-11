library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(janitor)
library(ggplot2)
library(viridis)
library(openair)
library(stringr)

Sys.setenv(TZ = 'UTC')

#For plotting NOx diurnals at different stations and comparing NOx with UV data and photolysis rates


#Use hours from conc date, not from UV data, because UV data only has one data point per hour
#'#94D2BD','#001219','#005F73','#0A9396','#E9D8A6','#EE9B00','#CA6702','#9B2226' colours
#Light blue, grey, dark blue, greeny-blue, cream, orange, dark orange, red

# Importing data ----------------------------------------------------------

files = list.files('data/uv', full.names=TRUE,pattern = "db")
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],na.strings= c('NA','missing'))%>%
    mutate(V1 = round(V1)) %>% 
    rename(hour = V1,
           sza = V2,
           UVB = V3,
           UVA = V4) %>% 
    select(hour,sza,UVB,UVA) %>% 
    tibble()
  
}

uv = bind_rows(datList)
uv_db = uv %>% 
  mutate(index = 1:nrow(uv),
         date = case_when(between(index,1,24) ~ as.POSIXct("2022-06-05"), #db
                          between(index,24,48) ~ as.POSIXct("2022-06-06"), #db
                          # between(index,1,24) ~ as.POSIXct("2022-05-23"), #eg
                          # between(index,24,48) ~ as.POSIXct("2022-05-24"), #eg
                          # between(index,48,72) ~ as.POSIXct("2022-05-25"), #eg
                          # between(index,72,96) ~ as.POSIXct("2022-05-26"), #eg
                          # between(index,1,24) ~ as.POSIXct("2022-06-13"), #m2
                          # between(index,24,48) ~ as.POSIXct("2022-06-14"), #m2
                          # between(index,48,72) ~ as.POSIXct("2022-06-15"), #m2
                          # between(index,72,96) ~ as.POSIXct("2022-06-16"), #m2
                          # between(index,216,240) ~ as.POSIXct("2022-06-15"),
                          # between(index,240,264) ~ as.POSIXct("2022-06-16")
                          ),
         date = glue::glue("{date} {hour}"),
         date = ymd_h(date)) %>% 
  select(-index) %>% 
  select(date,everything())

files = list.files('data/uv_photo', full.names=TRUE,pattern = "photo")
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],na.strings= c('NA','missing'))%>%
    mutate(V1 = round(V1)) %>% 
    rename(hour = V1,
           sza = V2,
           JO1D = V3,
           JNO2 = V5,
           JNO3_NO = V6,
           JNO3_NO2 = V7) %>%
    select(hour,sza,JO1D,JNO2,JNO3_NO,JNO3_NO2) %>%
    tibble()
  
}

photo = bind_rows(datList)
photo1 = photo %>%
  mutate(index = 1:nrow(photo),
         date = case_when(between(index,1,24) ~ as.POSIXct("2022-06-06"),
                          between(index,24,48) ~ as.POSIXct("2022-06-07"),
                          between(index,48,72) ~ as.POSIXct("2022-06-08"),
                          between(index,72,96) ~ as.POSIXct("2022-06-09"),
                          between(index,96,120) ~ as.POSIXct("2022-06-10"),
                          between(index,120,144) ~ as.POSIXct("2022-06-11"),
                          between(index,144,168) ~ as.POSIXct("2022-06-12"),
                          between(index,168,192) ~ as.POSIXct("2022-06-13"),
                          between(index,192,216) ~ as.POSIXct("2022-06-14"),
                          between(index,216,240) ~ as.POSIXct("2022-06-15"),
                          between(index,240,264) ~ as.POSIXct("2022-06-16")),
         date = glue::glue("{date} {hour}"),
         date = ymd_h(date)) %>%
  select(-index) %>%
  select(date,everything())

dat = read.csv("data/all_data_flagged_one_min.csv") %>% 
  mutate(date = ymd_hms(date),
         hour = hour(date),
         minute = minute(date),
         minute1 = minute/60,
         time1 = hour + minute1,
         doy = yday(date),
         time = format(as.POSIXct(date),format = '%H:%M:%S'),
         time = hms(time))

# for_uv = dat %>%
#   filter(station == "Disko Bay")%>%
#   timeAverage(avg.time = "1 day") %>%
#   select(date,lat,long)

# Sorting data UV ------------------------------------------------------------

diurnal_db = left_join(dat,uv_db,by = "date") %>% 
  select(-hour.x) %>%
  rename(hour = hour.y) %>% 
  mutate(co = ifelse(co_flag != 0,NA_real_,co),
         nox_cal = ifelse(date > '2022-05-23 08:30' & date < '2022-05-23 09:44',1,nox_cal),
         no = case_when(nox_flag != 0 ~ NA_real_,
                        nox_cal != 0 ~ NA_real_,
                        zero_box != 0 ~ NA_real_,
                        stack_flag != 0 ~ NA_real_,
                        flag_all != 0 ~ NA_real_,
                        date > '2022-06-15 10:00' & date < '2022-06-15 11:40' ~ NA_real_,
                        date > '2022-05-23 08:30' & date < '2022-05-23 12:44' ~ NA_real_,
                        date < "2022-05-25 07:30" & date > "2022-05-25 03:30" ~ NA_real_,
                        TRUE ~ no),
         no2 = case_when(nox_flag != 0 ~ NA_real_,
                         nox_cal != 0 ~ NA_real_,
                         zero_box != 0 ~ NA_real_,
                         stack_flag != 0 ~ NA_real_,
                         flag_all != 0 ~ NA_real_,
                         date > '2022-06-15 10:00' & date < '2022-06-15 11:40' ~ NA_real_,
                         date > '2022-05-23 08:30' & date < '2022-05-23 12:44' ~ NA_real_,
                         date < "2022-05-25 07:30" & date > "2022-05-25 03:30" ~ NA_real_,
                         TRUE ~ no2),
         nox = case_when(nox_flag != 0 ~ NA_real_,
                         nox_cal != 0 ~ NA_real_,
                         zero_box != 0 ~ NA_real_,
                         TRUE ~ nox),
         noy = case_when(noy_flag != 0 ~ NA_real_,
                         noy_cal != 0 ~ NA_real_,
                         noy_zero != 0 ~ NA_real_,
                         TRUE ~ noy),
         o3 = ifelse(o3_flag != 0,NA_real_,o3),
         ox = nox + o3) %>% 
  filter(
    station == "Disko Bay",
    # stack_flag == "no",
    # flag_all == 0,
  ) %>% 
  timeAverage(avg.time = "1 hour") %>% 
  group_by(hour) %>%
  summarise(
    date = date,
    no = mean(no,na.rm = TRUE),
    no2 = mean(no2,na.rm = TRUE),
    nox = mean(nox,na.rm = TRUE),
    noy = mean(noy,na.rm = TRUE),
    u = mean(u,na.rm = TRUE),
    v = mean(v,na.rm = TRUE),
    o3 = mean(o3,na.rm = TRUE),
    co = mean(co,na.rm = TRUE),
    UVA = mean(UVA,na.rm = TRUE),
    UVB = mean(UVB,na.rm = TRUE),
    ox = mean(ox,na.rm = TRUE),
    lat = lat,
    long = long,
  ) %>% 
  mutate(ws = sqrt(u^2+v^2),
         wd = atan2(u/ws,v/ws)*180/pi+180) %>% 
  ungroup() %>% 
  arrange(date)

diurnal_db %>%
  rename(NO = no,
         'NO[2]' = no2,
         "O[3]" = o3,
         "O[x]" = ox,
         CO = co) %>% 
  pivot_longer(c(NO,'NO[2]',UVA,UVB)) %>%
  ggplot(aes(hour,value,col = name)) +
  geom_line(size = 1) +
  facet_grid(rows = vars(name),scales = "free_y",labeller = label_parsed) +
  scale_color_manual(values = c('#001219','#005F73','#CA6702','#9B2226')) +
  labs(x = "Hour of day (UTC)",
       y = NULL) +
  theme_bw() +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  theme(legend.position = "none",
        text = element_text(size = 16))
  NULL
  
ggsave('eg.svg',
         path = 'output/diurnals',
         width = 31,
         height = 15,
         units = 'cm')


# Sorting data photo ------------------------------------------------------

diurnal = left_join(dat,photo1,by = "date") %>% 
  select(-hour.y) %>%
  rename(hour = hour.x) %>% 
  mutate(co = ifelse(co_flag != 0,NA_real_,co),
         no = case_when(nox_flag != 0 ~ NA_real_,
                        nox_cal != 0 ~ NA_real_,
                        zero_box != 0 ~ NA_real_,
                        stack_flag != "no" ~ NA_real_,
                        flag_all != 0 ~ NA_real_,
                        date > '2022-06-15 10:00' & date < '2022-06-15 11:40' ~ NA_real_,
                        TRUE ~ no),
         no2 = case_when(nox_flag != 0 ~ NA_real_,
                         nox_cal != 0 ~ NA_real_,
                         zero_box != 0 ~ NA_real_,
                         stack_flag != "no" ~ NA_real_,
                         flag_all != 0 ~ NA_real_,
                         date > '2022-06-15 10:00' & date < '2022-06-15 11:40' ~ NA_real_,
                         TRUE ~ no2),
         nox = case_when(nox_flag != 0 ~ NA_real_,
                         nox_cal != 0 ~ NA_real_,
                         zero_box != 0 ~ NA_real_,
                         TRUE ~ nox),
         noy = case_when(noy_flag != 0 ~ NA_real_,
                         noy_cal != 0 ~ NA_real_,
                         noy_zero != 0 ~ NA_real_,
                         TRUE ~ noy),
         o3 = ifelse(o3_flag != 0,NA_real_,o3),
         ox = nox + o3) %>%
  filter(
    station == "Disko Bay",
    # stack_flag == "no",
    # flag_all == 0,
  ) %>% 
  timeAverage(avg.time = "1 hour") %>% 
  group_by(hour) %>%
  summarise(
    date = date,
    no = mean(no,na.rm = TRUE),
    no2 = mean(no2,na.rm = TRUE),
    nox = mean(nox,na.rm = TRUE),
    noy = mean(noy,na.rm = TRUE),
    u = mean(u,na.rm = TRUE),
    v = mean(v,na.rm = TRUE),
    o3 = mean(o3,na.rm = TRUE),
    co = mean(co,na.rm = TRUE),
    JO1D = mean(JO1D),
    JNO2 = mean(JNO2),
    JNO3_NO = mean(JNO3_NO),
    JNO3_NO2 = mean(JNO3_NO2),
    ox = mean(ox,na.rm = TRUE),
    lat = lat,
    long = long,
  ) %>% 
  mutate(ws = sqrt(u^2+v^2),
         wd = atan2(u/ws,v/ws)*180/pi+180) %>% 
  ungroup() %>% 
  arrange(date)

diurnal %>%
  rename(NO = no,
         'NO[2]' = no2,
         "O[3]" = o3,
         "O[x]" = ox,
         CO = co) %>% 
  pivot_longer(c(NO,'NO[2]',JO1D)) %>%
  ggplot(aes(hour,value,col = name)) +
  geom_line(size = 1) +
  facet_grid(rows = vars(name),scales = "free_y",labeller = label_parsed) +
  scale_color_manual(values = c('#001219','#005F73','#94D2BD','#CA6702','#9B2226')) +
  labs(x = "Hour of day (UTC)",
       y = NULL) +
  theme_bw() +
  # scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  theme(legend.position = "none",
        text = element_text(size = 16))
NULL

ggsave('ox_diurnal.svg',
       path = '~/SEANA/Plots/for science meeting',
       width = 31,
       height = 15,
       units = 'cm')

# Openair diurnals --------------------------------------------------------
  
myplot = dat %>% 
  filter(station == "East Greenland",
         nox_flag == 0,
         nox_cal == 0,
         zero_box == 0,
         stack_flag == "no") %>% 
  timeVariation(pollutant = c("no","no2","co","o3"))

diurnal_dat1 = myplot$data$hour

diurnal_dat1 %>% 
ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  facet_grid(rows = vars(variable),scales = "free_y") +
  scale_color_manual(values = viridis(4)) +
  theme_bw() +
  theme(legend.position = "None")
NULL


plot(myplot,subset = "hour")


# East Greenland timeseries  ----------------------------------------------

dat %>% 
  filter(station == "East Greenland",
         # stack_flag == "no",
         # nox_flag == 0,
         # nox_cal == 0,
         # flag_all == 0,
         # date < "2022-05-25 07:30" & date > "2022-05-25 03:30",
         no < 2.5
  ) %>%
  mutate(no = case_when(nox_flag != 0 ~ NA_real_,
                        nox_cal != 0 ~ NA_real_,
                        zero_box != 0 ~ NA_real_,
                        stack_flag != "no" ~ NA_real_,
                        flag_all != 0 ~ NA_real_,
                        date > '2022-06-15 10:00' & date < '2022-06-15 11:40' ~ NA_real_,
                        date > '2022-05-23 08:25' & date < '2022-05-23 09:44' ~ NA_real_,
                        # date < "2022-05-25 07:30" & date > "2022-05-25 03:30" ~ NA_real_,
                        TRUE ~ no),
         no2 = case_when(nox_flag != 0 ~ NA_real_,
                         nox_cal != 0 ~ NA_real_,
                         zero_box != 0 ~ NA_real_,
                         stack_flag != "no" ~ NA_real_,
                         flag_all != 0 ~ NA_real_,
                         date > '2022-06-15 10:00' & date < '2022-06-15 11:40' ~ NA_real_,
                         date > '2022-05-23 08:25' & date < '2022-05-23 09:44' ~ NA_real_,
                         # date < "2022-05-25 07:30" & date > "2022-05-25 03:30" ~ NA_real_,
                         TRUE ~ no2)) %>% 
  rename(NO = no,
         'NO[2]' = no2) %>% 
  pivot_longer(c(NO,'NO[2]')) %>% 
  ggplot(aes(date,value)) +
  geom_path(size = 1) +
  # geom_point() +
  facet_grid(rows = vars(name),scales = "free_y",labeller = label_parsed) + 
  labs(x = "Date",
       y = NULL) +
  scale_x_datetime(date_breaks = "12 hours",date_labels = "%Y-%m-%d %H:%M") +
  theme_bw()
