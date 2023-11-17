# MK9 Light tag Analysis
# project initiated August 2, 2023 by J. Chawarski, ASL Environmental Sciences

require(tidyverse)
require(cowplot)
require(WaveletComp)
require(tidyquant) 

#### Build ASL color palette ####

# build ASL color palette
kelp1 <- "#334736"
kelp2 <- "#4d754f"
kelp3 <- "#678c6c"

redwood1 <- "#743e1e"
redwood2 <- "#8d4d2b"
redwood3 <- "#ab5732"

starecho.april1 <- "#3d3556"
starecho.april2 <- "#4f4070"
starecho.april3 <- "#6a5499"

beach1 <- "#d7d6c9"
beach2 <- "#e2e1d3"
beach3 <- "#f5f3ea"

coral1 <- "#623f72"
coral2 <- "#8658a4"

aqua1 <- "#17817e"
aqua2 <- "#00bbbc"

wave1 <- "#2787ba"
wave2 <- "#2c9bd6"

mars1 <- "#9b4726"
mars2 <- "#e1642f"

algae1 <- "#007c45"
algae2 <- "#00af5e"

require(showtext)
font_add_google("Barlow") # add ASL font type
showtext_auto() # gives showtext permission to overwrite ggplot default


setwd("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/BMSC Summer Oceanography Course")

light <- read.csv("Data/Light Tag - MK9/2190089-Archive.csv")

# convert Time to datetime object 

light$Time <- as.POSIXct(strptime(light$Time, format="%m/%d/%Y %H:%M:%S"), tz="UTC")
light$Date <- as.Date(light$Time, tz="UTC")

light %>% filter(Dry %in% 0) %>% filter(Light.Level < 100) %>% filter(ToD %in% "Day") %>%
  filter(Time >= "2023-07-04 00:25:00 UTC" & Time < "2023-07-05 00:25:00 UTC") %>%
  ggplot(aes(x=Time, y=Light.Level, color=ToD)) + geom_point(alpha=0.1) + #geom_ma(ma_fun = SMA, n = 6, color=wave1, linetype="solid") + 
  #scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
  #scale_y_continuous(breaks = c(-100, -96, -92, -88, -84, -80, -76), position = "left") + 
  xlab("") + ylab("Light Level") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) 
  #ggtitle("Cape Bathurst AZFP - bmsc 2018")
  #theme(axis.text.x = element_blank())

# one minute light level
light %>% filter(Dry %in% 0) %>% filter(Light.Level < 100) %>%
  ggplot(aes(x=Time, y=One.Minute.Light.Level)) + geom_point(color= wave2, alpha=0.1) + #geom_ma(ma_fun = SMA, n = 6, color=wave1, linetype="solid") + 
  scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
  #scale_y_continuous(breaks = c(-100, -96, -92, -88, -84, -80, -76), position = "left") + 
  xlab("") + ylab("Light Level") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) 
  #ggtitle("Cape Bathurst AZFP - bmsc 2018")
  #theme(axis.text.x = element_blank())

light %>% filter(Dry %in% 0) %>% filter(Light.Level < 100) %>%
  ggplot(aes(x=Time, y=External.Temp)) + geom_point(color= wave2, alpha=0.1) + #geom_ma(ma_fun = SMA, n = 6, color=wave1, linetype="solid") + 
  scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
  #scale_y_continuous(breaks = c(-100, -96, -92, -88, -84, -80, -76), position = "left") + 
  xlab("") + ylab("Light Level") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) + 
  #ggtitle("Cape Bathurst AZFP - bmsc 2018")
  theme(axis.text.x = element_blank())




# trim to underwater deployment using wet/dry setting (Dry=1 -> Dry, Dry=0 -> Wet)

# calculate solar angles and moon angles for location using 'oce' package

  
lat <-  48.8402
lon <- -125.1720833
  
require(oce)
  
  sun_angle <- sunAngle(t=light$Time, lat= lat,lon= lon)
  light$sun_altitude <- sun_angle$altitude
  light <- mutate(light, ToD = case_when(sun_altitude > 6 ~ "Day",
                                         sun_altitude <= 6 & sun_altitude > -6 ~ "Twilight",
                                         sun_altitude <= -6 ~ "Night"))
  
  light %>% filter(Dry %in% 0) %>% filter(Light.Level < 100) %>%
    ggplot(aes(x=Time, y=sun_altitude)) + geom_point(color= wave2, alpha=0.1) + #geom_ma(ma_fun = SMA, n = 6, color=wave1, linetype="solid") + 
    scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
    #scale_y_continuous(breaks = c(-100, -96, -92, -88, -84, -80, -76), position = "left") + 
    xlab("") + ylab("sun altitude") + 
    theme_bw() 
  
  
  moon_angle <- moonAngle(t=light$Time, lat=lat, lon=lon)
  light$moon_fraction <- moon_angle$illuminatedFraction
  light$moon_angle <- moon_angle$altitude
  
  #moon fraction
  light %>% filter(Dry %in% 0) %>% filter(Light.Level < 100) %>%
    ggplot(aes(x=Time, y=moon_fraction)) + geom_point(color= wave2, alpha=0.1) + #geom_ma(ma_fun = SMA, n = 6, color=wave1, linetype="solid") + 
    scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
    xlab("") + ylab("Moon Fraction") + 
    theme_bw() 
  
  #moon altitude
  light %>% filter(Dry %in% 0) %>% filter(Light.Level < 100) %>%
    ggplot(aes(x=Time, y=moon_angle)) + geom_point(color= wave2, alpha=0.1) + #geom_ma(ma_fun = SMA, n = 6, color=wave1, linetype="solid") + 
    scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
    xlab("") + ylab("Moon Angle") + 
    theme_bw() 
  
  
  
# use solar angle to determine light anomaly -> expected light based on solar angle minus measured light level. 
# how does this impact DVM patterns seen in krill
require(oce)
CTD <- read.ctd("Data/SMP37 - CTD/BMSC_bottom_CTD_SBE37SM.cnv")

ctd <- CTD[["data"]] 
  
 ctd <- data.frame(ctd) 
  
 x <- seconds(ctd$timeK)
 y <- y %m+% years(30)
 y <- with_tz(y, "PDT")
y <- as.Date(x)

ctd$datetime.utc <- y

write.csv(ctd, "BMSC_mooring_bottomCTD.csv")

  #### Data Preparation ####
  # read in Echoview output (.csv) 
  # Exported using surface line and nearfield exclusions at 60 minute intervals

  echo.bmsc <- read.csv("Data/Processed Acoustic Data/BMSC_1hr_integration_67kHz.csv")
  
  require(lubridate)
  echo.bmsc$Date <- as.Date(as.character(echo.bmsc$Date_M),format="%Y%m%d")
  format <- "%Y-%m-%d %H:%M:%S"
  # combining date and time into single object
  echo.bmsc$datetime <- as.POSIXct(paste(as.character(echo.bmsc$Date), echo.bmsc$Time_M), format=format, tz="UTC")
  echo.bmsc$Datetime <- as.Date(echo.bmsc$datetime)
  
  Sv_label <- expression(paste("S"["v"]," [dB re 1 m" ^-1,"]"))
  
  require(tidyquant)
  require(showtext)
  font_add_google("Barlow") # add ASL font type
  showtext_auto() # gives showtext permission to overwrite ggplot default
  
  #### Echometric plot with 6-hour moving averages ####
  
  #MVBS 
  p1 <- 
    echo.bmsc %>%
    filter(Sv_mean < -60) %>%
    ggplot(aes(x=datetime, y=Sv_mean)) + geom_point(color= wave2, alpha=0.1) + geom_ma(ma_fun = SMA, n = 6, color=wave1, linetype="solid") + 
    scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
    scale_y_continuous(breaks = c(-100, -96, -92, -88, -84, -80, -76, -72, -68, -64, -60), position = "left") + 
    xlab("") + ylab(Sv_label) + 
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) + 
    #ggtitle("Cape Bathurst AZFP - bmsc 2018")
    theme(axis.text.x = element_blank())
  
  #CoM
  p2 <- 
    echo.bmsc %>%
    filter(Sv_mean < -60) %>%
    ggplot(aes(x=datetime, y=Center_of_mass)) + geom_point(color= mars2, alpha=0.1) + geom_ma(ma_fun = SMA, n = 6, color=mars1, linetype="solid") + 
    scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
    scale_y_reverse(position="right") + 
    xlab("") + ylab("Center of mass [m]") + 
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) + 
    #ggtitle("Cape Bathurst AZFP - bmsc 2018")
    theme(axis.text.x = element_blank())
  
  
  #Inertia
  in_lab <- expression(paste("Inertia [m" ^-2,"]")) # label for inertia
  p3 <- echo.bmsc %>%
    filter(Sv_mean < -60) %>%
    ggplot(aes(x=datetime, y=Inertia)) + geom_point(color= coral2, alpha=0.1) + geom_ma(ma_fun = SMA, n = 6, color=coral1, linetype="solid") + 
    scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
    #scale_y_continuous(breaks = c(-100, -96, -92, -88, -84, -80, -76)) + 
    xlab("") + ylab(in_lab) + 
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) +
    #ggtitle("Cape Bathurst AZFP - bmsc 2018")
    theme(axis.text.x = element_blank())
  
  
  #Proportion Occupied
  p_lab <- expression(paste("P"["occ"]))
  p4 <- 
    echo.bmsc %>%
    filter(Sv_mean < -60) %>%
    ggplot(aes(x=datetime, y=Proportion_occupied)) + geom_point(color= algae2, alpha=0.1) + geom_ma(ma_fun = SMA, n = 6, color=algae1, linetype="solid") + 
    scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
    scale_y_continuous(position = "right") + 
    xlab("") + ylab(p_lab) + 
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) +
    #ggtitle("Cape Bathurst AZFP - bmsc 2018")
    theme(axis.text.x = element_blank())
  
  
  #Equivalent Area
  p5 <-
    echo.bmsc %>%
    filter(Sv_mean < -60) %>%
    ggplot(aes(x=datetime, y=Equivalent_area)) + geom_point(color= aqua2, alpha=0.1) + geom_ma(ma_fun = SMA, n = 6, color=aqua1, linetype="solid") + 
    scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
    scale_y_continuous(position = "left") + 
    xlab("") + ylab("EA") + 
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) +
    #ggtitle("Cape Bathurst AZFP - bmsc 2018")
    theme(axis.text.x = element_blank())
  
  #Aggregation Index
  p6 <- 
    echo.bmsc %>%
    filter(Sv_mean < -60) %>%
    ggplot(aes(x=datetime, y=Aggregation_index)) + geom_point(color= redwood2, alpha=0.1) + geom_ma(ma_fun = SMA, n = 6, color=redwood1, linetype="solid") + 
    scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
    scale_y_continuous(position = "right") + 
    xlab("") + ylab("AI") + 
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) 
  
  require(cowplot)
  plot_grid(p1,p2,p3,p4,p5, p6, ncol=1, align ="v", greedy = F)
  
  
  
  

