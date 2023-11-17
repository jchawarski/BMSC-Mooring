# BMSC AZFP June-August 2023 - Mackenzie Anchorage

require(tidyverse)
require(cowplot)
require(WaveletComp)
require(tidyquant)   # contains handy moving average calcs
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


#### Data Preparation ####
# read in Echoview output (.csv) 
# Exported using surface line and nearfield exclusions at 10 minute intervals

krill <- read.csv("BMSC_125kHz_krill_dbdiff_10min.csv")

require(lubridate)
krill$Date <- as.Date(as.character(krill$Date_M),format="%Y%m%d")
format <- "%Y-%m-%d %H:%M:%S"
# combining date and time into single object
krill$datetime <- as.POSIXct(paste(as.character(krill$Date), krill$Time_M), format=format, tz="UTC")
krill$Datetime <- as.Date(krill$datetime)

krill <- krill %>% filter(Sv_mean > -900)


lat <-  48.8402
lon <- -125.1720833

require(oce)

sun_angle <- sunAngle(t=krill$datetime, lat= lat,lon= lon)
krill$sun_altitude <- sun_angle$altitude
krill <- mutate(krill, ToD = case_when(sun_altitude > 6 ~ "Day",
                                       sun_altitude <= 6 & sun_altitude > -6 ~ "Twilight",
                                       sun_altitude <= -6 ~ "Night"))

Sv_label <- expression(paste("S"["v"]," [dB re 1 m" ^-1,"]"))

require(tidyquant)
require(showtext)
font_add_google("Barlow") # add ASL font type
showtext_auto() # gives showtext permission to overwrite ggplot default

#### Echometric plot with 6-hour moving averages ####

#MVBS 
p1 <- 
  krill %>% 
  #filter(Sv_mean < -65) %>% 
  ggplot(aes(x=datetime, y=Sv_mean)) + geom_point(color= wave2, alpha=0.1) + geom_ma(ma_fun = SMA, n = 6, color=wave1, linetype="solid") + 
  scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
  #scale_y_continuous(breaks = c(-100, -96, -92, -88, -84, -80, -76), position = "left") + 
  xlab("") + ylab(Sv_label) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) + 
  #ggtitle("Cape Bathurst AZFP - April 2018")
  theme(axis.text.x = element_blank())

#CoM
p2 <- 
  krill %>%
  #filter(Sv_mean < -65) %>%
  ggplot(aes(x=datetime, y=50-Center_of_mass)) + geom_point(color= mars2, alpha=0.1) + geom_ma(ma_fun = SMA, n = 6, color=mars1, linetype="solid") + 
  scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
  scale_y_reverse(position="right") + 
  xlab("") + ylab("Center of mass [m]") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) + 
  #ggtitle("Cape Bathurst AZFP - April 2018")
  theme(axis.text.x = element_blank())


#Inertia
in_lab <- expression(paste("Inertia [m" ^-2,"]")) # label for inertia
p3 <- krill %>%
  #filter(Sv_mean < -65) %>%
  ggplot(aes(x=datetime, y=Inertia)) + geom_point(color= coral2, alpha=0.1) + geom_ma(ma_fun = SMA, n = 6, color=coral1, linetype="solid") + 
  scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
  #scale_y_continuous(breaks = c(-100, -96, -92, -88, -84, -80, -76)) + 
  xlab("") + ylab(in_lab) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) +
  #ggtitle("Cape Bathurst AZFP - April 2018")
  theme(axis.text.x = element_blank())


#Proportion Occupied
p_lab <- expression(paste("P"["occ"]))
p4 <- 
  krill %>%
  #filter(Sv_mean < -65) %>%
  ggplot(aes(x=datetime, y=Proportion_occupied)) + geom_point(color= algae2, alpha=0.1) + geom_ma(ma_fun = SMA, n = 6, color=algae1, linetype="solid") + 
  scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
  scale_y_continuous(position = "right") + 
  xlab("") + ylab(p_lab) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) +
  #ggtitle("Cape Bathurst AZFP - April 2018")
  theme(axis.text.x = element_blank())


#Equivalent Area
p5 <-
  krill %>%
  #filter(Sv_mean < -65) %>%
  ggplot(aes(x=datetime, y=Equivalent_area)) + geom_point(color= aqua2, alpha=0.1) + geom_ma(ma_fun = SMA, n = 6, color=aqua1, linetype="solid") + 
  scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
  scale_y_continuous(position = "left") + 
  xlab("") + ylab("EA") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) +
  #ggtitle("Cape Bathurst AZFP - April 2018")
  theme(axis.text.x = element_blank())

#Aggregation Index
p6 <- 
  krill %>%
  #filter(Sv_mean < -65) %>%
  ggplot(aes(x=datetime, y=Aggregation_index)) + geom_point(color= redwood2, alpha=0.1) + geom_ma(ma_fun = SMA, n = 6, color=redwood1, linetype="solid") + 
  scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
  scale_y_continuous(position = "right") + 
  xlab("") + ylab("AI") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) 

require(cowplot)
plot_grid(p1,p2,p3,p4,p5, p6, ncol=1, align ="v", greedy = F)



# daytime Sv

krill %>%
  filter(ToD %in% "Day") %>% 
  ggplot(aes(x=datetime, y=Sv_mean)) + geom_point(color= wave2, alpha=0.1) + #geom_ma(ma_fun = SMA, n = 6, color=wave1, linetype="solid") + 
  #scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
  #scale_y_continuous(breaks = c(-100, -96, -92, -88, -84, -80, -76), position = "left") + 
  xlab("") + ylab(Sv_label) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow"))  
  #ggtitle("Cape Bathurst AZFP - April 2018")
  theme(axis.text.x = element_blank())
  
  krill %>%
    filter(ToD %in% "Day") %>%
    ggplot(aes(x=datetime, y=Sv_mean, group=Date)) + geom_boxplot()
  
  
  
  krill %>% 
      filter(datetime >= "2023-07-04 00:25:00 UTC" & datetime < "2023-07-05 00:25:00 UTC") %>%
    filter(ToD %in% "Day") %>%
    ggplot(aes(x=datetime, y=Proportion_occupied, color=ToD)) + geom_point() + #geom_ma(ma_fun = SMA, n = 6, linetype="solid") + 
    #scale_x_datetime(date_minor_breaks = "1 day", date_breaks = "1 week", date_labels = "%b %d") + 
    #scale_y_continuous(breaks = c(-100, -96, -92, -88, -84, -80, -76), position = "left") + 
    xlab("") +  ylab("Proportion") + #ylab(Sv_label) + 
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text("Barlow")) 

  
  
  
  
  
  krill.sum <- krill %>% group_by(Date, ToD) %>% summarise(svmean = mean(Sv_mean), 
                                                      svsd = sd(Sv_mean),
                                                      pocc = mean(Proportion_occupied),
                                                      posd = sd(Proportion_occupied),
                                                      )  

  light.sum <- light %>% filter(Dry %in% 0) %>% filter(Light.Level < 100) %>% group_by(Date, ToD) %>%
                                                  summarise(meanlight = mean(Light.Level),
                                                            sdlight = sd(Light.Level))

  mod.dat <- data.frame(krill.sum) %>% left_join(data.frame(light.sum), by=c("Date", "ToD"))
  
  mod.dat %>% filter(ToD %in% "Day") %>%
  ggplot(aes(y=pocc, x=meanlight)) + geom_point() + geom_smooth(method= "lm") + xlab("Daytime light level") + ylab("Proportion") + 
    theme_bw()

