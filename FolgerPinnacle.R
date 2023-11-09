setwd("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/BMSC Summer Oceanography Course")

files <- list.files("Data/ONC Data/Chloro and Turb", pattern = "*.csv", full.names = T)

onc <- lapply(files, function(i){read.csv(i, skip = 54, header = F)})                          # concatenates and trims upcast from all CTD files into large list
onc <- bind_rows(onc)

colnames(onc) <- c("datetime", "chlorophyll", "chl_flag", "turbidity", "turb_flag")

onc$Datetime <- as_datetime(onc$datetime)

# reduce data into 1 hour summary

onc.down  <-    onc %>%
      mutate(date = as.Date(Datetime), 
         hour = hour(Datetime)) %>%
      group_by(date, hour) %>%
        summarise(chl = mean(chlorophyll),
                  turb =mean(turbidity))

format <- "%Y-%m-%d %H:%M:%S"

onc.down$time <- paste(onc.down$hour, as.character("00"), as.character("00"),  sep=":")

onc.down$datetime <- as.POSIXct(paste(as.character(onc.down$date), onc.down$time), format=format, tz="UTC")

onc.down %>% filter(datetime >= "2023-06-23 00:00:00 UTC" & datetime < "2023-08-01 23:00:00 UTC") %>%
  ggplot(aes(x=datetime, y=turb)) + geom_line() + theme_bw()



files <- list.files("Data/ONC Data/PAR", pattern = "*.csv", full.names = T)

onc <- lapply(files, function(i){read.csv(i, skip = 54, header = F)})                          # concatenates and trims upcast from all CTD files into large list
onc <- bind_rows(onc)

colnames(onc) <- c("datetime", "PAR", "PAR_flag")
onc$Datetime <- as_datetime(onc$datetime)

# reduce data into 1 hour summary

onc.PAR  <-    onc %>%
  mutate(date = as.Date(Datetime), 
         hour = hour(Datetime)) %>%
  group_by(date, hour) %>%
  summarise(PAR = mean(PAR))

format <- "%Y-%m-%d %H:%M:%S"

onc.PAR$time <- paste(onc.PAR$hour, as.character("00"), as.character("00"),  sep=":")

onc.PAR$datetime <- as.POSIXct(paste(as.character(onc.PAR$date), onc.PAR$time), format=format, tz="UTC")

onc.PAR %>% filter(datetime >= "2023-06-23 00:00:00 UTC" & datetime < "2023-08-01 23:00:00 UTC") %>%
  ggplot(aes(x=datetime, y=PAR)) + geom_line() + theme_bw()







