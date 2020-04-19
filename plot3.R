
#----- ENVIRONMENT -----#

reqd_pkgs <- c("lubridate", # date handling functions - must be loaded before "here" to avoid namespace conflict!
               "here", # construct file paths relative to the project root
               "tidyverse", # language augmentations including dplyr
               "magrittr") # advanced pipe operators
for (pkg in reqd_pkgs[!(reqd_pkgs %in% installed.packages())])  install.packages(pkg)
for (pkg in reqd_pkgs[!(reqd_pkgs %in% (.packages()))])         library(pkg, character.only = TRUE)


#----- DATA PREPARATION -----#

# Download if necessary
if (!file.exists(here("data","household_power_consumption.txt"))) { # data not yet downloaded
  
  if (!file.exists(here("data"))) dir.create(here("data"))
  
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                destfile=here("data","household_power_consumption.zip"))
  unzip(here("data","household_power_consumption.zip"), exdir=here("data"))
  
  unlink(here("data","household_power_consumption.zip")) # delete unnecessary zip file 
  
}

# Read into R
df_raw <- 
  read.table(here("data","household_power_consumption.txt"),
             header=TRUE, sep=";", 
             colClasses=c(rep("character",2),rep("numeric",7)), na.strings=c("?"),
             stringsAsFactors=FALSE) %>%
  filter(Date %in% c("1/2/2007","2/2/2007")) # minimise memory consumption

# Format appropriately
df_clean <- 
  df_raw %>% 
  mutate(Timestamp = as.POSIXct(dmy(Date)+hms(Time))) %>%
  select(-Date, -Time) %>%
  select(Timestamp, everything()) # move TS column to start

#----- CHARTS -----#

png(here("plot3.png"), width=480, height=480)

with(df_clean, {
  plot(x=Timestamp, y=pmax(Sub_metering_1, Sub_metering_2, Sub_metering_3), type="n",
       xlab=NA, ylab="Energy sub metering")
  lines(x=Timestamp, y=Sub_metering_1, col="black")
  lines(x=Timestamp, y=Sub_metering_2, col="red")
  lines(x=Timestamp, y=Sub_metering_3, col="blue")
  print(strwidth(legend("topright", lwd=1, 
                        c(paste0("Sub_metering_",1:3)), col=c("black","red","blue"))))
  legend("topright", lwd=1,
         c(paste0("Sub_metering_",1:3)), col=c("black","red","blue"))
})

dev.off()