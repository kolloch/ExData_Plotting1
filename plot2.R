library(readr)
library(dplyr)

csv <- read_csv2(
    "household_power_consumption.txt",
    na = "?",
    col_types = "ccnnnnnnn"
)

problems(csv)

# convert date time string into POSIXct
asDateTime <- function (x) {
    as.POSIXct(strptime(x, "%m/%d/%Y %H:%M:%S"))
}

csv <- mutate( # Add DateTime column as POSIXct
    csv, 
    DateTime = asDateTime(paste(Date, Time))
) %>% filter( # filter the date range given in the exercise
    DateTime >= asDateTime("02/01/2007 00:00:00"),
    DateTime < asDateTime("02/03/2007 00:00:00")
) %>% select( # remove redundant columns
    -Date, 
    -Time
)

head(csv)

png(filename = "plot2.png", width = 480, height = 480)

with(
    csv,
    plot(
        DateTime, Global_active_power, 
        ylab = "Global Active Power (kilowatts)",
        xlab = "",
        type = "l"
    )
)

dev.off()