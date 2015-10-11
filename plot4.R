library(readr)
library(dplyr)

csv <- read_csv(
    "household_power_consumption.txt",
    delim = ";",
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

png(filename = "plot4.png", width = 480, height = 480)

minMetering <- with(csv, min(Sub_metering_1, Sub_metering_2, Sub_metering_3))
maxMetering <- with(csv, max(Sub_metering_1, Sub_metering_2, Sub_metering_3))

par(mfrow = c(2, 2))

with( csv, {
    plot(
        DateTime, Global_active_power, 
        ylab = "Global Active Power",
        xlab = "",
        type = "l"
    )
    plot(
        DateTime, Voltage, 
        ylab = "Voltage",
        xlab = "",
        type = "l"
    )
    plot(
        c(min(DateTime), max(DateTime)), c(minMetering, maxMetering),
        ylab = "Energy sub metering",
        xlab = "",
        type = "n"
    )
    lines(DateTime, Sub_metering_1, col = "black")
    lines(DateTime, Sub_metering_2, col = "red")
    lines(DateTime, Sub_metering_3, col = "blue")
    legend(
        "topright", 
        legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
        fill = c("black", "red", "blue")    
    )
    plot(
        DateTime, Global_reactive_power, 
        ylab = "Global Reactive Power",
        xlab = "",
        type = "l"
    )
})

dev.off()