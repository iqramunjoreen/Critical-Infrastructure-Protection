library("depmixS4")
library("ggplot2")
library("corrplot")
library("gridExtra")
library("forecast")
library("stats")
library("ggbiplot")
library(devtools)

getwd()
setwd("C:/Users/bansa/Downloads")
data <- read.csv(file="TermProjectData.txt", header=TRUE, sep=",", na.strings = c("", "NA"))
data[is.na(data)] <- 0
data$Date <- strptime(as.character(data$Date), "%d/%m/%Y")
data$Day <- as.POSIXlt(data$Date)$wday

wkdays <- subset(data, Day > 0 & Day < 6)
wkdays <- subset(wkdays, (strptime(Time, format = "%H:%M:%S") >= strptime("18:00:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") < strptime("22:00:00", format = "%H:%M:%S")))

wkends <- wkends <- subset(data, Day == 0 | Day == 6)
wkends <- subset(wkends, (strptime(Time, format = "%H:%M:%S") >= strptime("18:00:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") < strptime("22:00:00", format = "%H:%M:%S")))

train_set_wkday <- subset(wkdays, Date < "2009-01-01")
train_set_wkend <- subset(wkends, Date < "2009-01-01")

test_set_wkday <- subset(wkdays, Date >= "2009-01-01")
test_set_wkend <- subset(wkends, Date >= "2009-01-01")

# weekdays
train_set_wkday_data_variables_only <- train_set_wkday[, 3:9]
pca_wkday <- prcomp(train_set_wkday_data_variables_only, scale. = TRUE)
pca_wkday

# circular ggbiplot weekdays
ggbiplot(pca_wkday, circle = TRUE, alpha = 0.001)

# bar plot weekdays
pca_wkday.var = pca_wkday$sdev ^ 2
pca_wkday.var.per <- round(pca_wkday.var/sum(pca_wkday.var)*100, 1)
barplot(pca_wkday.var.per, main="Scree Plot", xlab = "PC", ylab = "Percentage Variation")


# weekends
train_set_wkend_data_variables_only <- train_set_wkend[, 3:9]
pca_wkend <- prcomp(train_set_wkend_data_variables_only, scale. = TRUE)
pca_wkend

# circular ggbiplot weekdays
ggbiplot(pca_wkend, circle = TRUE, alpha = 0.001)

# bar plot weekdays
pca_wkend.var = pca_wkend$sdev ^ 2
pca_wkend.var.per <- round(pca_wkend.var/sum(pca_wkend.var)*100, 1)
barplot(pca_wkend.var.per, main="Scree Plot Weekends", xlab = "PC", ylab = "Percentage Variation")


0.352 * 0.60462310 # pc1 * pc1_global_intensity
0.352 * 0.50045151 # pc1 * pc1_global_active_power

0.1505 * 0.783596323 # pc2 * pc2_voltage

# Variables for Weekdays = Global_intensity & Global_active_power

0.3974 * 0.57223047 # pc1 * pc1_global_intensity
0.3974 * 0.49776249 # pc1 * pc1_global_active_power

# Variables for weekends = Global_intensity & Global_active_power


#### --------- UNIVARIATE TRAINING - WEEKDAYS

# Number of states: 10
train_wkday_univariate_1 <- depmix(response =Global_intensity ~ 1, data = train_set_wkday,
               nstates = 10, ntimes = nrow(train_set_wkday))
train_wkday_univariate_1_line_1 <- fit(train_wkday_univariate_1)
print(train_wkday_univariate_1_line_1)
summary(train_wkday_univariate_1_line_1)

# Number of states: 13
train_wkday_univariate_2 <- depmix(response =Global_intensity ~ 1, data = train_set_wkday,
                                   nstates = 13, ntimes = nrow(train_set_wkday))
train_wkday_univariate_2_line_2 <- fit(train_wkday_univariate_2)
print(train_wkday_univariate_2_line_2)
summary(train_wkday_univariate_2_line_2)

# Number of states: 17
train_wkday_univariate_3 <- depmix(response =Global_intensity ~ 1, data = train_set_wkday,
                                   nstates = 17, ntimes = nrow(train_set_wkday))
train_wkday_univariate_3_line_3 <- fit(train_wkday_univariate_3)
print(train_wkday_univariate_3_line_3)
summary(train_wkday_univariate_3_line_3)


#### --------- UNIVARIATE TRAINING - WEEKENDS
# Number of states: 10
train_wkend_univariate_4 <- depmix(response =Global_intensity ~ 1, data = train_set_wkend,
                                   nstates = 10, ntimes = nrow(train_set_wkend))
train_wkend_univariate_4_line_4 <- fit(train_wkend_univariate_4)
print(train_wkend_univariate_4_line_4)
summary(train_wkend_univariate_4_line_4)

# Number of states: 13
train_wkend_univariate_5 <- depmix(response =Global_intensity ~ 1, data = train_set_wkend,
                                   nstates = 13, ntimes = nrow(train_set_wkend))
train_wkend_univariate_5_line_5 <- fit(train_wkend_univariate_5)
print(train_wkend_univariate_5_line_5)
summary(train_wkend_univariate_5_line_5)


train_wkend_univariate_6 <- depmix(response =Global_intensity ~ 1, data = train_set_wkend,
                                   nstates = 17, ntimes = nrow(train_set_wkend))
train_wkend_univariate_6_line_6 <- fit(train_wkend_univariate_6)
print(train_wkend_univariate_6_line_6)
summary(train_wkend_univariate_6_line_6)



#### --------- MULTIVARIATE TRAINING - WEEKDAYS

# Number of states: 10
train_wkday_multivariate_1 <- depmix(list(Global_intensity~1, Voltage~1), data = train_set_wkday,
       nstates = 10, family = list(gaussian(), gaussian()), ntimes = nrow(train_set_wkday))
train_wkday_multivariate_1_line_1 <- fit(train_wkday_multivariate_1)

print(train_wkday_multivariate_1_line_1)
summary(train_wkday_multivariate_1_line_1)


# Number of states: 13
train_wkday_multivariate_2 <- depmix(list(Global_intensity~1, Voltage~1), data = train_set_wkday,
                                     nstates = 13, family = list(gaussian(), gaussian()), ntimes = nrow(1074516))
train_wkday_multivariate_2_line_2 <- fit(train_wkday_multivariate_2)

print(train_wkday_multivariate_2_line_2)
summary(train_wkday_multivariate_2_line_2)


# Number of states: 17
train_wkday_multivariate_3 <- depmix(list(Global_intensity~1, Voltage~1), data = train_set_wkday,
                                     nstates = 17, family = list(gaussian(), gaussian()), ntimes = nrow(train_set_wkday))
train_wkday_multivariate_3_line_3 <- fit(train_wkday_multivariate_3)

print(train_wkday_multivariate_3_line_3)
summary(train_wkday_multivariate_3_line_3)



#### --------- MULTIVARIATE TRAINING - WEEKENDS

# Number of states: 10
train_wkend_multivariate_1 <- depmix(list(Global_intensity~1, Voltage~1), data = train_set_wkend,
                                     nstates = 10, family = list(gaussian(), gaussian()), ntimes = nrow(train_set_wkend))
train_wkend_multivariate_1_line_1 <- fit(train_wkend_multivariate_1)

print(train_wkend_multivariate_1_line_1)
summary(train_wkend_multivariate_1_line_1)


# Number of states: 13
train_wkend_multivariate_2 <- depmix(list(Global_intensity~1, Voltage~1), data = train_set_wkend,
                                     nstates = 14, family = list(gaussian(), gaussian()), ntimes = nrow(train_set_wkend))
train_wkend_multivariate_2_line_1 <- fit(train_wkend_multivariate_2)

print(train_wkend_multivariate_2_line_1)
summary(train_wkend_multivariate_2_line_1)


# Number of states: 17
train_wkend_multivariate_3 <- depmix(list(Global_intensity~1, Voltage~1), data = train_set_wkend,
                                     nstates = 17, family = list(gaussian(), gaussian()), ntimes = nrow(train_set_wkend))
train_wkend_multivariate_3_line_1 <- fit(train_wkend_multivariate_3)

print(train_wkend_multivariate_3_line_1)
summary(train_wkend_multivariate_3_line_1)

# Number of states: 19
train_wkend_multivariate_4 <- depmix(list(Global_intensity~1, Voltage~1), data = train_set_wkend,
                                     nstates = 19, family = list(gaussian(), gaussian()), ntimes = nrow(train_set_wkend))
train_wkend_multivariate_4_line_1 <- fit(train_wkend_multivariate_4)

print(train_wkend_multivariate_4_line_1)
summary(train_wkend_multivariate_4_line_1)



###### --------- TESTING UNIVARIATE WEEKDAYS

# UNIVARIATE WEEKDAY TESTING ON 17 STATES
test_17_wkday_univariate <- depmix(response=Global_intensity~1, data = test_set_wkday,
                        nstates = 17, ntimes = nrow(test_set_wkday))
test_17_wkday_univariate <- setpars(test_17_wkday_univariate, getpars(train_wkday_univariate_3_line_3))

logLik(test_17_wkday_univariate)

# normalize the liklehoods for 17
norm_train_17_wkday_univariate <- logLik(train_wkday_univariate_3_line_3) / nrow(train_set_wkday)
norm_train_17_wkday_univariate

norm_test_17_wkday_univariate <- logLik(test_17_wkday_univariate) / nrow(test_set_wkday)
norm_test_17_wkday_univariate

norm_train_17_wkday_univariate - norm_test_17_wkday_univariate



# MULTIVARIATE WEEKEND TESTING ON 17 STATES
test_17_wkend_multivariate <- depmix(list(Global_intensity~1, Voltage~1), data = test_set_wkend,
                                   nstates = 17, family = list(gaussian(), gaussian()), ntimes = nrow(test_set_wkend))
test_17_wkend_multivariate <- setpars(test_17_wkend_multivariate, getpars(train_wkend_multivariate_3_line_1))

logLik(test_17_wkend_multivariate)

# normalize the liklehoods for 17
norm_train_17_wkend_multivariate <- logLik(train_wkend_multivariate_3_line_1) / nrow(train_set_wkend)
norm_train_17_wkend_multivariate

norm_test_17_wkend_multivariate <- logLik(test_17_wkend_multivariate) / nrow(test_set_wkend)
norm_test_17_wkend_multivariate

norm_train_17_wkend_multivariate - norm_test_17_wkend_multivariate




# UNIVARIATE WEEKEND TESTING ON 17 STATES
test_17_wkend_univariate <- depmix(response=Global_intensity~1, data = test_set_wkend,
                                   nstates = 17, ntimes = nrow(test_set_wkend))
test_17_wkend_univariate <- setpars(test_17_wkend_univariate, getpars(train_wkend_univariate_6_line_6))

logLik(test_17_wkend_univariate)

# normalize the liklehoods for 17
norm_train_17_wkend_univariate <- logLik(train_wkend_univariate_6_line_6) / nrow(train_set_wkend)
norm_train_17_wkend_univariate

norm_test_17_wkend_univariate <- logLik(test_17_wkend_univariate) / nrow(test_set_wkend)
norm_test_17_wkend_univariate

norm_train_17_wkend_univariate - norm_test_17_wkend_univariate






# MULTIVARIATE WEEKDAY TESTING ON 17 STATES
test_17_wkday_multivariate <- depmix(list(Global_intensity~1, Voltage~1), data = test_set_wkday,
                                     nstates = 17, family = list(gaussian(), gaussian()), ntimes = nrow(test_set_wkday))
test_17_wkday_multivariate <- setpars(test_17_wkday_multivariate, getpars(train_wkday_multivariate_3_line_3))

logLik(test_17_wkday_multivariate)

# normalize the liklehoods for 17
norm_train_17_wkday_multivariate <- logLik(train_wkday_multivariate_3_line_3) / nrow(train_set_wkday)
norm_train_17_wkday_multivariate

norm_test_17_wkday_multivariate <- logLik(test_17_wkday_multivariate) / nrow(test_set_wkday)
norm_test_17_wkday_multivariate

norm_train_17_wkday_multivariate - norm_test_17_wkday_multivariate




anom_data_1 <- read.csv(file="Data1(WithAnomalies).txt", header=TRUE, sep=",", na.strings = c("", "NA"))
anom_data_1 <- na.exclude(anom_data_1)
anom_data_1$Date <- strptime(as.character(anom_data_1$Date), "%d/%m/%Y")
anom_data_1$Day <- as.POSIXlt(anom_data_1$Date)$wday

anom_data_wkdays_1 <- subset(anom_data_1, Day > 0 & Day < 6)
anom_data_wkdays_1 <- subset(anom_data_wkdays_1, (strptime(Time, format = "%H:%M:%S") >= strptime("18:00:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") < strptime("22:00:00", format = "%H:%M:%S")))

anom_data_wkends_1 <- subset(anom_data_1, Day == 0 | Day == 6)
anom_data_wkends_1 <- subset(anom_data_wkends_1, (strptime(Time, format = "%H:%M:%S") >= strptime("18:00:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") < strptime("22:00:00", format = "%H:%M:%S")))

test_17_wkday_anom_1 <- depmix(list(Global_intensity~1, Voltage~1), data = anom_data_wkdays_1,
                               nstates = 17, family = list(gaussian(), gaussian()), ntimes = nrow(anom_data_wkdays_1))
test_17_wkday_anom_1 <- setpars(test_17_wkday_anom_1, getpars(train_wkday_multivariate_3_line_3))
forwardbackward(test_17_wkday_anom_1)

norm1 <- (logLik(test_17_wkday_anom_1) / nrow(anom_data_wkdays_1))
(logLik(train_wkday_multivariate_3_line_3) / nrow(train_set_wkday)) - norm1
#Wkends

test_17_wkend_anom_1 <- depmix(list(Global_intensity~1, Voltage~1), data = anom_data_wkends_1,
                               nstates = 17, family = list(gaussian(), gaussian()), ntimes = nrow(anom_data_wkends_1))
test_17_wkend_anom_1 <- setpars(test_17_wkend_anom_1, getpars(train_wkend_multivariate_3_line_1))
forwardbackward(test_17_wkend_anom_1)
logLik(test_17_wkend_anom_1)

norm_train_17_wkend_anom_1 <- logLik(train_wkend_multivariate_3_line_1) / nrow(train_set_wkend)
norm_train_17_wkend_anom_1

norm_test_17_wkend_anom_1 <- logLik(test_17_wkend_anom_1) / nrow(anom_data_wkends_1)
norm_test_17_wkend_anom_1

norm_train_17_wkend_anom_1 - norm_test_17_wkend_anom_1




anom_data_2 <- read.csv(file="Data2(WithAnomalies).txt", header=TRUE, sep=",", na.strings = c("", "NA"))
write.csv(anom_data_wkdays_2, file = "anom_Data_2.csv")
anom_data_2$Global_reactive_power[is.na(anom_data_2$Global_reactive_power)] <- mean(anom_data_2$Global_reactive_power, na.rm = TRUE)
anom_data_2$Voltage[is.na(anom_data_2$Voltage)] <- mean(anom_data_2$Voltage, na.rm = TRUE)
anom_data_2$Global_intensity[is.na(anom_data_2$Global_intensity)] <- mean(anom_data_2$Global_intensity, na.rm = TRUE)
anom_data_2$Sub_metering_1[is.na(anom_data_2$Sub_metering_1)] <- mean(anom_data_2$Sub_metering_1, na.rm = TRUE)
anom_data_2$Sub_metering_2[is.na(anom_data_2$Sub_metering_2)] <- mean(anom_data_2$Sub_metering_2, na.rm = TRUE)
anom_data_2$Sub_metering_3[is.na(anom_data_2$Sub_metering_3)] <- mean(anom_data_2$Sub_metering_3, na.rm = TRUE)

anom_data_2$Date <- strptime(as.character(anom_data_2$Date), "%d/%m/%Y")
anom_data_2$Day <- as.POSIXlt(anom_data_2$Date)$wday

anom_data_wkdays_2 <- subset(anom_data_2, Day > 0 & Day < 6)
anom_data_wkdays_2 <- subset(anom_data_wkdays_2, (strptime(Time, format = "%H:%M:%S") >= strptime("18:00:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") < strptime("22:00:00", format = "%H:%M:%S")))

anom_data_wkends_2 <- subset(anom_data_2, Day == 0 | Day == 6)
anom_data_wkends_2 <- subset(anom_data_wkends_2, (strptime(Time, format = "%H:%M:%S") >= strptime("18:00:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") < strptime("22:00:00", format = "%H:%M:%S")))

test_17_wkday_anom_2 <- depmix(list(Global_intensity~1, Voltage~1), data = anom_data_wkdays_2,
                               nstates = 17, family = list(gaussian(), gaussian()), ntimes = nrow(anom_data_wkdays_2))
test_17_wkday_anom_2 <- setpars(test_17_wkday_anom_2, getpars(train_wkday_multivariate_3_line_3))
fb <- forwardbackward(test_17_wkday_anom_2)

fb$logLike
nrow(anom_data_wkdays_2)
tail(anom_data_wkdays_2)
head(anom_data_wkdays_2)

(logLik(train_wkday_multivariate_3_line_3) / nrow(train_set_wkday)) - (logLik(test_17_wkday_anom_2) / nrow(anom_data_wkdays_2))
anom_data_wkdays_2 <- na.exclude(anom_data_wkdays_2)
write.csv(anom_data_wkdays_2, file="anom_Data_wkdays_2.csv")

#Wkends

test_17_wkend_anom_2 <- depmix(list(Global_intensity~1, Voltage~1), data = anom_data_wkends_2,
                               nstates = 17, family = list(gaussian(), gaussian()), ntimes = nrow(anom_data_wkends_2))
test_17_wkend_anom_2 <- setpars(test_17_wkend_anom_2, getpars(train_wkend_multivariate_3_line_1))
forwardbackward(test_17_wkend_anom_2)
logLik(test_17_wkend_anom_2)

norm_train_17_wkend_anom_2 <- logLik(train_wkend_multivariate_3_line_1) / nrow(train_set_wkend)
norm_train_17_wkend_anom_2

norm_test_17_wkend_anom_2 <- logLik(test_17_wkend_anom_2) / nrow(anom_data_wkends_2)
norm_test_17_wkend_anom_2

norm_train_17_wkend_anom_2 - norm_test_17_wkend_anom_2




anom_data_3 <- read.csv(file="Data3(WithAnomalies).txt", header=TRUE, sep=",", na.strings = c("", "NA"))
anom_data_3 <- na.exclude(anom_data_3)
anom_data_3$Date <- strptime(as.character(anom_data_3$Date), "%d/%m/%Y")
anom_data_3$Day <- as.POSIXlt(anom_data_3$Date)$wday

anom_data_wkdays_3 <- subset(anom_data_3, Day > 0 & Day < 6)
anom_data_wkdays_3 <- subset(anom_data_wkdays_3, (strptime(Time, format = "%H:%M:%S") >= strptime("18:00:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") < strptime("22:00:00", format = "%H:%M:%S")))

anom_data_wkends_3 <- subset(anom_data_3, Day == 0 | Day == 6)
anom_data_wkends_3 <- subset(anom_data_wkends_3, (strptime(Time, format = "%H:%M:%S") >= strptime("18:00:00", format = "%H:%M:%S")) & (strptime(Time, format = "%H:%M:%S") < strptime("22:00:00", format = "%H:%M:%S")))

test_17_wkday_anom_3 <- depmix(list(Global_intensity~1, Voltage~1), data = anom_data_wkdays_3,
                               nstates = 17, family = list(gaussian(), gaussian()), ntimes = nrow(anom_data_wkdays_3))
test_17_wkday_anom_3 <- setpars(test_17_wkday_anom_3, getpars(train_wkday_multivariate_3_line_3))
forwardbackward(test_17_wkday_anom_3)

norm3 <- (logLik(test_17_wkday_anom_3) / nrow(anom_data_wkdays_3))
(logLik(train_wkday_multivariate_3_line_3) / nrow(train_set_wkday)) - rorm3

#Wkends

test_17_wkend_anom_3 <- depmix(list(Global_intensity~1, Voltage~1), data = anom_data_wkends_3,
                               nstates = 17, family = list(gaussian(), gaussian()), ntimes = nrow(anom_data_wkends_3))
test_17_wkend_anom_3 <- setpars(test_17_wkend_anom_3, getpars(train_wkend_multivariate_3_line_1))
forwardbackward(test_17_wkend_anom_3)
logLik(test_17_wkend_anom_3)

norm_train_17_wkend_anom_3 <- logLik(train_wkend_multivariate_3_line_1) / nrow(train_set_wkend)
norm_train_17_wkend_anom_3

norm_test_17_wkend_anom_3 <- logLik(test_17_wkend_anom_3) / nrow(anom_data_wkends_3)
norm_test_17_wkend_anom_3

norm_train_17_wkend_anom_3 - norm_test_17_wkend_anom_3



