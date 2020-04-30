data <- read.csv(url("https://covid.ourworldindata.org/data/owid-covid-data.csv"))
summary(data)
head(data)
summary(data$iso_code)
aggregate(data$total_deaths, by = list(data$iso_code), max)

# let's look at US data only
us_data <- subset(data, iso_code == "USA")
head(us_data)
# let's plot the growth of totak cases daily
plot(as.Date(us_data$date), us_data$total_cases)
# zoom in on post march 2020
plot(as.Date(us_data$date),us_data$total_deaths,xlab="Date",ylab="US total deaths", type="l", xlim=c(as.Date("2020-03-01"),as.Date("2020-05-01")), log="y")
plot(as.Date(us_data$date),us_data$total_deaths,xlab="Date",ylab="US total deaths", type="l", xlim=c(as.Date("2020-03-01"),as.Date("2020-05-01")))
# let's compare some countries
library(ggplot2)
comp_data <- subset(data, iso_code %in% c("USA" , "DEU" , "FRA" , "ITA", "ESP", "CHN", "GBR", "IRN"))
ggplot(comp_data, aes(x = as.Date(date), y = total_deaths, col=iso_code)) + 
  geom_line(aes(color = iso_code)) +
  xlim(as.Date("2020-01-01"),as.Date("2020-05-01")) +
  scale_y_log10()

ggplot(comp_data, aes(x = as.Date(date), y = total_cases, col=iso_code)) + 
  geom_line(aes(color = iso_code)) +
  xlim(as.Date("2020-01-01"),as.Date("2020-05-01")) +
  scale_y_log10()

# linear representation
ggplot(comp_data, aes(x = as.Date(date), y = total_deaths, col=iso_code)) + 
  geom_line(aes(color = iso_code)) +
  xlim(as.Date("2020-01-01"),as.Date("2020-05-01"))

ggplot(comp_data, aes(x = as.Date(date), y = total_cases, col=iso_code)) + 
  geom_line(aes(color = iso_code)) +
  xlim(as.Date("2020-01-01"),as.Date("2020-05-01"))

# Let's normalize to 100th case
data_1k <- subset(data, total_cases>=1000)
df <- aggregate(as.Date(data_1k$date), by = list(data_1k$iso_code), min)
colnames(df)
colnames(df) <- c("iso_code", "min_date")
df_n <- merge(x = df, 
      y = data_1k,
      by = "iso_code",
      all = FALSE )
# compute days since 100th case
df_n$days_since_1000 <- difftime(as.Date(df_n$date), as.Date(df_n$min_date),  units = c("days"))
head(df_n)

# isolate some countries
data_countries <- subset(df_n, iso_code %in% c("USA" , "DEU" , "FRA" , "ITA", "ESP", "CHN", "GBR", "IRN"))

ggplot(data_countries, aes(x = days_since_1000, y = total_deaths, col=iso_code)) + 
  geom_line(aes(color = iso_code))  +
  scale_y_log10()