## Plotting, looking into your data helps exploring every aspect of it
## This script further prepare the dataset and plots important aspects of it

rm(list=ls(all=TRUE))
gc()

require(data.table)
require(ggplot2)
require(mondate)
require(gridExtra)
require(scales)

# Set working directory and load data
setwd("C:/Users/Boldi/Documents/Publish/Interest rate models/longstaff-schwartz_model")
source("./Scripts/load data.R")


# Prepare the data for plotting yields
yields[, tenor := factor(tenor, levels = unique(tenor))]
yields <- merge(yields, yields[tenor == "M3", list(date, short_yield = yield)],
                by = "date", all.x = T)


# Creating new variables:
# 1) diff_to_short - every yields difference to the short yield (3M gov yield)
# 2) log_yield - transform every yield to the freqently used log scale
yields[, diff_to_short := yield - short_yield]
yields[, log_yield := log(1 + yield / 100) * 100]
yields <- yields[order(tenor, date)]

# Optional - cutting the end of the yield dataset 
# as I don't have to corresponding cash-flow and price data
yields <- yields[date <= as.Date("2015-06-30")]


# Plot of the overnight interest rate versus the short government yield
ggplot(yields[tenor %in% c( "M3", "O/N")], aes(x = date, y = yield / 100, fill = tenor)) +
  geom_line(aes(linetype = tenor)) +
  scale_linetype_manual(values = c(2, 1)) + 
  scale_y_continuous(labels = percent) +
  labs(title = "The O/N interest rate vs the 3M government bond yield \n (from 2002 to mid 2015) \n", 
       y = "Yield \n", x = "\n Trading days") +
  scale_x_date(minor_breaks = date_breaks("1 year")) +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))


# Plot of the 3 months, 5 years and 15 years government bond yields (as short, medium, long term yields)
lvl <- ggplot(yields[tenor %in% c("M3", "Y5", "Y15")], aes(x = date, y = yield / 100, fill = tenor)) +
  geom_line(aes(linetype = tenor)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Yield levels in government bond yields \n (from 2002 to mid 2015) \n", 
       y = "Yield \n", x = "\n Trading days") +
  scale_x_date(minor_breaks = date_breaks("1 year")) +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 12, face = "bold"))

# Plot of the difference of the short yields to the medium and long term yields
dif <- ggplot(yields[tenor %in% c("M3", "Y5", "Y15")], aes(x = date, y = diff_to_short / 100, fill = tenor)) +
  geom_line(aes(linetype = tenor)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Difference of government bond yields \n (from 2002 to mid 2015) \n", 
       y = "Yield difference \n", x = "\n Trading days") +
  scale_x_date(minor_breaks = date_breaks("1 year")) +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 12, face = "bold"))

# Plotting the previous two on the same graph
grid.arrange(lvl, dif, ncol = 1)

# Plotting every yields difference, not just the medium and long
ggplot(yields, aes(x = date, y = diff_to_short, col = tenor)) + geom_line()

# Plot about log versus effective yields
ggplot(yields[tenor %in% c("O/N", "M3")], aes(x = date, y = yield, col = "effective")) + geom_line() + 
  geom_line(aes(x = date, y = log_yield, col = "log")) + facet_wrap(~ tenor)


# Plot cash-flow data
# Create new variable, months_to_mat - months to maturity of a certain cash-flow
cf_data[, months_to_mat := round(as.numeric(mondate(maturity_date) - mondate(duedate))/12, 4)]

ggplot(cf_data[year(duedate)>= 2002], 
       aes(x = duedate, y = months_to_mat, group = alias)) + geom_line(col = "black", alpha = .7) +
  labs(title = "Government bond cash-flows \n (from 2002 to mid 2015) \n", 
       y = "Years untill maturity \n", x = "\n Due date") +
  scale_x_date(minor_breaks = date_breaks("1 year")) +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))


# Plot market price data
best_price <- merge(best_price, cf_data[!duplicated(alias), list(alias, interest_type)], 
                    by = "alias", all.x = T)

# Aggregate the market price data in order to visualize the number of observations
data_per_date <- best_price[interest_type != "FLOAT", .N, by = date]
data_per_date <- data_per_date[order(date)]

ggplot(data_per_date, aes(x = date, y = N)) + geom_area(alpha = .7) +
  labs(title = "Available market prices \n (from 2002 to mid 2015) \n", 
       y = "Count of market prices \n", x = "\n Trading days") +
  scale_x_date(minor_breaks = date_breaks("1 year")) +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))
