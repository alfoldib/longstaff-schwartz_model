# 

rm(list=ls(all=TRUE))
gc()

require(data.table)
require(ggplot2)
require(mondate)
require(gridExtra)
require(scales)

setwd("C:/Users/Boldi/Documents/Szakdoga")
source("./Scripts/load data.R")


# Plot yields
yields[, tenor := factor(tenor, levels = unique(tenor))]
yields <- merge(yields, yields[tenor == "M3", list(date, short_yield = yield)],
                by = "date", all.x = T)
yields[, diff_to_short := yield - short_yield]
yields[, log_yield := log(1 + yield / 100) * 100]
yields <- yields[order(tenor, date)]

yields <- yields[date <= as.Date("2015-06-30")]

ggplot(yields[tenor %in% c("O/N", "M3")], aes(x = date, y = change, col = tenor)) + geom_line()



ggplot(yields[tenor %in% c( "M3", "O/N")], aes(x = date, y = yield / 100, fill = tenor)) +
  geom_line(aes(linetype = tenor)) +
  scale_linetype_manual(values = c(2, 1)) + 
  scale_y_continuous(labels = percent) +
  labs(title = "Bankközi kamatláb és a három hónapos állampapírhozam összehasonlítása \n (2002-tõl 2015 félévéig) \n", 
       y = "Hozam \n", x = "\n Kereskedési nap") +
  scale_x_date(minor_breaks = "years") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))


lvl <- ggplot(yields[tenor %in% c("M3", "Y5", "Y15")], aes(x = date, y = yield / 100, fill = tenor)) +
  geom_line(aes(linetype = tenor)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Állampapírpiaci hozamszintek \n (2002-tõl 2015 félévéig) \n", 
       y = "Hozam \n", x = "\n Kereskedési nap") +
  scale_x_date(minor_breaks = "years") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

dif <- ggplot(yields[tenor %in% c("M3", "Y5", "Y15")], aes(x = date, y = diff_to_short / 100, fill = tenor)) +
  geom_line(aes(linetype = tenor)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Állampapírpiaci hozamszintek különbsége \n (2002-tõl 2015 félévéig) \n", 
       y = "Hozamkülönbség \n", x = "\n Kereskedési nap") +
  scale_x_date(minor_breaks = "years") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

grid.arrange(lvl, dif, ncol = 1)

ggplot(yields, aes(x = date, y = diff_to_short, col = tenor)) + geom_line()
ggplot(yields[tenor %in% c("O/N", "M3")], aes(x = date, y = yield, col = tenor)) + geom_line()

ggplot(yields[tenor %in% c("O/N", "M3")], aes(x = date, y = yield, col = "effective")) + geom_line() + 
  geom_line(aes(x = date, y = log_yield, col = "log")) + facet_wrap(~ tenor)

# Plot cash-flow
cf_data[, months_to_mat := round(as.numeric(mondate(maturity_date) - mondate(duedate))/12, 4)]

ggplot(cf_data[year(duedate)>= 2002], 
       aes(x = duedate, y = months_to_mat, group = alias)) + geom_line(col = "black", alpha = .7) +
  labs(title = "Vizsgált államkötvény pénzáramlások \n (2002-tõl 2015 félévéig) \n", 
       y = "Lejáratig hátralévõ évek\n", x = "\n Esedékesség dátuma") +
  scale_x_date(minor_breaks = "years") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

# ggplot(cf_data, aes(x = months_to_mat, y = issue_date, group = alias)) + geom_line()


# Plot price data
best_price <- merge(best_price, cf_data[!duplicated(alias), list(alias, interest_type)], 
                    by = "alias", all.x = T)

data_per_date <- best_price[interest_type != "FLOAT", .N, by = date]
data_per_date <- data_per_date[order(date)]

ggplot(data_per_date, aes(x = date, y = N)) + geom_area(alpha = .7) +
  labs(title = "Rendelkezésre álló piaci árak \n (2003-tõl 2015 félévéig) \n", 
       y = "Piaci árak darabszáma \n", x = "\n Kereskedési nap") +
  scale_x_date(minor_breaks = "years") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))
