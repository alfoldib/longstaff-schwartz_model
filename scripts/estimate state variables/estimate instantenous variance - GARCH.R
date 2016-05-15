# Script which determines the current levels of state variables, 
# - the short-term interest rate 
# - and the instantenous variance of the short-term interest rate

setwd("C:/Users/Boldi/Documents/Szakdoga")

rm(list=ls(all=TRUE))
gc()

# List of required packages
require(rugarch)
require(ggplot2)
require(reshape2)
require(scales)
require(forecast)
require(FinTS)
require(data.table)
require(ggplot2)
require(xts)
require(gridExtra)


load("./Data/hun_yields_20150927.rdata")

short_int_var <- "M3"

dat <- yields[tenor == short_int_var]

dat[, yield := yield / 100]
dat[, change := change / 100]
dat[, abs_change :=  abs(change)]
dat[, sq_change :=  (change)^2]

# Function that gets the data from the result of generic acf() function
get_plot_parts_acf <- function(x, ci, type_name){
  clim <- qnorm((1 + ci)/2)/sqrt(x$n.used)
  acf <- c(x$acf)
  lag <- c(x$lag)
  
  res <- data.frame(type = type_name, lag = c(lag, length(lag) + 1), 
                    acf = c(NA, acf[-1], NA), 
                    clim = clim)
  
  return(res)
}


# Alapadat ábrázolás

to_plot <- melt(dat[!is.na(change), list(date, yield, change, abs_change, sq_change)], id=1)

levels(to_plot$variable) <- c("Hozam", "Megváltozás",
                                "Abs(Megváltozás)", "Megváltozás^2")

ggplot(data = to_plot[variable != "Hozam",], 
       aes(x = date, y = value)) + 
  geom_line() +
  scale_y_continuous(labels = percent) +
  facet_wrap( ~ variable, scales = "free_y") + 
  labs(list(title = "Volatility clustering in 3M hungarian government bond yieds\n",
            x = "\nDate", y = NULL)) + 
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

plot(as.Date(dat$date, format = "%Y.%m.%d"), dat$yield, type = "l", 
     main = "A 3 hónapos magyar állampapírhozam alakulása",
     xlab = "\nDátum", ylab = "")

ggplot(data = to_plot[variable == "Hozam" | 
                          variable == "Megváltozás" ,], 
       aes(x = date, y = value, fill = variable)) +
  geom_line(aes(linetype = variable)) +
  scale_linetype_manual(values=c(2, 1)) + 
  scale_y_continuous(labels = percent) +
  labs(list(title = "A 3 hónapos magyar állampapírhozam alakulása\n",
            x = "\nDátum", y = NULL)) + 
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

ggplot(data = to_plot[variable == "Megváltozás",],
       aes(x = value)) +
  geom_density()

ggplot(data = to_plot[variable == "Hozam",],
       aes(x = value)) +
  geom_histogram()


###############################################################################
#
# GARCH Modell illesztése - azonnali variancia idõsorának meghatározása
#
###############################################################################


## Napi adatokon

# Autokorreláció és ARCH hatások tesztelése
dat <- dat[order(date)]

int_rate <- xts(log(1+dat$yield), dat$date)
colnames(int_rate) <- "yield"

# Differenciálás
int_change <- diff(int_rate)[-1]
colnames(int_change) <- "int_change"

# Kamatláb adatok lag-elése a differenciákhoz képest
int_rate <- int_rate[-1]

# Korrelogram ábra
correlogram_data <- get_plot_parts_acf(acf(int_change, lag.max = 15, plot = F), 0.95, "Megváltozás")
correlogram_data <- rbind(correlogram_data,
                          get_plot_parts_acf(acf(int_change^2, lag.max = 15, plot = F), 0.95, "Megváltozás^2"))

ggplot(correlogram_data, aes(x = lag, y = acf)) + 
  geom_bar(stat = "identity", width = .3) + 
  geom_ribbon(aes(ymin = -clim, ymax = clim), alpha = .2) + 
  facet_wrap(~ type, nrow = 1) + 
  labs(title = "A hozammegváltozásnak és négyzetének korrelogramja \n", 
       y = "Mintabeli autokorreláció \n", x = "\n Késleltetés") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

Box.test(int_change, type="Ljung-Box")
ArchTest(int_change^2, lags = 12)

# Null hipotézis elutasítva -> ARCH hatásra utal

# Illesztünk egy idõsoros modellt (ARIMA modellt). Majd megvizsgáljuk 
# az illesztést követõen a reziduálisokat. Amennyiben az azokon elvégzett 
# Engle-LM (ARCH) tesztnél is elutasítjuk a Null hipotézist, 
# akkor helyes megközelítés a volatilitás GARCH modellel történõ becslése.

# Várható érték egyenlet meghatározása (mean model)
# Legjobban illeszkedõ ARIMA modell illesztése
arima_int_rate <- auto.arima(int_change, xreg = int_rate)
summary(arima_int_rate)

# Reziduumok elmentése új változóba
res <- arima_int_rate$residuals

# Reziduumok tesztelése - autokorreláció és ARCH hatás
# Korrelogram ábra

correlogram_data <- get_plot_parts_acf(acf(res, lag.max = 15, plot = F), 0.95, "Hibatag")
correlogram_data <- rbind(correlogram_data,
                          get_plot_parts_acf(acf(res^2, lag.max = 15, plot = F), 0.95, "Hibatag^2"))

ggplot(correlogram_data, aes(x = lag, y = acf)) + 
  geom_bar(stat = "identity", width = .3) + 
  geom_ribbon(aes(ymin = -clim, ymax = clim), alpha = .2) + 
  facet_wrap(~ type, nrow = 1) + 
  labs(title = "Várható érték modell hibatagjainak korrelogramja \n", 
       y = "Mintabeli autokorreláció \n", x = "\n Késleltetés") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

ArchTest(res^2, lags = 1)


# Null hipotézist elutasítjuk, van ARCH hatás


# GARCH(1, 1) modell illesztése a napi változásokra
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                               garchOrder = c(1, 1), 
                                               external.regressors = int_rate),
                         mean.model = list(armaOrder = c(0, 0), 
                                           include.mean = T,
                                           archm = T,
                                           archpow = 1, 
                                           external.regressors = int_rate), 
                         distribution.model = "norm")

garch_fit  <- ugarchfit(garch_spec, int_change)


# Showing fit summary
garch_fit
m <- round(garch_fit@fit$matcoef, 6)

# Ábrázoláshoz szükséges adatok összegyûjtése
fit_res <- data.frame(dt = "daily", name = "yield change", short_name = "change",
                      date = as.Date(index(int_change)), value = as.numeric(int_change))
fit_res <- rbind(fit_res,
                 data.frame(dt = "daily", name = "yield", short_name = "yield",
                            date = as.Date(index(int_rate)), value = as.numeric(int_rate)))
fit_res <- rbind(fit_res,
                 data.frame(dt = "daily", name = "conditional volatility", short_name = "vol",
                            date = as.Date(index(sigma(garch_fit))), value = sigma(garch_fit)))
fit_res <- rbind(fit_res,
                 data.frame(dt = "daily", name = "conditional variance", short_name ="var",
                            date = as.Date(index(fitted(garch_fit))), value = garch_fit@fit$var))
fit_res <- rbind(fit_res,
                 data.frame(dt = "daily", name = "fitted values", short_name = "fit",
                            date = as.Date(index(fitted(garch_fit))), value = fitted(garch_fit)))
fit_res <- rbind(fit_res,
                 data.frame(dt = "daily", name = "residuals", short_name = "res",
                            date = as.Date(index(residuals(garch_fit))), value = residuals(garch_fit)))

#plot(garch_fit)

res <- as.numeric(residuals(garch_fit, standardize = TRUE))

correlogram_data <- get_plot_parts_acf(acf(res^2, lag.max = 15, plot = F), 0.95, "Hibatag^2")

correlogram <- ggplot(correlogram_data, aes(x = lag, y = acf)) + 
  geom_bar(stat = "identity", width = .3) + 
  geom_ribbon(aes(ymin = -clim, ymax = clim), alpha = .1) +
  labs(title = "Sztenderdizált hibatagok korrelogramja \n", 
       y = "Mintabeli autokorreláció \n", x = "\n Késleltetés") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))


empirical_density <- qplot(res[abs(res) < 5], geom = "blank") + 
  geom_histogram(aes(y = ..density.., fill = "bla"), binwidth = .5, col = "black") + 
  stat_function(fun=dnorm, aes(x = res[abs(res) < 5], fill = "blabla"), 
                col = "black", linetype = 2, size = 1) + 
  scale_fill_manual(name = "", values = c("grey", "black"), 
                      breaks = c("bla", "blabla"), 
                      labels = c("empirikus eloszlás", "normál eloszlás")) +
  labs(title = "Sztenderdizált hibatagok eloszlása \n", 
       y = "", x = "") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

grid.arrange(correlogram, empirical_density, ncol = 2)





rownames(fit_res) <- NULL
fit_res <- data.table(fit_res)

# Feltételes azonnali szórás ábrázolása
ggplot(fit_res[short_name %in% c("change", "vol")], 
       aes(x = date, y = abs(value), col = name)) + 
  scale_colour_manual(name = "", values = c("grey", "black"), 
                      breaks = c("change", "vol"), 
                      labels = c("Megváltozás", "Feltételes szórás")) +
  geom_line() + scale_y_continuous(labels = percent) +
  labs(title = "Feltételes szórás ábrázolása a megváltozás ellenében \n", 
       x = "Dátum", y = "%") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

theme(text = element_text(size=20),
      axis.text.x = element_text(angle=90, vjust=1)) 


# Feltételes azonnali variancia ábrázolása
ggplot(fit_res[short_name %in% c("fit")], 
       aes(x = date, y = value, col = name)) + 
  geom_line() + scale_colour_manual(values = c("black"))

# Feltételes azonnali variancia ábrázolása
ggplot(fit_res[short_name %in% c("yield", "change", "vol")], 
       aes(x = date, y = abs(value), col = name)) + 
  geom_line() + scale_colour_manual(values = c("grey", "darkgrey", "black")) + 
  scale_linetype_manual(values = c(1, 1, 1))

# Feltételes azonnali variancia ábrázolása
ggplot(fit_res[short_name %in% c("res")], 
       aes(x = date, y = abs(value), col = name)) + 
  geom_line() + scale_colour_manual(values = c("black"))

state_data <- fit_res
save(state_data, file = "./Data/state_vars_data.rData")

###############################################################################


## Heti adatokon

# Autokorreláció és ARCH hatások tesztelése
dat <- dat[order(date)]

int_rate <- xts(dat$yield, dat$date)
colnames(int_rate) <- "yield"

# Heti gyakoriságú hozamok
int_rate <- to.weekly(int_rate, OHLC = F)

# Differenciálás
int_change <- diff(int_rate)[-1]
colnames(int_change) <- "int_change"

int_rate <- int_rate[-1]

plot(int_change)

auto.corr.function <- acf(int_change, lag.max = 15)
partial.acf        <- pacf(int_change, lag.max = 15)
Box.test(int_change^2, type="Ljung-Box", lag = 12)
ArchTest(int_change^2, lags = 12)

# Null hipotézis elfogadva, nincs ARCH hatás
# Ettõl függetlenül folytatom az illesztést

# Illesztünk egy idõsoros modellt (ARIMA modellt). Majd megvizsgáljuk 
# az illesztést követõen a reziduálisokat. Amennyiben az azokon elvégzett 
# Engle-LM (ARCH) tesztnél is elutasítjuk a Null hipotézist, 
# akkor helyes megközelítés a volatilitás GARCH modellel történõ becslése.

# Várható érték egyenlet meghatározása (mean model)
# Legjobban illeszkedõ ARIMA modell illesztése
arima_int_rate <- auto.arima(int_change, xreg = int_rate)
summary(arima_int_rate)

# Augmented Dickey-Fuller teszt egységgyök meglétére - nem szükséges
adf.test(int_change, k = 0)

# Reziduumok elmentése új változóba
res <- arima_int_rate$residuals

# Reziduumok tesztelése - autokorreláció és ARCH hatás
plot(res)
plot(res^2)
acf(res^2)
pacf(res^2)
ArchTest(res^2, lags = 12)

# Null hipotézist ismét elfogadjuk, p = 1, 
# nincs ARCH hatás a heti hozamváltozásokban
# Ettõl függetlenül illesztek egy GARCH(1, 1)-et


# GARCH(1, 1) modell illesztése a heti változásokra
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                               garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), 
                                           include.mean = T,
                                           archm = T,
                                           archpow = 1, 
                                           external.regressors = int_rate), 
                         distribution.model = "norm")

garch_fit  <- ugarchfit(garch_spec, int_change)


# Összegzõ, tesztek bemutatása
garch_fit

# Ábrázoláshoz szükséges adatok összegyûjtése
fit_res <- data.frame(dt = "daily", type = "interest change", 
                      date = as.Date(index(int_change)), value = as.numeric(int_change))
fit_res <- rbind(fit_res,
                 data.frame(dt = "daily", type = "conditional volatility", 
                            date = as.Date(index(sigma(garch_fit))), value = sigma(garch_fit)))
fit_res <- rbind(fit_res,
                 data.frame(dt = "daily", type = "conditional variance", 
                            date = as.Date(index(fitted(garch_fit))), value = garch_fit@fit$var))
fit_res <- rbind(fit_res,
                 data.frame(dt = "daily", type = "fitted values", 
                            date = as.Date(index(fitted(garch_fit))), value = fitted(garch_fit)))
fit_res <- rbind(fit_res,
                 data.frame(dt = "daily", type = "residuals", 
                            date = as.Date(index(residuals(garch_fit))), value = residuals(garch_fit)))

rownames(fit_res) <- NULL
fit_res <- data.table(fit_res)

# Feltételes azonnali szórás ábrázolása
ggplot(fit_res[type %in% c("interest change", "conditional volatility")], 
       aes(x = date, y = abs(value), col = type)) + 
  geom_line() + scale_colour_manual(values = c("grey", "black"))

# Szintjében nincs változás!