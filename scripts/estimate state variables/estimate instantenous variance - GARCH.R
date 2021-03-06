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


# Alapadat �br�zol�s

to_plot <- melt(dat[!is.na(change), list(date, yield, change, abs_change, sq_change)], id=1)

levels(to_plot$variable) <- c("Hozam", "Megv�ltoz�s",
                                "Abs(Megv�ltoz�s)", "Megv�ltoz�s^2")

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
     main = "A 3 h�napos magyar �llampap�rhozam alakul�sa",
     xlab = "\nD�tum", ylab = "")

ggplot(data = to_plot[variable == "Hozam" | 
                          variable == "Megv�ltoz�s" ,], 
       aes(x = date, y = value, fill = variable)) +
  geom_line(aes(linetype = variable)) +
  scale_linetype_manual(values=c(2, 1)) + 
  scale_y_continuous(labels = percent) +
  labs(list(title = "A 3 h�napos magyar �llampap�rhozam alakul�sa\n",
            x = "\nD�tum", y = NULL)) + 
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

ggplot(data = to_plot[variable == "Megv�ltoz�s",],
       aes(x = value)) +
  geom_density()

ggplot(data = to_plot[variable == "Hozam",],
       aes(x = value)) +
  geom_histogram()


###############################################################################
#
# GARCH Modell illeszt�se - azonnali variancia id�sor�nak meghat�roz�sa
#
###############################################################################


## Napi adatokon

# Autokorrel�ci� �s ARCH hat�sok tesztel�se
dat <- dat[order(date)]

int_rate <- xts(log(1+dat$yield), dat$date)
colnames(int_rate) <- "yield"

# Differenci�l�s
int_change <- diff(int_rate)[-1]
colnames(int_change) <- "int_change"

# Kamatl�b adatok lag-el�se a differenci�khoz k�pest
int_rate <- int_rate[-1]

# Korrelogram �bra
correlogram_data <- get_plot_parts_acf(acf(int_change, lag.max = 15, plot = F), 0.95, "Megv�ltoz�s")
correlogram_data <- rbind(correlogram_data,
                          get_plot_parts_acf(acf(int_change^2, lag.max = 15, plot = F), 0.95, "Megv�ltoz�s^2"))

ggplot(correlogram_data, aes(x = lag, y = acf)) + 
  geom_bar(stat = "identity", width = .3) + 
  geom_ribbon(aes(ymin = -clim, ymax = clim), alpha = .2) + 
  facet_wrap(~ type, nrow = 1) + 
  labs(title = "A hozammegv�ltoz�snak �s n�gyzet�nek korrelogramja \n", 
       y = "Mintabeli autokorrel�ci� \n", x = "\n K�sleltet�s") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

Box.test(int_change, type="Ljung-Box")
ArchTest(int_change^2, lags = 12)

# Null hipot�zis elutas�tva -> ARCH hat�sra utal

# Illeszt�nk egy id�soros modellt (ARIMA modellt). Majd megvizsg�ljuk 
# az illeszt�st k�vet�en a rezidu�lisokat. Amennyiben az azokon elv�gzett 
# Engle-LM (ARCH) tesztn�l is elutas�tjuk a Null hipot�zist, 
# akkor helyes megk�zel�t�s a volatilit�s GARCH modellel t�rt�n� becsl�se.

# V�rhat� �rt�k egyenlet meghat�roz�sa (mean model)
# Legjobban illeszked� ARIMA modell illeszt�se
arima_int_rate <- auto.arima(int_change, xreg = int_rate)
summary(arima_int_rate)

# Reziduumok elment�se �j v�ltoz�ba
res <- arima_int_rate$residuals

# Reziduumok tesztel�se - autokorrel�ci� �s ARCH hat�s
# Korrelogram �bra

correlogram_data <- get_plot_parts_acf(acf(res, lag.max = 15, plot = F), 0.95, "Hibatag")
correlogram_data <- rbind(correlogram_data,
                          get_plot_parts_acf(acf(res^2, lag.max = 15, plot = F), 0.95, "Hibatag^2"))

ggplot(correlogram_data, aes(x = lag, y = acf)) + 
  geom_bar(stat = "identity", width = .3) + 
  geom_ribbon(aes(ymin = -clim, ymax = clim), alpha = .2) + 
  facet_wrap(~ type, nrow = 1) + 
  labs(title = "V�rhat� �rt�k modell hibatagjainak korrelogramja \n", 
       y = "Mintabeli autokorrel�ci� \n", x = "\n K�sleltet�s") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

ArchTest(res^2, lags = 1)


# Null hipot�zist elutas�tjuk, van ARCH hat�s


# GARCH(1, 1) modell illeszt�se a napi v�ltoz�sokra
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

# �br�zol�shoz sz�ks�ges adatok �sszegy�jt�se
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
  labs(title = "Sztenderdiz�lt hibatagok korrelogramja \n", 
       y = "Mintabeli autokorrel�ci� \n", x = "\n K�sleltet�s") +
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
                      labels = c("empirikus eloszl�s", "norm�l eloszl�s")) +
  labs(title = "Sztenderdiz�lt hibatagok eloszl�sa \n", 
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

# Felt�teles azonnali sz�r�s �br�zol�sa
ggplot(fit_res[short_name %in% c("change", "vol")], 
       aes(x = date, y = abs(value), col = name)) + 
  scale_colour_manual(name = "", values = c("grey", "black"), 
                      breaks = c("change", "vol"), 
                      labels = c("Megv�ltoz�s", "Felt�teles sz�r�s")) +
  geom_line() + scale_y_continuous(labels = percent) +
  labs(title = "Felt�teles sz�r�s �br�zol�sa a megv�ltoz�s ellen�ben \n", 
       x = "D�tum", y = "%") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

theme(text = element_text(size=20),
      axis.text.x = element_text(angle=90, vjust=1)) 


# Felt�teles azonnali variancia �br�zol�sa
ggplot(fit_res[short_name %in% c("fit")], 
       aes(x = date, y = value, col = name)) + 
  geom_line() + scale_colour_manual(values = c("black"))

# Felt�teles azonnali variancia �br�zol�sa
ggplot(fit_res[short_name %in% c("yield", "change", "vol")], 
       aes(x = date, y = abs(value), col = name)) + 
  geom_line() + scale_colour_manual(values = c("grey", "darkgrey", "black")) + 
  scale_linetype_manual(values = c(1, 1, 1))

# Felt�teles azonnali variancia �br�zol�sa
ggplot(fit_res[short_name %in% c("res")], 
       aes(x = date, y = abs(value), col = name)) + 
  geom_line() + scale_colour_manual(values = c("black"))

state_data <- fit_res
save(state_data, file = "./Data/state_vars_data.rData")

###############################################################################


## Heti adatokon

# Autokorrel�ci� �s ARCH hat�sok tesztel�se
dat <- dat[order(date)]

int_rate <- xts(dat$yield, dat$date)
colnames(int_rate) <- "yield"

# Heti gyakoris�g� hozamok
int_rate <- to.weekly(int_rate, OHLC = F)

# Differenci�l�s
int_change <- diff(int_rate)[-1]
colnames(int_change) <- "int_change"

int_rate <- int_rate[-1]

plot(int_change)

auto.corr.function <- acf(int_change, lag.max = 15)
partial.acf        <- pacf(int_change, lag.max = 15)
Box.test(int_change^2, type="Ljung-Box", lag = 12)
ArchTest(int_change^2, lags = 12)

# Null hipot�zis elfogadva, nincs ARCH hat�s
# Ett�l f�ggetlen�l folytatom az illeszt�st

# Illeszt�nk egy id�soros modellt (ARIMA modellt). Majd megvizsg�ljuk 
# az illeszt�st k�vet�en a rezidu�lisokat. Amennyiben az azokon elv�gzett 
# Engle-LM (ARCH) tesztn�l is elutas�tjuk a Null hipot�zist, 
# akkor helyes megk�zel�t�s a volatilit�s GARCH modellel t�rt�n� becsl�se.

# V�rhat� �rt�k egyenlet meghat�roz�sa (mean model)
# Legjobban illeszked� ARIMA modell illeszt�se
arima_int_rate <- auto.arima(int_change, xreg = int_rate)
summary(arima_int_rate)

# Augmented Dickey-Fuller teszt egys�ggy�k megl�t�re - nem sz�ks�ges
adf.test(int_change, k = 0)

# Reziduumok elment�se �j v�ltoz�ba
res <- arima_int_rate$residuals

# Reziduumok tesztel�se - autokorrel�ci� �s ARCH hat�s
plot(res)
plot(res^2)
acf(res^2)
pacf(res^2)
ArchTest(res^2, lags = 12)

# Null hipot�zist ism�t elfogadjuk, p = 1, 
# nincs ARCH hat�s a heti hozamv�ltoz�sokban
# Ett�l f�ggetlen�l illesztek egy GARCH(1, 1)-et


# GARCH(1, 1) modell illeszt�se a heti v�ltoz�sokra
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                               garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), 
                                           include.mean = T,
                                           archm = T,
                                           archpow = 1, 
                                           external.regressors = int_rate), 
                         distribution.model = "norm")

garch_fit  <- ugarchfit(garch_spec, int_change)


# �sszegz�, tesztek bemutat�sa
garch_fit

# �br�zol�shoz sz�ks�ges adatok �sszegy�jt�se
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

# Felt�teles azonnali sz�r�s �br�zol�sa
ggplot(fit_res[type %in% c("interest change", "conditional volatility")], 
       aes(x = date, y = abs(value), col = type)) + 
  geom_line() + scale_colour_manual(values = c("grey", "black"))

# Szintj�ben nincs v�ltoz�s!