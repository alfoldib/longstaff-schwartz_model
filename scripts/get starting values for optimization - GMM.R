# Modell keresztmetszeti korlátozásainak tesztelés- GMM

# install.packages('C:/Users/Boldi/Documents/R/win-library/3.2/gmm_mod', repos = NULL, type = 'source')

rm(list=ls(all=TRUE))
gc()

require(data.table)
require(reshape2)
require(xts)
require(ggplot2)
require(gmm)
require(mondate)
require(scales)

setwd("C:/Users/Boldi/Documents/Szakdoga")

source("./Scripts/load data.R")
source("./Functions/Longstaff - Schwarcz functions.R")


g <- function(theta, x) {
  
  # theta[1] = alpha
  # theta[2] = beta
  # theta[3] = gamma
  # theta[4] = delta
  # theta[5] = eta
  # theta[6] = nu
  
  b <- -C(taus, theta[1], theta[2], theta[4], theta[6])/taus
  c <- -D(taus, theta[1], theta[2], theta[4], theta[6])/taus
  
  a <- -(kappa(theta[1], theta[2], theta[3], theta[4], theta[5], theta[6])*taus + 
           2*theta[3]*log(A(taus, theta[1], theta[4])) + 
           2*theta[5]*log(B(taus, theta[2], theta[6])))/taus
  
  b_dr <- t(b %*% t(x[, 1]))
  c_dV <- t(c %*% t(x[, 2]))
  
  eps <- as.matrix(x[, 3:ncol(x)]) - a - b_dr - c_dV
  
  g_mat <- cbind(eps, eps*x[, 1], eps*x[, 2])
  names(g_mat) <- paste(rep(c("sum_eps", "sum_eps_r", "sum_eps_V"), each = 4), 
                        rep(names(eps),3), sep = "-")
  
  return(g_mat)
  
}

g_fitted <- function(theta, x){
  b <- -C(taus, theta[1], theta[2], theta[4], theta[6])/taus
  c <- -D(taus, theta[1], theta[2], theta[4], theta[6])/taus
  
  a <- -(kappa(theta[1], theta[2], theta[3], theta[4], theta[5], theta[6])*taus + 
           2*theta[3]*log(A(taus, theta[1], theta[4])) + 
           2*theta[5]*log(B(taus, theta[2], theta[6])))/taus
  
  b_dr <- t(b %*% t(x[, 1]))
  c_dV <- t(c %*% t(x[, 2]))
  
  g_res <- a + b_dr + c_dV
}


get_plot_parts_acf <- function(x, ci, tenor){
  clim <- qnorm((1 + ci)/2)/sqrt(x$n.used)
  acf <- c(x$acf)
  lag <- c(x$lag)
  
  res <- data.table(tenor = tenor,
                    lag = c(lag, length(lag) + 1), 
                    acf = c(NA, acf[-1], NA), 
                    clim = clim)
  
  return(res)
}

get_acf_data <- function(x) {
  acf_obj <- acf(x$fit_yield_change_error, plot = F, lag.max = 15)
  result <- get_plot_parts_acf(acf_obj, .95, unique(x$tenor))
}

# Dátumok összes lehetséges párosításának elõállítása
first <- as.Date("2002-01-01")
last  <- as.Date("2015-12-31")

dates <- data.table(expand.grid(from = seq.Date(first, last, by = "quarter"), 
                     to = seq.Date(first, last, by = "quarter")))

dates[, diff_month := (year(to) - year(from)) > 1]
dates <- dates[diff_month == T]
dates[, iter_num := 1:nrow(dates)]


# Melyik hozamokat hagyjuk ki a becslésbõl
yields_to_miss <- c("O/N", "M3", "Y10", "Y15")
taus <- c(1/2, 1, 3, 5)

yields <- yields[!(tenor %in% yields_to_miss)]

# Paraméter nevek megadása
var_names <- c("alpha", "beta", "gamma", "delta", "eta", "nu")


# Differenciák kiszámítása az állapotváltozókban
state_var <- dcast.data.table(state_data[short_name %in% c("yield", "var"), 
                                         list(date, short_name, value)], 
                              date ~ short_name,
                              value.var = "value")

state_var <- xts(as.data.frame(state_var[, list(yield, var)]), state_var$date)

diff_state_var <- diff(state_var)[-1, ]


# Differenciák kiszámítása a hozamokban - futamidõnként
yields[, tenor := factor(tenor, unique(tenor))]
yield_change <- dcast.data.table(yields[!(tenor %in% yields_to_miss)], 
                                 date ~ tenor, 
                                 value.var = "yield",
                                 function(x) {sum(x / 100)})

yield_change <- xts(as.data.frame(yield_change[, !grepl("date", 
                                                        names(yield_change)), 
                                               with = F]), 
                    yield_change$date)
yields_for_fit <- yield_change
yield_change <- diff(yield_change)[-c(1:2), ]

for (i in 1:nrow(dates)) {
  
  from_date <- dates[i, from]
  to_date   <- dates[i, to]
  
  curr_state_var    <- diff_state_var[paste(from_date, to_date, sep = "::")]
  curr_yield_change <- yield_change[paste(from_date, to_date, sep = "::")]
  
  dat <- as.matrix(cbind(curr_state_var, curr_yield_change))
  
  for (bigger in c("alpha", "beta")) {
    
    init <- c(ifelse(bigger == "alpha", .1, 0),
              ifelse(bigger == "beta", .1, 0),	
              .1,	.1,	.1,	.1)
    
    names(init) <- var_names
    
    res <- try(gmm(g, x = dat, 
                   t0 = init, optfct = "nlminb",
                   lower = c(0, 0, 0, 0, 0, -Inf),
                   upper = c(Inf, Inf, Inf, Inf, Inf, Inf)), silent = TRUE)
    
    if(class(res) == "try-error") {
      tmp_res <- data.table(iter_num    = dates[i, iter_num],
                            bigger_var  = bigger, 
                            obj_fun_val = as.numeric(NA),
                            conv_code   = -1,
                            conv_mess   = "estimation failed - singular",
                            test_res    = NA)
      
    } else {
      tmp_res <- data.table(iter_num    = dates[i, iter_num],
                            bigger_var  = bigger,
                            obj_fun_val = res$objective,
                            conv_code   = as.numeric(summary(res)$algoInfo$convergence),
                            conv_mess   = summary(res)$algoInfo$message,
                            test_res    = as.numeric(summary(res)$stest$test[2]))
      
      if (exists("coeff_data")) {
        coeff_data <- rbind(copy(coeff_data),
                            data.table(iter_num   = dates[i, iter_num],
                                       bigger_var = bigger,
                                       var_name   = rownames(summary(res)$coefficients),
                                       coeff      = summary(res)$coefficients[, 1],
                                       p_value    = summary(res)$coefficients[, 4]))
        
      } else {
        coeff_data <- data.table(iter_num   = dates[i, iter_num],
                                 bigger_var = bigger,
                                 var_name   = rownames(summary(res)$coefficients),
                                 coeff      = summary(res)$coefficients[, 1],
                                 p_value    = summary(res)$coefficients[, 4])
        
      }
    }
    
    if (exists("res_data")) {
      res_data <- rbind(copy(res_data),
                        copy(tmp_res))
      
    } else {
      res_data <- copy(tmp_res)
      
    }
  }
}

save(coeff_data, file = "./Data/long run coefficient data.rdata")
save(res_data, file = "./Data/gmm fit data for intevals.rdata")


# load("./Data/long run coefficient data.rdata")
# load("./Data/gmm fit data for intevals.rdata")

# Kereszttábla a becslési eredményekrõl
table(res_data$conv_mess, res_data$conv_code)

ggplot(coeff_data[-100 < coeff & coeff < 100], aes(x = iter_num, y = coeff, col = bigger_var)) + 
  geom_point() + facet_wrap(~ var_name, scales = "free_y")

ggplot(coeff_data[-10 < coeff & coeff < 10], aes(x = coeff, fill = bigger_var)) + 
  geom_histogram(binwidth = .01) + facet_wrap(~ var_name, scales = "free")

ggplot(res_data, aes(x = iter_num, y = obj_fun_val, col = bigger_var)) + 
  geom_point()


get_start_params <- merge(res_data, dates, by = "iter_num")
get_start_params <- get_start_params[order(iter_num, test_res, obj_fun_val, na.last = F)]

plot_statistics <- get_start_params[!duplicated(iter_num, fromLast = T)]

ggplot(plot_statistics, aes(x = from, y = to, col = test_res)) + 
  geom_point(size = 4) + 
  labs(title = "Kezdõ és végsõ dátum közötti idõszakra \n becsült GMM modellek szignifikanciái\n", 
         x = "\nKezdõ dátum", y = "Végsõ dátum\n") + 
  scale_color_gradient(high = "black", 
                       low = "grey", 
                       na.value = "white")+
  theme(legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

ggplot(get_start_params, aes(x = from, y = to, col = obj_fun_val)) + 
  geom_point(size = 4) + 
  labs(title = "Kezdõ és végsõ dátum közötti idõszakra \n becsült GMM modellek szignifikanciái\n", 
       x = "\nKezdõ dátum", y = "Végsõ dátum\n") + 
  scale_color_gradient(high = "black", 
                       low = "grey", 
                       na.value = "white")+
  theme(legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))


get_start_params <- get_start_params[!is.na(test_res) & test_res > .1 & conv_code == 0]

target_dates <- data.table(target_date = unique(yields$date))

setnames(get_start_params, c("from", "to"), c("start", "end"))

require(sqldf)


get_start_params <- data.table(sqldf("select a.target_date, b.*
                                      from target_dates as a 
                                      left join get_start_params as b 
                                      on a.target_date > b.start and 
                                      a.target_date < b.end"))

get_start_params <- get_start_params[order(target_date, test_res)]
get_start_params <- get_start_params[!duplicated(target_date, fromLast = T)]

ggplot(get_start_params, aes(x = target_date, y = test_res, col = bigger_var)) + 
  geom_point() + scale_y_continuous(limit = c(0, 1))

start_params <- merge(get_start_params, coeff_data, 
                      by = c("iter_num", "bigger_var"), all.x = T, allow.cartesian = T)

ident_bigger <- start_params[var_name %in% c("alpha", "beta")]
ident_bigger <- dcast.data.table(ident_bigger, target_date ~ var_name, value.var = "coeff")
ident_bigger[, true_bigger_var := ifelse(alpha > beta, "alpha", "beta")]

table(ident_bigger$true_bigger_var)

start_params <- merge(start_params, ident_bigger[, list(target_date, true_bigger_var)], 
                      by = "target_date", all.x = T)

ggplot(start_params, aes(x = target_date, y = coeff)) + geom_line(linetype = 2, size = 1) +
  facet_wrap(~ var_name, scales = "free_y") + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "black") + 
  labs(title = "Legjobb becslésekbõl származó paraméterek értéke és trendje\n", 
       x = "\nDátum", y = "Érték\n") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

start_params[, p_value := round(p_value, 6)]

save(start_params, file = "./Data/start parameters.rdata")

# Legjobb paraméterekkel meghatározott, illesztett becslés
long_params <- start_params[, list(target_date, obj_fun_val, test_res, var_name, value = coeff)]
long_params <- dcast.data.table(long_params, 
                                target_date + obj_fun_val + test_res ~ var_name, 
                                value.var = "value")
long_params <- long_params[order(target_date)]
long_params <- long_params[-1]

# Állapotváltozó különbségek
state_change <- as.data.table(diff_state_var)
setnames(state_change, names(state_change), 
         paste(names(state_change), "_change", sep = ""))
setnames(state_change, "index_change","target_date")

long_params <- merge(long_params, state_change, by = "target_date", all.x = T)

# Hozamok
yield_state_var <- cbind(state_var, yields_for_fit[-1])
yield_state_var <- as.data.table(yield_state_var)
setnames(yield_state_var, "index","target_date")

long_params <- merge(long_params, yield_state_var, by = "target_date", all.x = T)

# Hozamkülönbségek
yield_change_dat <- as.data.table(yield_change)
setnames(yield_change_dat, names(yield_change_dat), 
         paste(names(yield_change_dat), "_change", sep = ""))
setnames(yield_change_dat, "index_change","target_date")

long_params <- merge(long_params, yield_change_dat, by = "target_date", all.x = T)

get_fit <- melt.data.table(long_params, id.vars = 1:13, 
                           variable.name = "tenor", variable.factor = F, 
                           value.name = "obs", value.factor = F)

get_fit[, obs_type := ifelse(grepl("change", tenor), "obs_change", "obs_yield")]
get_fit[, tenor := gsub("_change", "", tenor)]

get_fit <- dcast.data.table(get_fit,  target_date + obj_fun_val + test_res +
                              alpha + beta + delta + eta + gamma + nu + 
                              yield + yield_change + var + var_change + 
                              tenor ~ obs_type, value.var = "obs")

tenor_tau <- data.table(tenor = c("M6", "M12", "Y3", "Y5"),
                        tau = taus)

get_fit <- merge(get_fit, tenor_tau, by = "tenor", all.x = T)

get_fit[, fit_yield := yield(tau, yield, var, alpha, beta, gamma, delta, eta, nu)]

get_fit[, a := -(kappa(alpha, beta, gamma, delta, eta, nu)*tau + 
                    2*gamma*log(A(tau, alpha, delta)) +
                    2*eta*log(B(tau, beta, nu)))/tau ]
get_fit[, b_dr := (-C(tau, alpha, beta, delta, nu)/tau)*yield_change]
get_fit[, c_dV := (-D(tau, alpha, beta, delta, nu)/tau)*var_change]

get_fit[, fit_yield_change := a + b_dr + c_dV]


get_fit[is.na(fit_yield_change), 
        fit_yield_change := obs_yield]

get_fit[, cum_fit_yield := cumsum(fit_yield_change), by = tenor]

get_fit[, fit_yield_change_error := obs_change - fit_yield_change]
get_fit[, fit_error := obs_yield - fit_yield]
get_fit[, cum_fit_error := obs_yield - cum_fit_yield]


# Fit vs residuals, hozammegváltozás nincs, hozam van (meghatározás módja miatt épült bele)
ggplot(get_fit, aes(x = cum_fit_yield, y = cum_fit_error)) + geom_point() + facet_wrap(~ tenor)

ggplot(get_fit[abs(fit_yield_change) < .005], 
       aes(x = fit_yield_change, y = fit_yield_change_error)) + 
  geom_point() + facet_wrap(~ tenor) + 
  scale_x_continuous(labels = percent) + 
  scale_y_continuous(labels = percent) + 
  labs(title = "Az illesztett hozammegváltozások reziduumjai \n", 
       y = "Reziduum \n", x = "\n Illesztett hozammegváltozás") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

ggplot(get_fit[abs(fit_yield_change) < .005], 
       aes(x = fit_yield_change, y = cum_fit_error)) + 
  geom_point() + facet_wrap(~ tenor)

get_fit[, tenor := factor(tenor, c("M6", "M12", "Y3", "Y5"))]

ggplot(get_fit, aes(x = target_date, y = fit_yield_change_error)) + 
  geom_line() + facet_wrap(~ tenor)


ggplot(get_fit, aes(x = target_date, y = cum_fit_error)) + geom_line() + facet_wrap(~ tenor)


get_res_acf <- split(get_fit[!is.na(fit_yield_change_error)], 
                     get_fit$tenor[!is.na(get_fit$fit_yield_change_error)])
get_res_acf <- lapply(get_res_acf, get_acf_data)
get_res_acf <- rbindlist(get_res_acf)

ggplot(get_res_acf, aes(x = lag, y = acf)) + 
  geom_bar(stat = "identity", width = .3) + 
  geom_ribbon(aes(ymin = -clim, ymax = clim), alpha = .2) + 
  facet_wrap(~ tenor, nrow = 2) + 
  labs(title = "Az illesztett hozammegváltozások reziduumjainak korrelogramja \n", 
       y = "Mintabeli autokorreláció \n", x = "\n Késleltetés") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

mean_data <- get_fit[, list(sample_mean = mean(fit_yield_change_error, na.rm = T)), 
                     by = tenor]

ggplot(get_fit[abs(fit_yield_change_error) < .005], 
       aes(x = fit_yield_change_error)) + 
  stat_density(aes(x = fit_yield_change_error, y= ..density..), fill = "grey") + 
  scale_x_continuous(labels = percent) +
  geom_vline(data = mean_data,
             aes(xintercept = sample_mean), linetype = 2, size = 1) + 
  facet_wrap(~ tenor, scales = "free_x") + 
  labs(title = "Reziduumok eloszlása \n", 
       y = "", x = "\n Reziduum") +
  theme(legend.title = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

get_fit <- melt.data.table(get_fit, id.vars = c(1:14, 17),
                           variable.name = "type", variable.factor = F, 
                           value.name = "value", value.factor = F)

get_fit[, tenor := factor(tenor, levels = c("M6", "M12", "Y3", "Y5"))]

mean_data <- get_fit[type == "fit_yield_change_error", 
                     list(sample_mean = mean(value, na.rm = T),
                          sample_deviation = sd(value, na.rm = T), 
                          sample_min = min(value, na.rm = T),
                          sample_max = max(value, na.rm = T)),
                     by = tenor]

ggplot(get_fit[type %in% c("cum_fit_yield", "obs_yield")], 
       aes(x = target_date, y = value, linetype = type)) + 
  geom_line() + 
  facet_wrap(~ tenor) +
  scale_y_continuous(labels = percent) +
  scale_linetype_manual(breaks = c("cum_fit_yield", "obs_yield"),
                        labels = c("Illesztett", "Megfigyelt"), 
                        values = c(1, 2)) +
  labs(title = "Megfigyelt és illesztett hozamidõsorok \n", 
       y = "Hozam \n", x = "\n Kereskedési nap") +
  scale_x_date(minor_breaks = "years") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "serif", colour = "black"),
        axis.text.y = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))

ggplot(get_fit[target_date > as.Date("2002-01-04") & type %in% c("obs_change", "fit_yield_change") &
                 -.005 < value & value < .005]) + 
  geom_density(aes(x = value, fill = type), alpha = .7) + 
  facet_wrap(~ tenor) +
  scale_x_continuous(labels = percent) +
  scale_fill_manual(breaks = c("fit_yield_change", "obs_change"),
                    labels = c("Illesztett", "Megfigyelt"), 
                    values = c("black", "grey")) +
  labs(title = "Megfigyelt és illesztett hozammegváltozások eloszlása \n", 
       y = "", x = "\n Hozammegváltozás") +
  theme(legend.title = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(family = "serif", colour = "black"),
        title = element_text(family = "serif", size = 14, face = "bold"))
  
  
  
normal_data <- split(mean_data, mean_data$tenor)

get_normal_values <- function(x) {
  tenor <- x$tenor
  sample_min <- x$sample_min
  sample_max <- x$sample_max
  x_values <- seq(sample_min, sample_max, by = (sample_max - sample_min)/1000) 
  normal <- dnorm(x_values, x$sample_mean, x$sample_deviation)
  
  x <- data.table(tenor = tenor,
                  x_values = x_values,
                  normal = normal)
}

normal_data <- lapply(normal_data, get_normal_values)
normal_data <- rbindlist(normal_data)

ggplot(get_fit[type == "fit_yield_change_error"], aes(x = value)) + 
  stat_density(aes(x = value, y= ..density..), fill = "grey") + 
  scale_x_continuous(labels = percent) +
  geom_vline(data = mean_data,
             aes(xintercept = sample_mean), linetype = 2, size = 1) + 
  facet_wrap(~ tenor, scales = "free_x") + 
  labs(title = "Reziduumok eloszlása \n", 
       y = "", x = "\n Hiba") +
  theme(legend.title = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        title = element_text(size = 14, face = "bold"))


correlation <- get_fit[type == "obs_yield", list(target_date, tenor, yield, type, value)]
correlation <- dcast.data.table(correlation, target_date + yield ~ tenor, value.var = "value")


x <- round(cor(correlation[, -1, with = F]), 4)

res <- get_fit[type == "cum_fit_error"]



res <- split(res, res$tenor)



res <- lapply(res, get_acf_data)
res <- rbindlist(res)
ggplot(res, aes(x = target_date, y = value)) + geom_line() + facet_wrap(~ tenor)
