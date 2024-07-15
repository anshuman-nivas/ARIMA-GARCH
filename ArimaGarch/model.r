require("quantmod")  # nolint
require("timeSeries")  # nolint
require("rugarch")  # nolint
require("PerformanceAnalytics")  # nolint
require("forecast")  # nolint

# Load EURUSD data  # nolint
eurusd <- read.csv(file = "EURUSD.csv", header = TRUE)  # nolint
dates <- as.Date(as.character(eurusd[, 1]), format = "%d/%m/%Y")  # nolint
returns <- diff(log(eurusd$C))  # nolint

# Initialize variables  # nolint
window.length <- 200  # nolint
forecasts.length <- length(returns) - window.length  # nolint
forecasts <- vector(mode = "numeric", length = forecasts.length)  # nolint
directions <- vector(mode = "numeric", length = forecasts.length)  # nolint
aic.values <- vector(mode = "numeric", length = forecasts.length)  # nolint

for (i in 0:forecasts.length) {  # nolint
    roll.returns <- returns[(1+i):(window.length + i)]  # nolint
    final.aic <- Inf  # nolint
    final.order <- c(0, 0, 0)  # nolint

    for (p in 1:4) for (q in 1:4) {  # nolint
        model <- tryCatch(arima(roll.returns, order = c(p,0,q)), error = function(err) FALSE, warning = function(err) FALSE)  # nolint
        if (!is.logical(model)) {  # nolint
            current.aic <- AIC(model)  # nolint
            if (current.aic < final.aic) {  # nolint
                final.aic <- current.aic  # nolint
                final.order <- c(p, 0, q)  # nolint
                final.arima <- arima(roll.returns, order = final.order)  # nolint
            }  # nolint
        }  # nolint
    }  # nolint

    aic.values[i+1] <- final.aic  # nolint

    spec <- ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder = c(final.order[1], final.order[3]), include.mean = TRUE), distribution.model = "sged")  # nolint
    fit <- tryCatch(ugarchfit(spec, roll.returns, solver = 'hybrid'), error = function(e) e, warning = function(w) w)  # nolint

    if (is(fit, "warning")) {  # nolint
        forecasts[i+1] <- 0  # nolint
    } else {  # nolint
        next.day.forecast <- ugarchforecast(fit, n.ahead = 1)  # nolint
        x <- next.day.forecast@forecast$seriesFor  # nolint
        directions[i+1] <- ifelse(x[1] > 0, 1, -1)  # nolint
        forecasts[i+1] <- x[1]  # nolint
    }  # nolint
}  # nolint

forecasts.ts <- xts(forecasts, dates[(window.length):length(returns)])  # nolint
strategy.forecasts <- Lag(forecasts.ts, 1)  # nolint
strategy.directions <- ifelse(strategy.forecasts > 0, 1, ifelse(strategy.forecasts < 0, -1, 0))  # nolint
strategy.directions.returns <- strategy.directions * returns[(window.length):length(returns)]  # nolint
strategy.directions.returns[1] <- 0  # nolint
strategy.curve <- cumsum(strategy.directions.returns)  # nolint

longterm.ts <- xts(returns[(window.length):length(returns)], dates[(window.length):length(returns)])  # nolint
longterm.curve <- cumsum(longterm.ts)  # nolint

colnames(strategy.curve) <- NULL  # nolint
colnames(longterm.curve) <- NULL  # nolint

both.curves <- cbind(strategy.curve, longterm.curve)  # nolint
colnames(both.curves) <- c("Strategy Returns", "Long Term Investing Returns")  # nolint

# Save cumulative returns plot as PNG  # nolint
png(file = "cumulative_returns.png", width = 800, height = 600)  # nolint
plot(x = both.curves[, "Strategy Returns"], xlab = "Time", ylab = "Cumulative Return", main = "Cumulative Returns", ylim = c(-0.25, 0.4), major.ticks = "quarters", minor.ticks = FALSE, col = "green")  # nolint
lines(x = both.curves[, "Long Term Investing Returns"], col = "red")  # nolint
legend(x = 'bottomleft', legend = c("ARIMA & GARCH", "Long Term Investing"), lty = 1, col = c("green", "red"))  # nolint
dev.off()  # Close the PNG device  # nolint

# Save ACF plot as PNG  # nolint
png("acf_plot.png")  # nolint
acf(returns, main = 'ACF Plot')  # nolint
dev.off()  # nolint
