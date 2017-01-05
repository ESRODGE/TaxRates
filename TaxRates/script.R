library("ggplot2")

TaxRates <- data.frame(seq(0, 500000, 1000))
colnames(TaxRates) <- c('Pay')

TaxRates$SS <- 0
TaxRates[TaxRates$Pay < 118501,]$SS <- 0.062
TaxRates[TaxRates$Pay > 118501,]$SS <- 7347 / TaxRates[TaxRates$Pay > 118501,]$Pay

TaxRates$Medicare <- .0145
TaxRates$Fed <- 0
TaxRates[TaxRates$Pay < 9275,]$Fed <- 0.1

TaxRates[TaxRates$Pay > 9275 & TaxRates$Pay < 37650,]$Fed <- ((.15 * (TaxRates[TaxRates$Pay > 9275 & TaxRates$Pay < 37650,]$Pay - 9276)) + 927.5) / TaxRates[TaxRates$Pay > 9275 & TaxRates$Pay < 37650,]$Pay
TaxRates[TaxRates$Pay > 37651 & TaxRates$Pay < 91150,]$Fed <- ((.25 * (TaxRates[TaxRates$Pay > 37651 & TaxRates$Pay < 91150,]$Pay - 37650)) + 5183.75) / TaxRates[TaxRates$Pay > 37651 & TaxRates$Pay < 91150,]$Pay
TaxRates[TaxRates$Pay > 91151 & TaxRates$Pay < 190150,]$Fed <- ((.28 * (TaxRates[TaxRates$Pay > 91151 & TaxRates$Pay < 190150,]$Pay - 91150)) + 18558.75) / TaxRates[TaxRates$Pay > 91151 & TaxRates$Pay < 190150,]$Pay
TaxRates[TaxRates$Pay > 190151 & TaxRates$Pay < 413350,]$Fed <- ((.33 * (TaxRates[TaxRates$Pay > 190151 & TaxRates$Pay < 413350,]$Pay - 190150)) + 46278.75) / TaxRates[TaxRates$Pay > 190151 & TaxRates$Pay < 413350,]$Pay
TaxRates[TaxRates$Pay > 413351 & TaxRates$Pay < 415050,]$Fed <- ((.35 * (TaxRates[TaxRates$Pay > 413351 & TaxRates$Pay < 415050,]$Pay - 413350)) + 119934.75) / TaxRates[TaxRates$Pay > 413351 & TaxRates$Pay < 415050,]$Pay
TaxRates[TaxRates$Pay > 415051,]$Fed <- ((.396 * (TaxRates[TaxRates$Pay > 415051,]$Pay - 415050)) + 120529.75) / TaxRates[TaxRates$Pay > 415051,]$Pay
TaxRates$TotalTax <- TaxRates$Fed + TaxRates$Medicare + TaxRates$SS

#AU
TaxRates$AustralianTax <- 0
TaxRates[TaxRates$Pay > 13153 & TaxRates$Pay < 26748,]$AustralianTax <- ((.19 * (TaxRates[TaxRates$Pay > 13153 & TaxRates$Pay < 26748,]$Pay - 13152))) / TaxRates[TaxRates$Pay > 13153 & TaxRates$Pay < 26748,]$Pay
TaxRates[TaxRates$Pay > 26749 & TaxRates$Pay < 57816,]$AustralianTax <- ((.325 * (TaxRates[TaxRates$Pay > 26749 & TaxRates$Pay < 57816,]$Pay - 26748)) + 2582) / TaxRates[TaxRates$Pay > 26749 & TaxRates$Pay < 57816,]$Pay
TaxRates[TaxRates$Pay > 57817 & TaxRates$Pay < 130104,]$AustralianTax <- ((.37 * (TaxRates[TaxRates$Pay > 57817 & TaxRates$Pay < 130104,]$Pay - 57816)) + 12683) / TaxRates[TaxRates$Pay > 57817 & TaxRates$Pay < 130104,]$Pay
TaxRates[TaxRates$Pay > 130105,]$AustralianTax <- ((.47 * (TaxRates[TaxRates$Pay > 130105,]$Pay - 130104)) + 39431) / TaxRates[TaxRates$Pay > 130105,]$Pay
TaxRates$AustralianMedicare <- 0.02
TaxRates[TaxRates$Pay < 15418.70,]$AustralianMedicare <- 0.0
TaxRates$AustralianTotalTax <- TaxRates$AustralianMedicare + TaxRates$AustralianTax

#Capital Gains
TaxRates$CapitalGains <- 0
TaxRates[TaxRates$Pay > 37651 & TaxRates$Pay < 415051,]$CapitalGains <- ((.15 * (TaxRates[TaxRates$Pay > 37651 & TaxRates$Pay < 415051,]$Pay - 37650))) / TaxRates[TaxRates$Pay > 37651 & TaxRates$Pay < 415051,]$Pay
TaxRates[TaxRates$Pay > 415051,]$CapitalGains <- ((.2 * (TaxRates[TaxRates$Pay > 415051,]$Pay - 415050)) + 56610) / TaxRates[TaxRates$Pay > 415051,]$Pay


USRates <- data.frame(Income = TaxRates$Pay, Rate = TaxRates$TotalTax)
USRates$Country <- 'United States'
AURates <- data.frame(Income = TaxRates$Pay, Rate = TaxRates$AustralianTotalTax)
AURates$Country <- 'Australia'

CapGains <- data.frame(Income = TaxRates$Pay, Rate = TaxRates$CapitalGains)
CapGains$Country <- 'US Capital Gains'


Combined <- rbind(USRates, AURates, CapGains)
Combined$Income <- Combined$Income / 1000


ggplot(data = Combined,
       aes(x = Income, y = Rate, colour = Country)) +
        ggtitle("Fed Tax + SS + Medicare \n as a perent of income") + 
        xlab("Income (1000 USD)") +
        ylab("Tax") +
        scale_y_continuous(breaks = round(seq(0, max(Combined$Rate), by = 0.05), 3)) +
        scale_x_continuous(breaks = round(seq(0, 500, by = 25), 0)) +
       geom_line()