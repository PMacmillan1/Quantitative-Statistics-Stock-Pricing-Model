##----------------------------------##
##---------Mini Project 1-----------##
##----------------------------------##

rm(list = ls())

## Question 1 ##
## "Adapt the code to plot large value stocks versus small value stocks (“Value Stocks”) and large growth stocks versus small growth stocks (“Growth Stocks”)"

setwd("~/Quants MSBC 5031/Data")
ff = read.table("french_6portfolios.txt")
names(ff) = c("DATE","S.L", "S.M", "S.H", "B.L", "B.M", "B.H")

ff$DATE <- as.character(ff$DATE)
ff$DATE <- paste(ff$DATE, "01", sep = "")

##this function computes gross returns, given percentage returns as an input##
gross_ret = function(vec){
  gross_ret = 1+(vec/100)
  return(gross_ret)
}
## Defining Variables for Gross Returns ##
ff$big_low = gross_ret(ff$B.L) #This is the large growth stock
ff$big_mid = gross_ret(ff$B.M)
ff$big_hi = gross_ret(ff$B.H) #This is the large value stock

ff$sm_low = gross_ret(ff$S.L) #This is the small growth stock
ff$sm_mid = gross_ret(ff$S.M)
ff$sm_hi = gross_ret(ff$S.H) #This is the small value stock

## Reformatting the DATE variable as a date object ##
ff$DATE = as.Date(ff$DATE, format="%Y%m%d")

## Plotting historical returns ##
## Note: cumulative return is (1+r1)*(1+r2)*...*(1+rt), computed using cumprod(gross_return)

## Illustrating geomean calculation ##
plot(ff$DATE, cumprod(ff$big_hi), type="l", main="Large Firm Returns", sub="Black is high B/M, Blue is low B/M", xlab="Date", ylab="Cumulative Return")

gm_mean = function(a){prod(a)^(1/length(a))}

big_hi_gm = gm_mean(ff$big_hi)
ff$big_hi_gm = big_hi_gm
lines(ff$DATE, cumprod(ff$big_hi_gm), lty="dashed")

## Contrasting the historical returns from four types of portfolios ##
par(mfrow=c(1,2))
plot(ff$DATE, cumprod(ff$big_hi), type="l", main="Large Firm Returns", sub="Black is high B/M, Blue is low B/M", xlab="Date", ylab="Cumulative Return")
lines(ff$DATE, cumprod(ff$big_low), col="blue")

plot(ff$DATE, cumprod(ff$sm_hi), type="l", main="Small Firm Returns", sub="Black is high B/M, Blue is low B/M", xlab="Date", ylab="Cumulative Return")
lines(ff$DATE, cumprod(ff$sm_low), col="blue", lty="dashed")

#Question 1

par(mfrow = c(1,2))

plot(ff$DATE, cumprod(ff$big_hi), type="l", main="Large Value Stock Returns vs. Small Value Stock Returns", ylim = range(0,300000), xlab="Date", ylab="Cumulative Return")
lines(ff$DATE, cumprod(ff$sm_hi), col="red")
legend("topleft", legend = c("large stock high B/M", "small stock high B/M"), col=c("black","red"), 
       lty=c("solid","solid"), bty="n")

plot(ff$DATE, cumprod(ff$big_low), type="l", main="Large Growth Stock Returns vs. Small Growth Stock Returns", xlab="Date", ylab="Cumulative Return")
lines(ff$DATE, cumprod(ff$sm_low), col="red", lty="dashed")
legend("topleft", legend = c("large stock low B/M", "small stock low B/M"), col=c("black","red"), 
       lty=c("solid","solid"), bty="n")

## Question 2 ## 
## Compare Geometric Mean to Arithmetic Mean for each of the four return series 

## 2a.
## Compute the arithmetic mean of the returns for each of the four portfolios (express your answer in decimal form rather than percentages).

arith_mean_Big_hi <- mean(ff$big_hi)
arith_mean_Big_low <- mean(ff$big_low)

arith_mean_Sm_hi <- mean(ff$sm_hi)
arith_mean_Sm_low <- mean(ff$sm_low)

arith_mean_Big_hi
arith_mean_Big_low
arith_mean_Sm_hi
arith_mean_Sm_low

## 2b.
## Compute the standard deviation of the returns.

sd_Big_hi <- sd(ff$big_hi)
sd_Big_low <- sd(ff$big_low)

sd_Sm_hi <- sd(ff$sm_hi)
sd_Sm_low <- sd(ff$sm_low)

sd_Big_hi
sd_Big_low
sd_Sm_hi
sd_Sm_low

## 2c.
## Compute the geometric mean of the gross returns

geomean_big_hi <- prod(ff$big_hi, na.rm = TRUE)^(1/1141)
geomean_big_low <- prod(ff$big_low, na.rm = TRUE)^(1/1141)

geomean_sm_hi <- prod(ff$sm_hi, na.rm = TRUE)^(1/1141)
geomean_sm_low <- prod(ff$sm_low, na.rm = TRUE)^(1/1141)

geomean_big_hi
geomean_big_low
geomean_sm_hi
geomean_sm_low

## 2d.
## Compute the approximate geometric mean.

approx_geo_mean_big_hi <- (arith_mean_Big_hi- 0.5*sd_Big_hi^2)
approx_geo_mean_big_low <- (arith_mean_Big_low- 0.5*sd_Big_low^2)

approx_geo_mean_sm_hi <- (arith_mean_Sm_hi- 0.5*sd_Sm_hi^2)
approx_geo_mean_sm_low <- (arith_mean_Sm_low- 0.5*sd_Sm_low^2)

approx_geo_mean_big_hi
approx_geo_mean_big_low
approx_geo_mean_sm_hi
approx_geo_mean_sm_low

## Deliverables

#A 
## In order to determine whether these differences are statistically significant, we can perform a t.test. 
##Do small stocks tend to outperform large stocks
#comparison of small vs. large value stocks
t.test(ff$sm_hi,ff$big_hi, paired = TRUE)
#Comparison of small vs. large growth stocks
t.test(ff$sm_low,ff$big_low, paired = TRUE)

#comparison of small value stocks vs. small growth stocks
t.test(ff$sm_hi,ff$sm_low, paired = TRUE)

#comparison of large value stocks vs. large growth stocks
t.test(ff$big_hi,ff$big_low, paired = TRUE)

