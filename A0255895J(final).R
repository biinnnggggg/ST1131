library(stargazer)
library(GGally)

setwd("~/NUS/NUS Academics/AY2223/ST1131/assignment2")

hdb_data <- read.csv("hdb-2012-to-2014.csv")
attach(hdb_data)

head(hdb_data)
names(hdb_data)

# response variable : resale_price

# explanatory variable(s) : town, flat_type, storey_range, floor_area_sqft, floor_area_sqm,
#                           flat_model, lease_commence_date


# exploratory data analysis
hdb_data['log_resale_price'] = log(resale_price)
attach(hdb_data)
log_resale_price

ggpairs(hdb_data[, c("resale_price", "log_resale_price", "lease_commence_date", "floor_area_sqm", "floor_area_sqft")])

head(street_name)
length(unique(street_name))


par(mfrow=c(1, 2), cex.lab=1.25)

skewness <- function(x) {
  x_mean = mean(x)
  stddev = sd(x)
  n = length(x)
  sum((x - x_mean)**3)/((n - 1) * stddev**3)
}

hist(resale_price, ylab = "Frequency", xlab = "Resale Price", main = "")
skewness(resale_price)

# skewness of resale_price is 1.072793

# the response variable is quantitative
# but does not follow a symmetric distribution

# consider the transform T : resale_price -> log(resale_price)

hist(log(resale_price), ylab = "Frequency", xlab = "Log(Resale Price)", main = "")
skewness(log(resale_price))

# skewness of log(resale_price) is 0.369603 which is lower

# from the histogram, the distribution of log(resale_price) is relatively symmetric
# and hence suitable for linear regression. This can be further justified by the lower
# skewness of 0.370 (3 d.p.).

par(mfrow=c(1, 1))
plot(log(resale_price) ~ floor_area_sqm)

# from the scatter plot the relationship between log(resale_price) and floor_area_sqm
# is linear

cor(log(resale_price), floor_area_sqm)

# correlation between log(resale_price) and floor_area_sqm is 0.8333234

# M1

M1 <- lm(log(resale_price) ~ floor_area_sqm)
summary(M1)
stargazer(M1, title="M1 Model Summary",  ci=TRUE, ci.level=0.95, single.row=TRUE)

plot(log(resale_price) ~ floor_area_sqm)
abline(M1, col='red')


# normal qqplot

raw.res = M1$res
SR = rstandard(M1)

# check if the standard residuals follow a normal distribution by visualizing on norm qq plot

par(mfrow=c(1, 2))
qqnorm(SR)
qqline(SR)

# residual plot

res = resid(M1)
plot(fitted(M1), res)
abline(0,0, col='red')

# more data exploration

par(mfrow=c(1, 1))
boxplot(log(resale_price) ~ flat_type, xlab="flat_type", ylab="log(resale_price)")


# M2
x <- factor(storey_range)
new.levels <- c(rep(c("01 TO 15"), each=7), rep(c("16 TO 27"),each=7))
x <- factor(new.levels[x])
storey_range <- x
storey_range

M2 <- lm(log(resale_price) ~ town + floor_area_sqm
         + flat_model + storey_range + floor_area_sqm * town
          + town * flat_model + flat_model * floor_area_sqm)
summary(M2)

# normal qqplot

raw.res = M2$res
SR = rstandard(M2)

# check if the standard residuals follow a normal distribution by visualizing on norm qq plot

par(mfrow=c(2, 1))
qqnorm(SR)
qqline(SR)

shapiro.test(sample(M2$residuals, 5000, replace=FALSE))

# residual plot

res = resid(M2)
plot(fitted(M2), res)
abline(0,0, col='red')

# identify from the model summary that flat_model * floor_area_sqm interaction term is not significant

# for model 3 we discard this

M3 <- lm(log(resale_price) ~ town + floor_area_sqm + flat_model
         + storey_range + floor_area_sqm * town + flat_model * town)
summary(M3)

# normal qqplot

raw.res = M3$res
SR = rstandard(M3)

# check if the standard residuals follow a normal distribution by visualizing on norm qq plot

par(mfrow=c(2, 1))
qqnorm(SR)
qqline(SR)

# residual plot

res = resid(M3)
plot(fitted(M3), res)
abline(0,0, col='red')

# consider the outliers

length(which(SR > 3 | SR < -3))
C <- cooks.distance(M3)
which(C > 1)

# identify from the model summary that town * flat_model interaction term is not significant.
# for model 4 we discard this.


Mn <- lm(log(resale_price) ~ town + floor_area_sqm + flat_model
         + storey_range + floor_area_sqm * town)
summary(Mn)

# normal qqplot

raw.res = Mn$res
SR = rstandard(Mn)

# check if the standard residuals follow a normal distribution by visualizing on norm qq plot

par(mfrow=c(2, 1))
qqnorm(SR)
qqline(SR)

# residual plot

res = resid(Mn)
plot(fitted(Mn), res)
abline(0,0, col='red')

# consider the outliers

length(which(SR > 3 | SR < -3))
C <- cooks.distance(Mn)
which(C > 1)

# M4 is our final model which we have shown to satisfy the normality and constant variance assumptions.







# other possible link functions

# consider the transform T : resale_price -> 1/(resale_price)

hist(1/(resale_price), ylab = "Frequency", xlab = "1/(Resale Price)", main = "")
skewness(1/(resale_price))

# skewness of 1/(resale_price) is 0.2715833 which is lower than 0.369603

# from the histogram, the distribution of 1/(resale_price) is relatively symmetric
# and hence suitable for linear regression. This can be further justified by the lower
# skewness of 0.272 (3 d.p.).

plot(1/(resale_price) ~ floor_area_sqm)

# from the scatter plot the relationship between log(resale_price) and floor_area_sqm
# is linear

cor(1/(resale_price), floor_area_sqm)

# correlation between log(resale_price) and floor_area_sqm is -0.8237911


par(mfrow=c(2, 2))


boxplot(resale_price ~ town, xlab = "Location", ylab = "Resale Price")
boxplot(resale_price ~ flat_type, xlab = "Flat Type", ylab = "Resale Price")
boxplot(resale_price ~ flat_model, xlab = "Flat Model", ylab = "Resale Price")
boxplot(resale_price ~ month, xlab = "Month Sold", ylab = "Resale Price")

par(mfrow=c(1, 2))

plot(floor_area_sqm, resale_price)
plot(lease_commence_date, resale_price)

cor(hdb_data[,c("floor_area_sqm", "lease_commence_date", "resale_price")], method="pearson")


# inital findings

# 1. cor(resale_price, floor_area_sqm) = 0.823 (3dp). There is strong linear and positive association
# between resale_price and floor_area_sqm. Candidate for explanatory variable.
# 2. natural grouping of resale_price by flat_type
# 3. cor(resale_price, lease_commence_date) = 0.370 (3dp). There is weak or no
# association between resale_price and lease_commmence_date.

# Univariate analysis on resale_price

par(mfrow=c(2, 1))
hist(resale_price, col = "red")
boxplot(resale_price, horizontal = TRUE, col = "red")

# resale price is slightly right-skewed

# resale_price ~ floor_area_sqm, group by flat_type

hdb_data$flat_type <- as.factor(hdb_data$flat_type)
hdb_data$flat_model <- as.factor(hdb_data$flat_model)


# start of the report

# 1. summarize the response variable using summary statistics, figures and/or plots.
# Comment if it is suitable to fit a linear regression model for this response.

par(mfrow=c(2, 1))
hist(resale_price, main = "Histogram of Resale Price")
hist(floor_area_sqm, main = "Histogram of Floor Area (sqm)")

# from the histograms, there are no gaps in the hdb_data for resale_price and floor area

# uncertain whether sample is randomized, we just assume it for the assignment
 
# We compute the correlation matrix for the quantitative variables floor_area_sqm
# lease_commence_date, and resale_price

cor(hdb_data[,c("floor_area_sqm", "lease_commence_date", "resale_price")], method="pearson")

