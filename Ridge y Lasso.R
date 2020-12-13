

install.packages('glmnet')

# For reproducible results
set.seed(123)
# Number of observations
num <- 500
dat <- data.frame(watched_jaws = rnorm(n=num, mean=50, sd=10),
                  swimmers = round(rnorm(n=num, mean=500, sd=100)),
                  temp = rnorm(n=num, mean=90, sd=2),
                  stock_price = runif(n=num, min = 100, max=150))
attacks <- round(rnorm(n=num, mean = 30, sd=10)+ # noise
                   -2*dat$watched_jaws+ # 1 fewer attack for 1 percent increase in Jaws movie audience
                   0.1*dat$swimmers+ # 1 more attack for each 10 swimmers on site
                   1*dat$temp+ # 1 more attack for each degrees of increase in temp
                   0*dat$stock_price) # no relationship
dat$attacks <- attacks
plot(dat)

# Regress all the predictor variables onto "attacks" response variable
res <- lm(attacks~., data=dat)
summary(res)

library(glmnet)
## Loading required package: Matrix
## Loaded glmnet 4.0-2
# Prepare glmnet input as matrix of predictors and response var as vector
varmtx <- model.matrix(attacks~.-1, data=dat)
response <- dat$attacks
# alpha=0 means ridge regression. 
ridge <- glmnet(scale(varmtx), response, alpha=0)
# Cross validation to find the optimal lambda penalization
cv.ridge <- cv.glmnet(varmtx, response, alpha=0)
lbs_fun <- function(fit, offset_x=1, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])+ offset_x
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
}
plot(ridge, xvar = "lambda", label=T)
lbs_fun(ridge)
abline(v=cv.ridge$lambda.min, col = "red", lty=2)
abline(v=cv.ridge$lambda.1se, col="blue", lty=2)


# alpha=1 means lasso regression. 
lasso <- glmnet(scale(varmtx), response, alpha=1)
# Cross validation to find the optimal lambda penalization
cv.lasso <- cv.glmnet(varmtx, response, alpha=1)
plot(lasso, xvar = "lambda", label=T)
lbs_fun(ridge, offset_x = -2)
abline(v=cv.lasso$lambda.min, col = "red", lty=2)
abline(v=cv.lasso$lambda.1se, col="blue", lty=2)


dat$colinear1 <- dat$watched_jaws + rnorm(n=num, mean=0, sd=1)
dat$colinear2 <- -dat$watched_jaws + rnorm(n=num, mean=0, sd=1)
plot(dat[, colnames(dat) %in% c("watched_jaws", "colinear1", "colinear2", "attacks")])

# Prepare glmnet input as matrix of predictors and response var as vector
varmtx <- model.matrix(attacks~.-1, data=dat)
response <- dat$attacks
# alpha=0 means ridge regression. 
ridge2 <- glmnet(scale(varmtx), response, alpha=0)
# Cross validation to find the optimal lambda penalization
cv.ridge2 <- cv.glmnet(varmtx, response, alpha=0)
# alpha=1 means lasso regression. 
lasso2 <- glmnet(scale(varmtx), response, alpha=1)
# Cross validation to find the optimal lambda penalization
cv.lasso2 <- cv.glmnet(varmtx, response, alpha=1)
par(mfrow=c(1,2))
par(mar=c(4,2,6,2))
plot(ridge2, xvar = "lambda", label=T)
lbs_fun(ridge2, offset_x = 1)
abline(v=cv.ridge2$lambda.min, col = "red", lty=2)
abline(v=cv.ridge2$lambda.1se, col="blue", lty=2)
title("Ridge (with co-linearity)", line=2.5)
plot(lasso2, xvar = "lambda", label=T)
lbs_fun(lasso2, offset_x = 1)
abline(v=cv.lasso2$lambda.min, col = "red", lty=2)
abline(v=cv.lasso2$lambda.1se, col="blue", lty=2)
title("Lasso (with co-linearity)", line=2.5)
