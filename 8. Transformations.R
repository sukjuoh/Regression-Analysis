## 8. Transformations  ##

# page 3
load("D:/regression/data/bacteria.Rdata")
plot(bacteria.table$t, bacteria.table$N_t, pch=23, cex=2, bg='orange')
bacteria.lm = lm(N_t ~ t, bacteria.table)
abline(bacteria.lm$coef)

# page 4
par(mfrow=c(2,2))
plot(bacteria.lm, pch=23, bg='orange')

# page 6
bacteria.log.lm <- lm(log(N_t) ~ t, bacteria.table)
plot(bacteria.table$t, bacteria.table$N_t, pch=23, cex=2, bg='orange')
lines(bacteria.table$t, fitted(bacteria.lm), lwd=2, col='red')
lines(bacteria.table$t, exp(fitted(bacteria.log.lm)), lwd=2, col='green')

# page 7
par(mfrow=c(2,2))
plot(bacteria.log.lm, pch=23, bg='orange')

# page 15
load("D:/regression/data/education.Rdata")
education.table$Region = factor(education.table$Region)
education.lm = lm(Y ~ X1 + X2 + X3, data=education.table)
summary(education.lm)

# page 16
par(mfrow=c(2,2))
plot(education.lm)

# page 17
par(mfrow=c(1,1))
boxplot(rstandard(education.lm) ~ education.table$Region, col=c('red', 'green', 'blue', 'yellow'))

# page 18
keep.subset = (education.table$STATE != 'AK')
education.noAK.lm = lm(Y ~ X1 + X2 + X3, subset=keep.subset, data=education.table)
summary(education.noAK.lm)

# page 19
par(mfrow=c(2,2))
plot(education.noAK.lm)

# page 20
par(mfrow=c(1,1))
boxplot(rstandard(education.noAK.lm) ~ education.table$Region[keep.subset], col=c('red', 'green', 'blue', 'yellow'))

# page 24
educ.weights = 0 * education.table$Y
for (region in levels(education.table$Region)) {
 subset.region = (education.table$Region[keep.subset] == region)
 educ.weights[subset.region] <- 1.0 / (sum(resid(education.noAK.lm)[subset.region]^2) / sum(subset.region))
}
unique(educ.weights)

# page 26
education.noAK.weight.lm <- lm(Y ~ X1 + X2 + X3, weights=educ.weights, subset=keep.subset, data=education.table)
summary(education.noAK.weight.lm)

# page 27
summary(education.noAK.lm)

# page 28
par(mfrow=c(2,2))
plot(education.noAK.weight.lm)

# page 29
par(mfrow=c(1,1))
boxplot(resid(education.noAK.weight.lm, type='pearson') ~ education.table$Region[keep.subset], col=c('red', 'green', 'blue', 'yellow'))

# page 33
set.seed(0)
mu = 2
sigma = 1
n = 20
sd = seq(1, n, 1) * sigma

unweighted.estimate = c()
weighted.estimate = c()
suboptimal.estimate = c()

for (i in 1:10000) {
 cur.sample = rnorm(n, mu, sd)
 unweighted.estimate[i] = mean(cur.sample)
 weighted.estimate[i] = sum(cur.sample/(seq(1,n,1)^2)) / sum(1/(seq(1,n,1)^2))
 suboptimal.estimate[i] = sum(cur.sample/seq(1,n,1)) / sum(1/seq(1,n,1))
}
print(data.frame(mean(unweighted.estimate), sd(unweighted.estimate)))
print(data.frame(mean(weighted.estimate), sd(weighted.estimate)))
print(data.frame(mean(suboptimal.estimate), sd(suboptimal.estimate)))

# page 34
X = c(0); Y = c(0)
plot(X, Y, xlim = c(-15, 15), ylim = c(0, 0.5), type='n', main='Comparison of densities of the estimators')
lines(density(weighted.estimate), col='red', lwd=4)
lines(density(unweighted.estimate), col='blue', lwd=4)
lines(density(suboptimal.estimate), col='purple', lwd=4)
legend(6,0.3, c('optimal', 'suboptimal', 'mean'), col=c('red', 'purple', 'blue'), lwd=rep(4,3))