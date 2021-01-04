# [ 3. 증명 - 표본과 모집단의 관계 ]
# 3.1 표본평균(xbar)과 모평균의 관계
# 모집단으로부터 추출된 표본의 평균의 평균을 구하면
# 모집단의 평균(모평균)에 근사해짐

# 3.2 표본평균의 표준편차와 모표준편차의 관계
# 모집단으로부터 추출된 표본들의 평균의 표준편차를 구하면
# 모집단의 표준편차에 표본의 크기를 나눈 값과 근사해짐

# 3.3 표본평균의 분포
# 표본평균의 분포는 정규분포를 따른다 *****
xbar ~ N(mu, sigma^2 / n)
# ------------------------------------------------------------------------------

# [ 3. 증명 - 모집단이 정규분포를 따르는 경우 ]
# 3.1 표본평균(xbar)과 모평균(mu)의 관계
# 3.2 표본평균의 표준편차와 모표준편차의 관계
mean_v10 <- c()
mean_v50 <- c()
mean_v100 <- c()

for (i in 1:1000) {
  mean_v10[i] <- mean(rnorm(10, mean = 0, sd = 1))
  mean_v50[i] <- mean(rnorm(50, mean = 0, sd = 1))
  mean_v100[i] <- mean(rnorm(100, mean = 0, sd = 1))
}

# 1) 표본평균의 표준편차 구하기
sd(mean_v10)    # 0.3011593
sd(mean_v50)    # 0.1405952
sd(mean_v100)   # 0.1010321

# 2) 모표준편차(1)와 비교
E(xbar) = mu    # xbar = 표본평균, E() = 평균, mu = 모평균
sd(xbar) = sigma / sqrt(n)    # sigma = 모표준편차

1 / sqrt(10)    # 0.3162278 == sd(mean_v10)
1 / sqrt(50)    # 0.1414214 == sd(mean_v50)
1 / sqrt(100)   # 0.1 == sd(mean_v100)
# 정확하지는 않지만 거의 일치

# [ 결론 ]
# => 표본평균의 분산을 구하면 모분산인 sigma^2에 n(sample size)을 나눈값에 근사해진다
# => 표본평균의 표준편차를 구하면 모표준편차인 sigma에 sqrt(n)을 나눈값에 근사해진다
# ------------------------------------------------------------------------------

# 3.3 표본평균의 분포
# 표본평균은 평균이 모평균과 같고 분산이 모분산(sigma^2)을 n으로 나눈값을 갖는
# 정규분포를 따른다

# 1) 표본평균의 확률분포 시각화
dev.new()
par(mfrow = c(1, 3))

hist(mean_v10)    # sample size가 10인 1000개의 표본평균이 갖는 분포
hist(mean_v50)    # sample size가 50인 1000개의 표본평균이 갖는 분포
hist(mean_v100)   # sample size가 100인 1000개의 표본평균이 갖는 분포

# 2) 표본평균의 이론적 분포 (xbar ~ N(mu, sigma^2/n))
mean_v10 ~ N(0, 1/10)
mean_v50 ~ N(0, 1/50)
mean_v100 ~ N(0, 1/100)

# 3) 위 분포 시각화
x1 <- seq(-1, 1, 0.01)
y10 <- dnorm(x1, mean = 0, sd = sqrt(1/10))
y50 <- dnorm(x1, mean = 0, sd = sqrt(1/50))
y100 <- dnorm(x1, mean = 0, sd = sqrt(1/100))

dev.new()
par(mfrow = c(1, 3))
plot(x1, y10, type = 'l', main = 'n=10, 표본평균의 분포', cex = 3)
plot(x1, y50, type = 'l', main = 'n=50, 표본평균의 분포', cex = 3)
plot(x1, y100, type = 'l', main = 'n=100, 표본평균의 분포', cex = 3)

# 4) 표본평균의 실제 분포와 이론적 분포 비교
dev.new()
par(mfrow = c(1, 3))

# 4-1) sample size 10인 표본의 분포
hist(mean_v10, prob = T)    # 실제 분포
lines(x1, y10, type = 'l', col = 'red', cex = 3)    # 이론적 분포

# 4-1) sample size 50인 표본의 분포
hist(mean_v50, prob = T)
lines(x1, y50, type = 'l', col = 'red', cex = 3)

# 4-1) sample size 100인 표본의 분포
hist(mean_v100, prob = T)
lines(x1, y100, type = 'l', col = 'red', cex = 3)

# [ 결론 ]
# 평균이 mu, 표준편차가 sigma인 정규분포를 따르는 표본으로부터 얻은 표본평균의 
# 분포는 평균이 mu, 표준편차가 sigma/sqrt(n)인 정규분포를 따른다
# ------------------------------------------------------------------------------

# -------------------------------연 습 문 제------------------------------------
# 연습문제 6. 이항분포
# X ~ B(100, 0.5)를 따르는 모집단으로부터
# 샘플 사이즈가 각각 10, 50, 100인 샘플을 총 1000번 추출하여
# 표본평균이 갖는 분포를 확인 (표본 평균의 평균이 모평균에 근사하는지)
# 단, E(X) = n * p, var(X) = n * p * (1-p) 라 가정
E(X)= 100 * 0.5 = 50
var(X) = 100 * 0.5 * 0.5 = 25
sd(X) = sqrt(25) = 5
# 위 모집단의 값을 표본의 값과 비교할 예정 -> 비슷하면 가설증명

# 증명 3.1 표본평균(xbar)과 모평균의 관계
b10 <- rbinom(10, size = 100, prob = 0.5)
b50 <- rbinom(50, size = 100, prob = 0.5)
b100 <- rbinom(100, size = 100, prob = 0.5)

mean(b10)    # 49.4
mean(b50)    # 49.64
mean(b100)   # 50.41

# 꽤 유사하긴 함

mean_b10 <- c()
mean_b50 <- c()
mean_b100 <- c()

for (i in 1:1000) {
  mean_b10[i] <- mean(rbinom(10, size = 100, prob = 0.5))
  mean_b50[i] <- mean(rbinom(50, size = 100, prob = 0.5))
  mean_b100[i] <- mean(rbinom(100, size = 100, prob = 0.5))
}

mean(mean_b10)    # 49.99
mean(mean_b50)    # 49.98
mean(mean_b100)   # 50.02
# 거의 근접 => 이항분포도 모평균의 평균이 표본평균의 평균과 비슷하다!

# 증명 3.2 표본평균의 표준편차와 모표준편차의 관계
var(xbar) = sigma^2 / n
sd(xbar) == sigma/sqrt(n)
sigma(모표준편차) = sqrt(npq) = sqrt(100*0.5*0.5) = 5

sqrt(100 * 0.5 * 0.5)    # 모표준편차 5

sd(mean_b10)    # 1.494863
sd(mean_b50)    # 0.7210751
sd(mean_b100)   # 0.5243237

sqrt(100 * 0.5 * 0.5) / sqrt(10)    # 1.581139 => 거의 근사
sqrt(100 * 0.5 * 0.5) / sqrt(50)    # 0.7071068 => 거의 근사
sqrt(100 * 0.5 * 0.5) / sqrt(100)    # 0.5 => 거의 근사

# 증명 3.3 표본의 분포
# 1) 표본평균의 확률분포 시각화
# xbar ~ N(mu, (sigma/sqrt(n))^2)
# 1-1) sample size = 10
dev.new()
hist(mean_b10, prob = T)    # 실제 분포

x10 <- seq(min(mean_10), max(mean_10), 0.01)
y10 <- dnorm(x10, mean = 50, sd = 5 / sqrt(10))
lines(x10, y10, type = 'o', col = 'red')    # 이론적 분포

hist(mean_b50, prob = T)
hist(mean_b100, prob = T)

# 2) 표본평균의 이론적 분포 (xbar ~ N(mu, sigma^2/n))
mean_b10 ~ N(50, 25/10)
mean_b50 ~ N(50, 25/50)
mean_b100 ~ N(50, 25/100)

# 3) 위 분포 시각화
x1 <- seq(-1, 1, 0.01)
y10 <- rbinom(x1, mean = 50, sd = sqrt(25/10))

x1 <- seq(-1, 1, 0.01)
y10 <- dnorm(x1, mean = 0, sd = sqrt(1/10))
y50 <- dnorm(x1, mean = 0, sd = sqrt(1/50))
y100 <- dnorm(x1, mean = 0, sd = sqrt(1/100))

dev.new()
par(mfrow = c(1, 3))
plot(x1, y10, type = 'l', main = 'n=10, 표본평균의 분포', cex = 3)
plot(x1, y50, type = 'l', main = 'n=50, 표본평균의 분포', cex = 3)
plot(x1, y100, type = 'l', main = 'n=100, 표본평균의 분포', cex = 3)

# 4) 표본평균의 실제 분포와 이론적 분포 비교
dev.new()
par(mfrow = c(1, 3))

# 4-1) sample size 10인 표본의 분포
hist(mean_v10, prob = T)
lines(x1, y10, type = 'l', col = 'red', cex = 3)

# 4-1) sample size 50인 표본의 분포
hist(mean_v50, prob = T)
lines(x1, y50, type = 'l', col = 'red', cex = 3)

# 4-1) sample size 100인 표본의 분포
hist(mean_v100, prob = T)
lines(x1, y100, type = 'l', col = 'red', cex = 3)
# ------------------------------------------------------------------------------