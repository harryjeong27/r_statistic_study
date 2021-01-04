# 과제 1. 기존 치료법의 치료기간이 평균 10일, 표준편차가 3일인 정규분포를 
# 따른다고 알려져 있다.새로운 치료법을 25명의 환자에게 적용해서 평균 9일, 
# 표준편차가 3일인 성적을 얻었다. 이 경우, 새로운 치료법의 효과를 인정할 수 있는가

# 더 작아졌는가? 왼쪽 검정
H0 : mu = 10
H1 : mu < 10

mu <- 10
sigma <- 3
n <- 25

xbar <- 9

# p-value
z* = (xbar - mu) / (sigma/sqrt(n)) = -1.666667

p-value = P(Z<z*)
        = P(Z < -1.666667)
        = P(Z < -1.666667)
        = pnorm(-1.666667, mean = 0, sd = 1)
        = 0.04779032    <<<    0.05
          => H0 기각 
          => 새로운 치료법의 효과가 더 낫다고 볼 수 있음

# z.test => 모표준편차를 알고 검정
library(BSDA)
표본 25개 (평균이 10) 만들어서 테스트 가능
z.test(10, alternative = 'less', 10, 3)

# 시각화
v_x <- seq(-4, 4, 0.01)
v_y <- dnorm(v_x, 0, 1)

dev.new()

plot(v_x, v_y, type = 'l', main = 'less')
abline(v = -1.64)
abline(h = 0)

f1 <- function(x) {
  dnorm(x, 0, 1)
}

z <- (xbar - mu) / (sigma/sqrt(n))    # -1.666667

polygon(c(z, seq(-4, z, 0.01), z),
        c(0, f1(seq(-4, z, 0.01)), 0),
        col = 'red')    # 유의확률 구간

polygon(c(-1.64, seq(-4, -1.64, 0.01), -1.64),
        c(0, f1(seq(-4, -1.64, 0.01)), 0),
        density = 10)    # 왼쪽 기각역
text(-1.64, 0, "-1.64")

# 그림에서 보듯 유의확률 구간이 왼쪽 기각역 안에 포함되므로 H0 기각!

# 과제 2. 랜덤하게 샘플링한 초콜릿 무게의 값이 다음과 같을 때
# 모분산이 25.0이라고 알려져 있을 때, '해당 라인에서 생산된 초콜릿의 무게는 
# 200이다' 라는 가설에 대한 검정 수행
df1 <- read.table('초콜릿.txt')
v1 <- unlist(df1)
names(v1) <- NULL

# 같은가? 양측검정
H0 : mu = 200
H1 : mu!= 200

mu <- 200
sigma <- sqrt(25)   # 5
xbar <- mean(v1)    # 199.46
n <- length(v1)

# 1) p-value
z* = (xbar - mu) / (sigma/sqrt(n)) = -0.7636753

p-value = P(|Z|>z*)
        = P(Z > -0.7636753)
        = pnorm(-0.7636753, mean = 0, sd = 1)
        = 0.2225304     >>>     0.025
          => H0 채택!

# 2) z검정 활용
library(BSDA)

z.test(v1, mu = 200, sigma.x = 5, conf.level = 0.95)

# 결과
One-sample z-Test

data:  v1
z = -0.76368, p-value = 0.4451    # 0.4451 <<< 0.05 이므로 H0 채택
alternative hypothesis: true mean is not equal to 200
95 percent confidence interval:
  198.0741 200.8459    # 95% 유의수준 신뢰구간
sample estimates:
  mean of x 
199.46     # 표본평균 xbar
# 결론 => 95% 유의수준에서 H0가 채택됨

# 시각화
v_x <- seq(-4, 4, 0.01)
v_y <- dnorm(v_x, 0, 1)

alpha <- 0.05

ld1 <- qnorm(alpha/2, 0, 1)
lu1 <- qnorm(1 - alpha/2, 0, 1)

f1 <- function(x) {
  dnorm(x, 0, 1)
}

dev.new()
plot(v_x, v_y, type = 'l', main = 'two.sided')

polygon(c(ld1, seq(ld1, -4, -0.01), -4),
        c(0, f1(seq(ld1, -4, -0.01)), 0), col = 'red')    # 왼쪽 기각역
text(ld1, 0, "-1.96")

polygon(c(lu1, seq(lu1, 4, 0.01), 4),
        c(0, f1(seq(lu1, 4, 0.01)), 0), col = 'red')    # 오른쪽 기각역
text(lu1, 0, "1.96")

z <- -0.76368
arrows(z, 0, z, f1(z), length = 0)

polygon(c(z, seq(-4, z, 0.01), z),
        c(0, f1(seq(-4, z, 0.01)), 0),
        density = 10)    # 유의확률 구간

# 그림에서 보듯 유의확률 구간이 양쪽 기각역 중 왼쪽 기각역을 포함하므로 H0 채택!


