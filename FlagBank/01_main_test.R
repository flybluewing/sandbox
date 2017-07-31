# 01_main.R 함수에 최종 코드가 입력되기 전
# 개발용으로 사용하는 영역.

source("hCommon.R")

k1 <- 1:10%%2
k2 <- c(1,1,1,0,0 ,0,1,1,1,0)
kMtx <- table( k1 ,k2 )
kStr <- "myStr"

