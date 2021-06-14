# 외부 참조 라이브러리 선언.
if( FALSE ){
    pkgName <- c( "RSNNS" ,"snowfall" ,"gtools" ,"pryr" ,"magrittr" ,"XML" ,"ggplot2" ,"gcookbook" ,"plyr" )
    install.packages( pkgName )
}
require(RSNNS)
library(snowfall)
library(gtools)
library(pryr)
library(magrittr)
library(XML)
library(ggplot2)    ;library(gcookbook)     # gcookbook 은 테스트 데이터용도.
library(plyr)

# 공통 함수/헤더파일 파일 로딩.
source("kCommon.R")
source("Zoid_H.R")
source("DataProducer_H.R")

source("Filter_H.R")
source("FilterSAF_H.R")
source("FilterDPF_H.R")
source("RFilter_H.R")

source("FlagBank_H.R")


# source("FlagBank.R")