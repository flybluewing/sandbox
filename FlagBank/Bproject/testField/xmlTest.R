setwd("C:\\zproject\\ISLRgit\\FlagBank\\Bproject\\testField")
# install.package("XML")
#   http://www.omegahat.net/RSXML/Tour.pdf
#       An Introduction to the XML package for R
#
#       - https://www.rdocumentation.org/packages/XML
#
library(XML)


doc <- xmlTreeParse("test.xml", getDTD = F)
xRoot <- xmlRoot(doc)
xmlName(xRoot)  ;xmlSize(xRoot)     ;xmlAttrs(xRoot)    ;names(xRoot)

xmlName(xRoot[[1]])     # 하위 첫번째 노드 객체(XMLNode) 접근
xRoot[1:2]              # 범위로 가져오기
xRoot[c("food")]    ;xRoot[c("food","foodToy")] # 맨 처음 발견된 하나씩만 가져오는 듯.(R 버전따라 차이나는 지 확인 요.)
xRoot[["food"]]


# 그룹함수 사용.
sapply( xmlChildren(xRoot[[1]]) ,xmlName )  ;
xmlSApply( xRoot[[1]] ,xmlName )    # xmlApply() etc...

# 값
xmlValue(xRoot[[1]])    #   XMLTextNode 객체 반환.
xRoot[[1]][["name"]][[1]]   # <name/> 은 XMLTextNode로서, Text문자열은 XMLTextNode의 자식으로서 소속되어 있음을 유의.

# 신규 노드 추가
bvrgNode <- xmlNode("beverage" 
                            ,attrs=c(info="season menu",limit="takeout only") 
                            ,xmlNode("name","ice coffee") ,xmlNode("price","1.5") 
                        )
xmlValue(bvrgNode[["price"]]) <- 2.1
xmlAttrs(bvrgNode[["price"]]) <- c( size="tall" )
xRoot[[1+xmlSize(xRoot)]] <- bvrgNode   #   xRoot[[2]] <- bvrgNode

#   파일 출력. (나름 pretty print도 나름 해 준다. ㅋㅋ)
saveXML( xRoot , file="out.xml")    # 

