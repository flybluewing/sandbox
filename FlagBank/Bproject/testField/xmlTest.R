setwd("C:\\zproject\\ISLRgit\\FlagBank\\Bproject\\testField")
# install.package("XML")
#   http://www.omegahat.net/RSXML/Tour.pdf
#       An Introduction to the XML package for R
#
#       - https://www.rdocumentation.org/packages/XML/versions/3.98-1.20/topics/saveXML
#
library(XML)


doc <- xmlTreeParse("test.xml", getDTD = F)
xRoot <- xmlRoot(doc)
xmlName(xRoot)  ;xmlSize(xRoot)     ;xmlAttrs(xRoot)
xmlName(xRoot[[1]])  ;xmlSize(xRoot[[1]])     ;xmlAttrs(xRoot[[1]]) # 하위 첫번째 노드

#   xmlApply(r[[1]], xmlName)
sapply( xmlChildren(xRoot[[1]]) ,xmlName )
xmlValue(xRoot[[1]])




saveXML( xRoot , file="out.xml")    # pretty print도 나름 해 준다. ㅋ

