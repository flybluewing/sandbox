
names(cutRstGrp)

cutRstLst <- cutRstGrp[["H960"]]

cutInfoLst.grp <- lapply( names(cutRstLst$cutRstLst) ,function( crName ){
    append( cutRstLst$cutRstLst[[crName]]$cutInfoLst ,cutRstLst$cutRstLstHCR[[crName]]$cutInfoLst )
})
names(cutInfoLst.grp) <- names(cutRstLst$cutRstLst)


cutInfoLst <- NULL
for( nIdx in names(cutInfoLst.grp) ){
    cutInfoLst <- append( cutInfoLst ,cutInfoLst.grp[[nIdx]] )
}



