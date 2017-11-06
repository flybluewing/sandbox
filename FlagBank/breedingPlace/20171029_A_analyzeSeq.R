# analyzeSeq() 함수 사용에 대한 예시들.

# eleSet

# 분석대상 지정 객체 기본생성
caf <- eleCafUtil.getNew( eleSet )

# 일부만 분석대상으로 지정하기. (index를 4로 나눠, 1,2번째에 해당하는 column만 선택. 50%)
caf.1_2 <- eleCafUtil.remainder( caf ,pRemainder=c(1,2) ,pBase=4 )


# 지정된 대로, 50% 만 분석시행.
tStmp <- Sys.time()
hAnaSet.1_2 <- analyzeSeq( eleSet ,pCaf=caf.1_2 ,pDebug=T )
tDiff <- Sys.time() - tStmp
k.FLogStr(sprintf("hAnaSet.1_2 %.1f%s",tDiff,units(tDiff)),pConsole=T)

# 분석안된 인덱스들을 찾아내기.
#   분석안된 컬럼들이 T 로 표시된다.
caf.undone <- eleCafUtil.undone( pEleSet=eleSet ,pHAnaSet=hAnaSet.1_2 )

# 분석안된 나머지도 분석하자.
tStmp <- Sys.time()
hAnaSet.3_0 <- analyzeSeq( eleSet ,pCaf=caf.undone ,pDebug=T )
tDiff <- Sys.time() - tStmp
k.FLogStr(sprintf("hAnaSet.3_0 %.1f%s",tDiff,units(tDiff)),pConsole=T)


# 둘을 합쳐 봅시다.
hASLst <- list()
# 편의를 위해 모두 리스트에 넣고.
hASLst[[1]] <- hAnaSet.1_2      ;hASLst[[2]] <- hAnaSet.3_0
#   Debuging 확인 용.
# hASLst[[1]]$hLst[[2]]$anaLst[[2]][[1]]
# hASLst[[2]]$hLst[[2]]$anaLst[[2]][[1]]

hAS.f <- NULL # 최종결과로 합치기.
for( lIdx in seq_len(length(hASLst)) ){
    if( 1==lIdx ){
        hAS.f <- hASLst[[lIdx]]
        next
    }

    curAS <- hASLst[[lIdx]]
    for( hIdx in seq_len(length( hAS.f$hLst )) ){
        for( eIdx in seq_len(length( hAS.f$hLst[[hIdx]]$anaLst )) ){
            for( cIdx in seq_len(length( hAS.f$hLst[[hIdx]]$anaLst[[eIdx]] )) ){

                if( "seqAnaObj"!=class(hAS.f$hLst[[hIdx]]$anaLst[[eIdx]][[cIdx]]) ){
                    hAS.f$hLst[[hIdx]]$anaLst[[eIdx]][[cIdx]] <- curAS$hLst[[hIdx]]$anaLst[[eIdx]][[cIdx]]
                } # if

            } # cIdx
        } # eIdx
    } # for(hIdx)

} # lIdx

