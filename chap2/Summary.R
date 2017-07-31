
[Basic]
    length()
    rm( list=ls() )
    
    matrix( data=c(1,2,3,4), nrow=2, ncol=2 )
    matrix( data=c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE )

    sqrt(x) # x^2

    x = rnorm(50)
    y = x + rnorm(50, mean(50), sd=.1 )
    cor( x, y )

    seq( 1, 10 ); seq( 1, 10, 2 );  # 0, 2, 4, 6, 8, 10
    set.seed(3) #   난수생성 seed값 설정.
    y = rnorm(100)
    mean(y)
    var(y)
    sd(y)   # sqrt( var(y) )

<plot>
    x <- rnorm(100);    y <- rnorm(100);
    pdf("./chap2/Plot_XY.pdf")  # 현재 작업디렉토리의 하위 디렉토리인 chap2에 파일 생성.
    plot( x, y, xlab="x-axis", ylab="y-lab", main="plot of X vs Y", col="green" )
    dev.off()

<conture plot>
    x = seq( -pi, pi, length=50 )
    y = x
    f = outer( x, y, function(x,y)cos(y)/(1+x^2) )
    contour( x, y, f, nlevels=45, add=T )   # nlevels : 등고선 수
    image( x, y, f )    # 컬러로 z 축 값을 나타냄.
    persp( x, y, f, theta=30, phi=40 )

<indexing>
    A = matrix( 1:20, 4, 5 )
    A[ c(2,4), c(1,2) ];    A[1:2,];    A[-(1:2),]
    dim( A )    # 4 5


[Loading Data]
    Auto = read.table("./DataSet/Auto.data", header=T, na.strings="?")   # data frame
    fix( Auto )     # 데이터 내용보기
    names( Auto )
    Auto = na.omit(Auto)    # 빠진 데이터 제거라는 데.. 빠진 데이터가 뭔지 모르겠당.

<plot>
    plot( Auto$cylinders, Auto$mpg )
    with( Auto, plot(cylinders, mpg) )

    attach(Auto);   plot(cylinders,mpg)
    cylinders = as.factor(cylinders)    # for Qualitative
    plot( cylinders, mpg, col="red" )  # 한쪽이 factor이면 자동으로 box plot.
    plot( cylinders, mpg, varwidth=T, horizontal=T )    # varwidth:variable 반영, horizontal:x-y 위치전환

    hist( mpg )
    hist( mpg, breaks=15 )

    pairs( Auto )
    pairs( ~ mpg + displacement + horsepower, Auto )

<interactive>
    plot( horsepower, mpg, col=3 )
    identify( horsepower, mpg, name )   # 마우스 오른쪽 클릭으로 interaction모드 중지 가능

<summary>
    summary( Auto )

[exit]
    savehistory()   # loadhistory()  참고
