
dict <- toupper( scan("dictionary.txt",what="character") )

blocks <- list(
    c("AACIOT"),
    c("AHMORS"),
    c("EGKLUY"),
    c("ABILTY"),
    c("ACDEMP"),
    c("EGINTV"),
    c("GILRUW"),
    c("ELPSTU"),
    c("DENOSW"),
    c("ACELRS"),
    c("ABJMOQ"),
    c("EEFHIY"),
    c("EHINPS"),
    c("DKNOTU"),
    c("ADENVZ"),
    c("BIFORX")
)

### new board:
dumbBoard <- function(x,y) matrix( sample(LETTERS, x*y, replace=TRUE), ncol=y,nrow=x )

newBoard <- function(x,y,blocks) {
    randLetters <- sapply( blocks, function(x) unlist(strsplit(x,""))[ sample(1:6,1) ] )
    return( matrix( sample(randLetters,length(randLetters)), ncol=y,nrow=x ) )
}

### test for a word match:
wordTest <- function(word) {
    indx <- grep( paste("^",word,sep=""), dict )
    full <- dict[ indx ] == word
    return( list(
         full = any( full ),
         partial = length(indx)>0
     ))
}

drawBoard <- function( board ) {
    a <- nrow(board)
    b <- ncol(board)
    par( mar=c(1,1,5,1) )
    plot( NULL, xlim=c(0.5,a+0.5),ylim=c(b+0.5,0.5) ,xaxs="i",yaxs="i",xaxt="n",yaxt="n",ann=FALSE)
    for(y in 1:nrow(board)) for(x in 1:ncol(board)) text(x,y,board[x,y],cex=8)
    abline(v=1:b+0.5)
    abline(h=1:a+0.5)
}

wordHL <- function( wdf ) {
    text( wdf$x, wdf$y, wdf$word, cex=8, col="red" )
    x01 <- cbind( wdf$x[ -nrow(wdf) ] , wdf$x[ -1 ] )
    y01 <- cbind( wdf$y[ -nrow(wdf) ] , wdf$y[ -1 ] )
    x01d <- apply( x01, 1, diff )
    y01d <- apply( y01, 1, diff )
    amt <- c(0.25,-0.25) # how much to cut from each end of the arrow
    for(i in 1:nrow(x01)) {
        if( x01d[i]==1 ) x01[i,] <- x01[i,]+ amt
        if( x01d[i]==-1 ) x01[i,] <- x01[i,]+ rev(amt)
        if( y01d[i]==1 ) y01[i,] <- y01[i,]+ amt
        if( y01d[i]==-1 ) y01[i,] <- y01[i,]+ rev(amt)
    }
    arrows( x0=x01[,1], x1=x01[,2],
          y0=y01[,1], y1=y01[,2],
          col=4,lwd=6)
    text( mean(par()$usr[1:2]), par()$usr[4], paste(wdf$word,collapse=""),xpd=TRUE,adj=c(0.5,-0.5),cex=4 )
}



searchBoard <- function( board1 ) {
fullW <- list()
cnt <- 1
for(x in 1:ncol(board1)) {
    for(y in 1:nrow(board1)) {
        let <- board1[x,y]
        print( paste("start: ",let) )
        wt <- wordTest( let )
        res <- list( full=list(), 
                    partial=list() )
        if( wt$partial ) {
            res$partial[[1]] <- data.frame( x=x,y=y,word=let,stringsAsFactors=FALSE)
            search <- TRUE
        } else search <- FALSE

        #cnt <- 1
        while( search ) {
            x1 <- res$partial[[1]][ nrow(res$partial[[1]]), 1]
            y1 <- res$partial[[1]][ nrow(res$partial[[1]]), 2]
            # expand search:
            xy1 <- expand.grid( 
                        x = c( (x1 -1) : (x1 +1) ),
                        y = c( (y1 -1) : (y1 +1) )
                        )
            # filter out-of-bounds coordinates:
            illIndx <- c(
                which( xy1[,1] < 1 | xy1[,1] > b ),
                which( xy1[,2] < 1 | xy1[,2] > a )
            )
            if( any( illIndx ) ) xy1 <- xy1[ -illIndx, ]
            # remove letters already used in this word:
            if( !any(is.na(res$partial[[1]] ) )) {
                blackL <- c()
                for(i in 1:nrow(xy1)) {
                    for(j in 1:nrow(res$partial[[1]] )) {
                        if( all( xy1[i,] == res$partial[[1]] [j,1:2] ) ) blackL <- c( blackL, i )
                    }
                }
                if( !is.null(blackL) ) xy1 <- xy1[ -blackL, ]
            }
            #continue only if there are viable places to go:
            if( nrow(xy1) > 0 ) { 
                xy1$word <- board1[ data.matrix(xy1) ]

                for(i in 1: nrow(xy1)) {
                    #new test word
                    nw <- paste( paste(res$partial[[1]]$word,collapse="") , xy1$word[i],sep="") 
                    print(nw)
                    wt1 <- wordTest( nw )
                    if( wt1$full & nchar(nw)>=3 ) {
                        res$full <- c(res$full, list( rbind(res$partial[[1]], c( xy1[i,] ) ) ) )
                        cat( "FULL WORD:", nw, "\n")
                        drawBoard( board1 )
                        wordHL( res$full[[ length(res$full) ]] )
                    }
                    if( wt1$partial ) {
                        #cat( "partial:", nw, "\n" )            
                        res$partial <- c( res$partial, list( rbind(res$partial[[1]], c( xy1[i,] ) ) ) )
                    } 
                    cnt <- cnt+1
                }
            }
            #after we look at every 'next' letter, remove the exhausted working partial word:
            if( length(res$partial)>1 ) {
                res$partial <- res$partial[ -1 ]
            } else {
                search <- FALSE
            }
        } # end while
        fullW <- c(fullW, res$full)
    } # end Y
} # end X
    return(fullW)
} # end of function

a <- b <- 4
board1 <- newBoard(a,b,blocks)
drawBoard( board1 )

readline("press enter to continue.")
#Rprof()
fullW <- searchBoard( board1 )
#Rprof(NULL); summaryRprof()

names(fullW) <- sapply(fullW, function(x) paste( x$word,collapse="") )

while(0) {
for(i in 1:length(fullW)) {
    drawBoard( board1 )
    wordHL( fullW[[i]] )
    Sys.sleep(0.25)
   # locator(1)
}
}

report <- data.frame(
    word = sapply(fullW, function(x) paste( x$word, collapse="") )
    ,nletters = sapply(fullW, nrow)
,stringsAsFactors=FALSE)
report$score <- report$nletters-2

report <- report[ order(-report$score),]
sum( report$score )


follow <- function( x,y,let, res ) {
    cat( "##########\n")
    #print(res)
    #cat( x, y, let, "\n")
    ### find surrounding coordinates:
    xy1 <- expand.grid( 
                x = c( (x-1) : (x+1) ),
                y = c( (y-1) : (y+1) )
                )
    # filter illegal coordinates:
    xy1 <- xy1[ -c( 
        which( xy1[,1] < 1 | xy1[,1] > b | ( xy1[,1] == x & xy1[,2] == y ) ) ,
        which( xy1[,2] < 1 | xy1[,2] > a )
    ), ]
    # already used letters:
    if( !any(is.na(res$used) )) {
    blackL <- c()
    for(i in 1:nrow(xy1)) {
        for(j in 1:nrow(res$used)) {
            if( all( xy1[i,] == res$used[j,1:2] ) ) blackL <- c( blackL, i )
        }
    }
    if( !is.null(blackL) ) xy1 <- xy1[ -blackL, ]
    }
    xy1$let <- board1[ data.matrix(xy1) ]
    for(i in 1: nrow(xy1)) {
        nw <- paste(let,xy1$let[i],sep="") #new test word
        wt1 <- wordTest( nw )
        if(wt1$full) {
            res$full <- c(res$full, ( rbind(res$used, c( xy1[i,1:2], word=nw ) ) ) )
            cat( "\n\nFULL WORD:", nw, "\n\n")
        }
        if(wt1$partial) {
            cat( "partial:", nw, "\n" )            
            res$used <- rbind(res$used, c( xy1[i,1:2], word=nw ) )
            res <- follow( xy1[i,1], xy1[i,2], nw, res)
            #follow( xy1[i,1], xy1[i,2], nw, res)
            #print(res)
        } # else { res$used <- NA }
    }
#    res$used <- res$used[ -nrow(res$used) ,]
#    browser("end")
    cat( "full=",wt1$full, "\tpartial=",wt1$partial, "\n")
    if( wt1$partial | wt1$full ) return( res ) else return(NULL)
}
#res <- follow( x, y, let, res )
            








