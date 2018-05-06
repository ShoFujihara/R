library(magrittr)
cro <- function(Row,Column){
if(is.table(Row)) {
d <- data.frame(Row)
d <- d[rep(row.names(d), d$Freq), 1:2]
Row <- d[,1]
Column <- d[,2]
}
tbl <- table(Row,Column)
tblsum <- tbl %>% addmargins()
rowtbl <- tbl %>% addmargins(1) %>% prop.table(1) %>% addmargins(2) %>% round(3)*100
coltbl <- tbl %>% addmargins(2) %>% prop.table(2) %>% addmargins(1) %>% round(3)*100
chisq <- chisq.test(tbl)
V <- sqrt(chisq$statistic/sum(tbl)/(min(dim(tbl))-1))[[1]] %>% round(3)
list(N = sum(tbl),Table = tblsum, Row_Percent = rowtbl, Column_Percent = coltbl, Chi_squared_Test = chisq, V = V)
}
