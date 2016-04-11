Facebook粉絲團分析（分析專頁：朱立倫）
================

分析台北市長朱立倫粉絲專頁之每日發文數、likes數、comments數與shares數，資料分析區間為2016/01/01至2016/04/11

讀取朱立倫粉絲團資料
--------------------

``` r
if (!require('Rfacebook')){
  install.packages("Rfacebook")
  library(Rfacebook)
}
```

``` r
token <- 'EAACEdEose0cBAFXogp1QmxjsZC85d1D5P1lf0g0jZCzOPWcFSKgm77fk81T41xitiw7tvoIj84ZAbRHd9DegAstQbgnPCZAbzkIJFOZAzIj3HwNuC7dZAV8foDNYCitd6BykYsznz1MbkVGeDg8DE1LrNUZCRp4DSvfYkRsTMFGbAZDZD'
totalPage<-NULL
lastDate<-Sys.Date()
DateVectorStr<-as.character(seq(as.Date("2016-01-01"),lastDate,by="5 days"))
for(i in 1:(length(DateVectorStr)-1)){
  tempPage<-getPage("llchu",token,
                    since = DateVectorStr[i],until = DateVectorStr[i+1])
  totalPage<-rbind(totalPage,tempPage)
}
```

    ## 14 posts 13 posts 25 posts 4 posts 5 posts 5 posts 7 posts 5 posts 1 posts 6 posts 5 posts 3 posts 4 posts 6 posts 4 posts 8 posts 5 posts 5 posts 4 posts 5 posts

``` r
nrow(totalPage)
```

    ## [1] 134

2016/01/01至2016/04/11 柯文哲粉絲團一共有134篇文章

每日發文數分析
--------------

分析朱立倫粉絲團每天的發文數

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time,
                                 format =  "%Y-%m-%dT%H:%M:%S+0000",
                                 tz = "GMT")
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei")
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PC<-aggregate(id~dateTPE+weekdays,totalPage,length)  
library(knitr)
kable(head(PC[order(PC$id,decreasing = T),]))
```

|     | dateTPE    | weekdays |   id|
|-----|:-----------|:---------|----:|
| 14  | 2016-01-12 | 星期二   |    7|
| 27  | 2016-01-13 | 星期三   |    5|
| 37  | 2016-01-15 | 星期五   |    5|
| 71  | 2016-01-14 | 星期四   |    5|
| 68  | 2016-03-20 | 星期日   |    4|
| 1   | 2016-01-04 | 星期一   |    3|

從此表格可以看出朱立倫在2016-01-12發了最多文章，一共發了7篇。12到15日這幾天發文的頻率都很高，我認為可能是因為1/16是選舉日，所以發多一點文來拉票。

每日likes數分析
---------------

分析朱立倫粉絲團每天的likes數

``` r
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
LC<-aggregate(likes_count~dateTPE+weekdays,totalPage,mean)
library(knitr)
kable(head(LC[order(LC$likes_count,decreasing = T),]))
```

|     | dateTPE    | weekdays |  likes\_count|
|-----|:-----------|:---------|-------------:|
| 49  | 2016-01-16 | 星期六   |       83385.0|
| 51  | 2016-02-06 | 星期六   |       57639.0|
| 48  | 2016-01-09 | 星期六   |       52729.5|
| 37  | 2016-01-15 | 星期五   |       49404.6|
| 3   | 2016-01-18 | 星期一   |       46132.0|
| 6   | 2016-02-08 | 星期一   |       41877.0|

從此表格看出朱立倫在2016-01-16得到的讚數最多，那天平均得到了83385個like。我推測因為這天為選舉之日，朱立倫的粉絲們都關注著選情，還有朱立倫的動態，而他今天也po了謝票的文章，可以看出粉絲們都非常關心這場選舉。

每日comments數分析
------------------

分析朱立倫粉絲團每天的comments次數

``` r
CC<-aggregate(comments_count~dateTPE+weekdays,totalPage,mean)
library(knitr)
kable(head(CC[order(CC$comments_count,decreasing = T),]))
```

|     | dateTPE    | weekdays |  comments\_count|
|-----|:-----------|:---------|----------------:|
| 49  | 2016-01-16 | 星期六   |          10605.5|
| 37  | 2016-01-15 | 星期五   |           7843.6|
| 3   | 2016-01-18 | 星期一   |           3629.0|
| 48  | 2016-01-09 | 星期六   |           1883.0|
| 15  | 2016-01-19 | 星期二   |           1649.0|
| 51  | 2016-02-06 | 星期六   |           1377.0|

從此表格看出朱立倫在2016-01-16得到的comments最多，那天平均得到了10625.5個comments。我推測因為這天為選舉之日，朱立倫的粉絲們都關注著選情，還有朱立倫的動態，而他今天也po了謝票的文章，可以看出粉絲們都非常關心這場選舉。

每日文章被數分析
----------------

分析朱立倫粉絲團每天的shares次數。

``` r
SC<-aggregate(shares_count~dateTPE+weekdays,totalPage,mean)
library(knitr)
kable(head(SC[order(SC$shares_count,decreasing = T),]))
```

|     | dateTPE    | weekdays |  shares\_count|
|-----|:-----------|:---------|--------------:|
| 37  | 2016-01-15 | 星期五   |       2342.000|
| 35  | 2016-01-01 | 星期五   |       1520.667|
| 49  | 2016-01-16 | 星期六   |       1363.500|
| 51  | 2016-02-06 | 星期六   |       1264.000|
| 14  | 2016-01-12 | 星期二   |       1000.429|
| 48  | 2016-01-09 | 星期六   |        937.000|

從此表格看出朱立倫在2016-01-15文章被shares的次數最多，那天平均被分享了2342次。我認為可能朱立倫的粉絲在選舉前一天，要幫朱立倫拉票，所以大家都分享到自己的臉書，讓更多人看到以達到拉票的目的。
