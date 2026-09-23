# Common effor unit (CEU)

## Overview

This vignette provides an overview of the steps to generate
fishery-specific weekly common effort units (CEUs), using a subset of
anonymised landings data. Data have been subjected to quality controls.
Each row is associated with a fishing area (column ‘fa’). Catch amounts
have been removed from the data.

Functions from the `fisheriescape` R package are used.

## R packages

### CRAN R packages

``` r

library(dplyr)
```

### GitHub R packages

The following packages are all available from
<https://jolenesutton.github.io>.

``` r

library(fisheriescape) 
```

## Get landings data

### Landings data

``` r


df<-fs.data.anon

names(df)
#>  [1] "fishery"      "event.id"     "trip.id"      "cfv.anon"     "year"        
#>  [6] "dateland"     "ctchdate"     "sw"           "nafodiv"      "depth.gebco" 
#> [11] "x"            "y"            "gear.name"    "gear.amount"  "hours.fished"
#> [16] "days.fished"  "fa"
```

|                  |  2015 | 2016 |
|:-----------------|------:|-----:|
| Atlantic halibut |   856 |  879 |
| Snow crab        | 10688 | 9731 |
| Winter flounder  |   498 |  547 |

Records per year {.table .table .table-striped .table-hover
.table-condensed style="margin-left: auto; margin-right: auto;"}

| fishery | event.id | trip.id | cfv.anon | year | dateland | ctchdate | sw | nafodiv | depth.gebco | x | y | gear.name | gear.amount | hours.fished | days.fished | fa |
|:---|:---|:---|:---|---:|:---|:---|:---|:---|---:|---:|---:|:---|---:|---:|---:|:---|
| Atlantic halibut | ahx319;2015-07-13;2015-07-10;49.49083;-63.851 | ahx319;2015-07-13;2015-07-10 | ahx319 | 2015 | 2015-07-13 | 2015-07-10 | 29 | 4S | -192.0005 | 2244026 | 1586757 | longline | 12 | 6 | 1 | GQ_GFA_4S4 |
| Atlantic halibut | ahx319;2015-07-13;2015-07-11;49.47217;-63.84033 | ahx319;2015-07-13;2015-07-11 | ahx319 | 2015 | 2015-07-13 | 2015-07-11 | 29 | 4S | -213.9925 | 2245667 | 1585274 | longline | 12 | 6 | 1 | GQ_GFA_4S4 |
| Atlantic halibut | ahx149;2015-08-10;2015-08-08;50.043;-59.21133 | ahx149;2015-08-10;2015-08-08 | ahx149 | 2015 | 2015-08-10 | 2015-08-08 | 33 | 4S | -209.9678 | 2505219 | 1804340 | longline | 6 | 6 | 1 | GQ_GFA_4S5 |
| Atlantic halibut | ahx149;2015-08-10;2015-08-09;50.043;-59.21 | ahx149;2015-08-10;2015-08-09 | ahx149 | 2015 | 2015-08-10 | 2015-08-09 | 33 | 4S | -209.9678 | 2505301 | 1804390 | longline | 6 | 12 | 1 | GQ_GFA_4S5 |
| Atlantic halibut | ahx149;2015-08-13;2015-08-12;50.34833;-59.55267 | ahx149;2015-08-13;2015-08-12 | ahx149 | 2015 | 2015-08-13 | 2015-08-12 | 33 | 4S | -127.8552 | 2466770 | 1820762 | longline | 6 | 6 | 1 | GQ_GFA_4S5 |
| Atlantic halibut | ahx149;2015-08-24;2015-08-22;50.26633;-59.586 | ahx149;2015-08-24;2015-08-22 | ahx149 | 2015 | 2015-08-24 | 2015-08-22 | 35 | 4S | -141.9655 | 2469449 | 1811720 | longline | 6 | 4 | 1 | GQ_GFA_4S5 |

head(df) {.table .table
style="width: auto !important; margin-left: auto; margin-right: auto;"}

  
  
There are three different gear types, and there are missing values in
columns ‘gear.amount’, ‘hours.fished’, and ‘days.fished’.

``` r

# gear types
table(df$gear.name)
#> 
#>  gillnet longline     trap 
#>     1045     1735    20419

# gear amounts
tapply(df$gear.amount,df$fishery,summary)
#> $`Atlantic halibut`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>    6.00    6.00   12.00   12.37   18.00   48.00    1049 
#> 
#> $`Snow crab`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>    1.00   25.00   50.00   58.84   80.00  403.00     164 
#> 
#> $`Winter flounder`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>   1.000   4.000   5.000   6.785   7.000  30.000      25

# hours fished
tapply(df$hours.fished,df$fishery,summary)
#> $`Atlantic halibut`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>    1.00    4.00    8.00    9.39   12.00   24.00     471 
#> 
#> $`Snow crab`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>      24      24      24      24      24      24 
#> 
#> $`Winter flounder`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>    1.00   24.00   24.00   22.51   24.00   24.00      18

# days fished
tapply(df$days.fished,df$fishery,summary)
#> $`Atlantic halibut`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>   1.000   1.000   1.000   1.032   1.000   3.000     471 
#> 
#> $`Snow crab`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>      NA      NA      NA     NaN      NA      NA   20419 
#> 
#> $`Winter flounder`
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>   1.000   1.000   1.500   1.727   2.000   5.000      18
```

## Summarize trips

First, set ‘hours.fished’ (i.e. daily soak time) to 24.

``` r

df[which(df$gear.name=='trap'),'hours.fished']<-24
```

  
  
Correct days fished before summarizing

``` r

index<-which(df$days.fished==0)
if(length(index)>0){
  df[index,'days.fished']<-NA
}
```

### Summarize trips for trap fisheries

``` r

df.trap<-df[which(df$gear.name=="trap"),]
df.non.trap<-df[which(df$gear.name!="trap"),]

trips.trap<-fs_summarize_trips(df.trap,
                          gear.type="trap",
                          group.cols=c('nafodiv','fishery','gear.name','trip.id','fa'),
                          gear.col='gear.amount',
                          hour.col='hours.fished')



trips.non.trap<-fs_summarize_trips(df.non.trap,
                          gear.type="non.trap",
                          group.cols=c('nafodiv','fishery','gear.name','trip.id','fa'),
                          gear.col='gear.amount',
                          hour.col='hours.fished',
                          day.col='days.fished')

summary(trips.trap)
#>       fishery           event.id          trip.id           cfv.anon    
#>  Length   :20419   Length   :20419   Length   :20419   Length   :20419  
#>  N.unique :    1   N.unique :20098   N.unique :15758   N.unique :  625  
#>  N.blank  :    0   N.blank  :    0   N.blank  :    0   N.blank  :    0  
#>  Min.nchar:    9   Min.nchar:   34   Min.nchar:   28   Min.nchar:    6  
#>  Max.nchar:    9   Max.nchar:   63   Max.nchar:   28   Max.nchar:    6  
#>                                                                         
#>                                                                         
#>       year           dateland          ctchdate             sw       
#>  Min.   :2015   Length   :20419   Length   :20419   Length   :20419  
#>  1st Qu.:2015   N.unique :  276   N.unique :  277   N.unique :   23  
#>  Median :2015   N.blank  :    0   N.blank  :    0   N.blank  :    0  
#>  Mean   :2015   Min.nchar:   10   Min.nchar:   10   Min.nchar:    2  
#>  3rd Qu.:2016   Max.nchar:   10   Max.nchar:   10   Max.nchar:    2  
#>  Max.   :2016                                                        
#>                                                                      
#>       nafodiv       depth.gebco            x                 y          
#>  Length   :20419   Min.   :-479.01   Min.   :1645552   Min.   :1284374  
#>  N.unique :    2   1st Qu.:-110.03   1st Qu.:2245140   1st Qu.:1412839  
#>  N.blank  :    0   Median : -85.00   Median :2357087   Median :1459616  
#>  Min.nchar:    2   Mean   : -94.94   Mean   :2329768   Mean   :1510140  
#>  Max.nchar:    2   3rd Qu.: -72.00   3rd Qu.:2465386   3rd Qu.:1589473  
#>                    Max.   : -40.00   Max.   :2597402   Max.   :2906314  
#>                    NAs    :1037      NAs    :1036      NAs    :1036     
#>      gear.name      gear.amount      hours.fished  days.fished   
#>  Length   :20419   Min.   :  1.00   Min.   :24    Min.   : NA    
#>  N.unique :    1   1st Qu.: 25.00   1st Qu.:24    1st Qu.: NA    
#>  N.blank  :    0   Median : 50.00   Median :24    Median : NA    
#>  Min.nchar:    4   Mean   : 58.84   Mean   :24    Mean   :NaN    
#>  Max.nchar:    4   3rd Qu.: 80.00   3rd Qu.:24    3rd Qu.: NA    
#>                    Max.   :403.00   Max.   :24    Max.   : NA    
#>                    NAs    :164                    NAs    :20419  
#>          fa             GEAR             HRS       DAYS            sum.gear    
#>  Length   :20419   Min.   :  1.00   Min.   :24   Mode:logical   Min.   :  1.0  
#>  N.unique :   16   1st Qu.: 25.00   1st Qu.:24   NAs :20419     1st Qu.: 49.0  
#>  N.blank  :    0   Median : 50.00   Median :24                  Median : 75.0  
#>  Min.nchar:    9   Mean   : 58.84   Mean   :24                  Mean   : 79.6  
#>  Max.nchar:   10   3rd Qu.: 80.00   3rd Qu.:24                  3rd Qu.:100.0  
#>                    Max.   :403.00   Max.   :24                  Max.   :433.0  
#>                    NAs    :164                                  NAs    :175    
#>    max.hours 
#>  Min.   :24  
#>  1st Qu.:24  
#>  Median :24  
#>  Mean   :24  
#>  3rd Qu.:24  
#>  Max.   :24  
#> 
summary(trips.non.trap)
#>       fishery          event.id         trip.id          cfv.anon   
#>  Length   :2780   Length   :2780   Length   :2780   Length   :2780  
#>  N.unique :   2   N.unique :2778   N.unique :2497   N.unique : 249  
#>  N.blank  :   0   N.blank  :   0   N.blank  :   0   N.blank  :   0  
#>  Min.nchar:  15   Min.nchar:  34   Min.nchar:  28   Min.nchar:   6  
#>  Max.nchar:  16   Max.nchar:  63   Max.nchar:  28   Max.nchar:   6  
#>                                                                     
#>                                                                     
#>       year           dateland         ctchdate            sw      
#>  Min.   :2015   Length   :2780   Length   :2780   Length   :2780  
#>  1st Qu.:2015   N.unique : 307   N.unique : 341   N.unique :  31  
#>  Median :2016   N.blank  :   0   N.blank  :   0   N.blank  :   0  
#>  Mean   :2016   Min.nchar:  10   Min.nchar:  10   Min.nchar:   2  
#>  3rd Qu.:2016   Max.nchar:  10   Max.nchar:  10   Max.nchar:   2  
#>  Max.   :2016                                                     
#>                                                                   
#>       nafodiv      depth.gebco           x                 y          
#>  Length   :2780   Min.   :-357.0   Min.   :1924319   Min.   :1275679  
#>  N.unique :   2   1st Qu.:-156.0   1st Qu.:2249894   1st Qu.:1373327  
#>  N.blank  :   0   Median : -28.0   Median :2469477   Median :1448292  
#>  Min.nchar:   2   Mean   : -71.2   Mean   :2387935   Mean   :1451083  
#>  Max.nchar:   2   3rd Qu.: -14.0   3rd Qu.:2492088   3rd Qu.:1497197  
#>                   Max.   : 331.9   Max.   :2597338   Max.   :1997246  
#>                   NAs    :125      NAs    :125       NAs    :125      
#>      gear.name     gear.amount     hours.fished    days.fished   
#>  Length   :2780   Min.   : 1.00   Min.   : 1.00   Min.   :1.000  
#>  N.unique :   2   1st Qu.: 5.00   1st Qu.: 7.00   1st Qu.:1.000  
#>  N.blank  :   0   Median : 6.00   Median :16.00   Median :1.000  
#>  Min.nchar:   7   Mean   : 9.03   Mean   :15.27   Mean   :1.344  
#>  Max.nchar:   8   3rd Qu.:12.00   3rd Qu.:24.00   3rd Qu.:1.000  
#>                   Max.   :48.00   Max.   :24.00   Max.   :5.000  
#>                   NAs    :1074    NAs    :489     NAs    :489    
#>          fa            GEAR            HRS             DAYS      
#>  Length   :2780   Min.   : 1.00   Min.   : 1.00   Min.   :1.000  
#>  N.unique :  17   1st Qu.: 5.00   1st Qu.: 7.00   1st Qu.:1.000  
#>  N.blank  :   0   Median : 6.00   Median :16.00   Median :1.000  
#>  Min.nchar:  10   Mean   : 9.03   Mean   :15.27   Mean   :1.344  
#>  Max.nchar:  11   3rd Qu.:12.00   3rd Qu.:24.00   3rd Qu.:1.000  
#>                   Max.   :48.00   Max.   :24.00   Max.   :5.000  
#>                   NAs    :1074    NAs    :489     NAs    :489    
#>     sum.gear        max.hours        av.days     
#>  Min.   : 1.000   Min.   : 1.00   Min.   :1.000  
#>  1st Qu.: 5.000   1st Qu.: 7.00   1st Qu.:1.000  
#>  Median : 6.000   Median :16.00   Median :1.000  
#>  Mean   : 9.601   Mean   :15.31   Mean   :1.342  
#>  3rd Qu.:12.000   3rd Qu.:24.00   3rd Qu.:1.000  
#>  Max.   :48.000   Max.   :24.00   Max.   :5.000  
#>  NAs    :1074     NAs    :479     NAs    :479
```

  
  
Restrict columns and remove duplicate rows.

``` r

COLS<-c('nafodiv','fishery','gear.name','fa','trip.id',
        "cfv.anon",'year','sw',
        'sum.gear','max.hours')

trips.trap<-trips.trap[,COLS]
trips.trap<-distinct(trips.trap)
rm(COLS)

COLS<-c('nafodiv','fishery','gear.name','fa','trip.id',
        "cfv.anon",'year','sw',
        'sum.gear','max.hours','av.days')
trips.non.trap<-trips.non.trap[,COLS]
trips.non.trap<-distinct(trips.non.trap)
rm(COLS)

head(trips.trap)
#>   nafodiv   fishery gear.name         fa                      trip.id cfv.anon
#> 1      4S Snow crab      trap GQ_CFA_12C scx444;2015-05-09;2015-05-09   scx444
#> 2      4S Snow crab      trap GQ_CFA_12C scx444;2015-05-12;2015-05-12   scx444
#> 3      4S Snow crab      trap GQ_CFA_12C scx444;2015-05-16;2015-05-16   scx444
#> 4      4S Snow crab      trap GQ_CFA_12C scx444;2015-05-18;2015-05-18   scx444
#> 5      4S Snow crab      trap GQ_CFA_12C scx444;2015-05-22;2015-05-22   scx444
#> 6      4S Snow crab      trap  GQ_CFA_14 scx444;2015-05-24;2015-05-23   scx444
#>   year sw sum.gear max.hours
#> 1 2015 19      150        24
#> 2 2015 20      150        24
#> 3 2015 20      150        24
#> 4 2015 21      150        24
#> 5 2015 21      150        24
#> 6 2015 21      140        24
head(trips.non.trap)
#>   nafodiv          fishery gear.name         fa                      trip.id
#> 1      4S Atlantic halibut  longline GQ_GFA_4S4 ahx319;2015-07-13;2015-07-10
#> 2      4S Atlantic halibut  longline GQ_GFA_4S4 ahx319;2015-07-13;2015-07-11
#> 3      4S Atlantic halibut  longline GQ_GFA_4S5 ahx149;2015-08-10;2015-08-08
#> 4      4S Atlantic halibut  longline GQ_GFA_4S5 ahx149;2015-08-10;2015-08-09
#> 5      4S Atlantic halibut  longline GQ_GFA_4S5 ahx149;2015-08-13;2015-08-12
#> 6      4S Atlantic halibut  longline GQ_GFA_4S5 ahx149;2015-08-24;2015-08-22
#>   cfv.anon year sw sum.gear max.hours av.days
#> 1   ahx319 2015 29       12         6       1
#> 2   ahx319 2015 29       12         6       1
#> 3   ahx149 2015 33        6         6       1
#> 4   ahx149 2015 33        6        12       1
#> 5   ahx149 2015 33        6         6       1
#> 6   ahx149 2015 35        6         4       1

summary(trips.trap)
#>       nafodiv           fishery          gear.name             fa       
#>  Length   :15779   Length   :15779   Length   :15779   Length   :15779  
#>  N.unique :    2   N.unique :    1   N.unique :    1   N.unique :   16  
#>  N.blank  :    0   N.blank  :    0   N.blank  :    0   N.blank  :    0  
#>  Min.nchar:    2   Min.nchar:    9   Min.nchar:    4   Min.nchar:    9  
#>  Max.nchar:    2   Max.nchar:    9   Max.nchar:    4   Max.nchar:   10  
#>                                                                         
#>                                                                         
#>       trip.id           cfv.anon          year              sw       
#>  Length   :15779   Length   :15779   Min.   :2015   Length   :15779  
#>  N.unique :15758   N.unique :  625   1st Qu.:2015   N.unique :   23  
#>  N.blank  :    0   N.blank  :    0   Median :2015   N.blank  :    0  
#>  Min.nchar:   28   Min.nchar:    6   Mean   :2015   Min.nchar:    2  
#>  Max.nchar:   28   Max.nchar:    6   3rd Qu.:2016   Max.nchar:    2  
#>                                      Max.   :2016                    
#>                                                                      
#>     sum.gear        max.hours 
#>  Min.   :  1.00   Min.   :24  
#>  1st Qu.: 41.00   1st Qu.:24  
#>  Median : 75.00   Median :24  
#>  Mean   : 76.27   Mean   :24  
#>  3rd Qu.:100.00   3rd Qu.:24  
#>  Max.   :433.00   Max.   :24  
#>  NAs    :160
summary(trips.non.trap)
#>       nafodiv          fishery         gear.name            fa      
#>  Length   :2512   Length   :2512   Length   :2512   Length   :2512  
#>  N.unique :   2   N.unique :   2   N.unique :   2   N.unique :  17  
#>  N.blank  :   0   N.blank  :   0   N.blank  :   0   N.blank  :   0  
#>  Min.nchar:   2   Min.nchar:  15   Min.nchar:   7   Min.nchar:  10  
#>  Max.nchar:   2   Max.nchar:  16   Max.nchar:   8   Max.nchar:  11  
#>                                                                     
#>                                                                     
#>       trip.id          cfv.anon         year              sw      
#>  Length   :2512   Length   :2512   Min.   :2015   Length   :2512  
#>  N.unique :2497   N.unique : 249   1st Qu.:2015   N.unique :  31  
#>  N.blank  :   0   N.blank  :   0   Median :2016   N.blank  :   0  
#>  Min.nchar:  28   Min.nchar:   6   Mean   :2016   Min.nchar:   2  
#>  Max.nchar:  28   Max.nchar:   6   3rd Qu.:2016   Max.nchar:   2  
#>                                    Max.   :2016                   
#>                                                                   
#>     sum.gear        max.hours        av.days     
#>  Min.   : 1.000   Min.   : 1.00   Min.   :1.000  
#>  1st Qu.: 5.000   1st Qu.: 8.00   1st Qu.:1.000  
#>  Median : 6.000   Median :18.00   Median :1.000  
#>  Mean   : 9.393   Mean   :15.85   Mean   :1.364  
#>  3rd Qu.:12.000   3rd Qu.:24.00   3rd Qu.:1.500  
#>  Max.   :48.000   Max.   :24.00   Max.   :5.000  
#>  NAs    :872      NAs    :361     NAs    :361
```

#### Fill in missing

``` r

trips.trap2<-suppressMessages(fs_fill_missing(trips.trap,
                        gear.type='trap',
                        vessel.col='cfv.anon',
                        year.col='year',
                        week.col='sw',
                        also.grp=c('nafodiv',"fishery" ,"gear.name")))

trips.non.trap2<-suppressMessages(fs_fill_missing(trips.non.trap,
                        gear.type='non.trap',
                        vessel.col='cfv.anon',
                        year.col='year',
                        week.col='sw',
                        also.grp=c('nafodiv',"fishery" ,"gear.name")))


summary(trips.trap2)
#>       nafodiv           fishery          gear.name             fa       
#>  Length   :15779   Length   :15779   Length   :15779   Length   :15779  
#>  N.unique :    2   N.unique :    1   N.unique :    1   N.unique :   16  
#>  N.blank  :    0   N.blank  :    0   N.blank  :    0   N.blank  :    0  
#>  Min.nchar:    2   Min.nchar:    9   Min.nchar:    4   Min.nchar:    9  
#>  Max.nchar:    2   Max.nchar:    9   Max.nchar:    4   Max.nchar:   10  
#>                                                                         
#>       trip.id           cfv.anon          year              sw       
#>  Length   :15779   Length   :15779   Min.   :2015   Length   :15779  
#>  N.unique :15758   N.unique :  625   1st Qu.:2015   N.unique :   23  
#>  N.blank  :    0   N.blank  :    0   Median :2015   N.blank  :    0  
#>  Min.nchar:   28   Min.nchar:    6   Mean   :2015   Min.nchar:    2  
#>  Max.nchar:   28   Max.nchar:    6   3rd Qu.:2016   Max.nchar:    2  
#>                                      Max.   :2016                    
#>     sum.gear        max.hours 
#>  Min.   :  1.00   Min.   :24  
#>  1st Qu.: 42.00   1st Qu.:24  
#>  Median : 75.00   Median :24  
#>  Mean   : 76.26   Mean   :24  
#>  3rd Qu.:100.00   3rd Qu.:24  
#>  Max.   :433.00   Max.   :24
summary(trips.non.trap2)
#>       nafodiv          fishery         gear.name            fa      
#>  Length   :2512   Length   :2512   Length   :2512   Length   :2512  
#>  N.unique :   2   N.unique :   2   N.unique :   2   N.unique :  17  
#>  N.blank  :   0   N.blank  :   0   N.blank  :   0   N.blank  :   0  
#>  Min.nchar:   2   Min.nchar:  15   Min.nchar:   7   Min.nchar:  10  
#>  Max.nchar:   2   Max.nchar:  16   Max.nchar:   8   Max.nchar:  11  
#>                                                                     
#>       trip.id          cfv.anon         year              sw      
#>  Length   :2512   Length   :2512   Min.   :2015   Length   :2512  
#>  N.unique :2497   N.unique : 249   1st Qu.:2015   N.unique :  31  
#>  N.blank  :   0   N.blank  :   0   Median :2016   N.blank  :   0  
#>  Min.nchar:  28   Min.nchar:   6   Mean   :2016   Min.nchar:   2  
#>  Max.nchar:  28   Max.nchar:   6   3rd Qu.:2016   Max.nchar:   2  
#>                                    Max.   :2016                   
#>     sum.gear       max.hours       av.days     
#>  Min.   : 1.00   Min.   : 1.0   Min.   :1.000  
#>  1st Qu.: 5.00   1st Qu.: 8.0   1st Qu.:1.000  
#>  Median : 9.00   Median :13.5   Median :1.000  
#>  Mean   :11.15   Mean   :15.1   Mean   :1.315  
#>  3rd Qu.:15.00   3rd Qu.:24.0   3rd Qu.:1.000  
#>  Max.   :48.00   Max.   :24.0   Max.   :5.000
```

## Summarize vessels

``` r

vessels.trap<-fs_summarize_vessels(trips.trap2,
                               gear.type="trap",
                               vessel.col="cfv.anon",
                               week.col="sw",
                               year.col="year",
                               fishing.area.col="fa",
                               also.grp=c('nafodiv',"fishery" ,"gear.name"))


vessels.non.trap<-fs_summarize_vessels(trips.non.trap2,
                               gear.type="non.trap",
                               vessel.col="cfv.anon",
                               week.col="sw",
                               year.col="year",
                               fishing.area.col="fa",
                               also.grp=c('nafodiv',"fishery" ,"gear.name"))


summary(vessels.trap)
#>       nafodiv          fishery         gear.name         cfv.anon   
#>  Length   :7351   Length   :7351   Length   :7351   Length   :7351  
#>  N.unique :   2   N.unique :   1   N.unique :   1   N.unique : 625  
#>  N.blank  :   0   N.blank  :   0   N.blank  :   0   N.blank  :   0  
#>  Min.nchar:   2   Min.nchar:   9   Min.nchar:   4   Min.nchar:   6  
#>  Max.nchar:   2   Max.nchar:   9   Max.nchar:   4   Max.nchar:   6  
#>                                                                     
#>          fa            year              sw            gear           hours   
#>  Length   :7351   Min.   :2015   Length   :7351   Min.   :  1.0   Min.   :24  
#>  N.unique :  16   1st Qu.:2015   N.unique :  23   1st Qu.: 55.0   1st Qu.:24  
#>  N.blank  :   0   Median :2015   N.blank  :   0   Median : 83.0   Median :24  
#>  Min.nchar:   9   Mean   :2015   Min.nchar:   2   Mean   : 87.5   Mean   :24  
#>  Max.nchar:  10   3rd Qu.:2016   Max.nchar:   2   3rd Qu.:120.0   3rd Qu.:24  
#>                   Max.   :2016                    Max.   :433.0   Max.   :24
summary(vessels.non.trap)
#>       nafodiv          fishery         gear.name         cfv.anon   
#>  Length   :1301   Length   :1301   Length   :1301   Length   :1301  
#>  N.unique :   2   N.unique :   2   N.unique :   2   N.unique : 249  
#>  N.blank  :   0   N.blank  :   0   N.blank  :   0   N.blank  :   0  
#>  Min.nchar:   2   Min.nchar:  15   Min.nchar:   7   Min.nchar:   6  
#>  Max.nchar:   2   Max.nchar:  16   Max.nchar:   8   Max.nchar:   6  
#>                                                                     
#>          fa            year              sw            gear      
#>  Length   :1301   Min.   :2015   Length   :1301   Min.   : 1.00  
#>  N.unique :  17   1st Qu.:2015   N.unique :  31   1st Qu.: 6.00  
#>  N.blank  :   0   Median :2016   N.blank  :   0   Median :10.00  
#>  Min.nchar:  10   Mean   :2016   Min.nchar:   2   Mean   :11.59  
#>  Max.nchar:  11   3rd Qu.:2016   Max.nchar:   2   3rd Qu.:15.00  
#>                   Max.   :2016                    Max.   :36.00  
#>      hours            days     
#>  Min.   : 1.00   Min.   :1.00  
#>  1st Qu.: 8.00   1st Qu.:1.00  
#>  Median :12.00   Median :1.00  
#>  Mean   :13.72   Mean   :1.27  
#>  3rd Qu.:24.00   3rd Qu.:1.00  
#>  Max.   :24.00   Max.   :5.00
```

## Summarize fishing areas

``` r

fareas.trap<-fs_summarize_fishing_areas(vessels.trap,
                               gear.type="trap",
                               week.col="sw",
                               year.col="year",
                               fishing.area.col="fa",
                               also.grp=c('nafodiv',"fishery" ,"gear.name")
                               )


fareas.non.trap<-fs_summarize_fishing_areas(vessels.non.trap,
                               gear.type="non.trap",
                               week.col="sw",
                               year.col="year",
                               fishing.area.col="fa",
                               also.grp=c('nafodiv',"fishery" ,"gear.name")
                               )


summary(fareas.trap)
#>       nafodiv         fishery        gear.name           fa           year     
#>  Length   :371   Length   :371   Length   :371   Length   :371   Min.   :2015  
#>  N.unique :  2   N.unique :  1   N.unique :  1   N.unique : 16   1st Qu.:2015  
#>  N.blank  :  0   N.blank  :  0   N.blank  :  0   N.blank  :  0   Median :2016  
#>  Min.nchar:  2   Min.nchar:  9   Min.nchar:  4   Min.nchar:  9   Mean   :2016  
#>  Max.nchar:  2   Max.nchar:  9   Max.nchar:  4   Max.nchar: 10   3rd Qu.:2016  
#>                                                                  Max.   :2016  
#>          sw        total.gear        soak.time 
#>  Length   :371   Min.   :   17.0   Min.   :24  
#>  N.unique : 23   1st Qu.:  217.5   1st Qu.:24  
#>  N.blank  :  0   Median :  588.0   Median :24  
#>  Min.nchar:  2   Mean   : 1733.6   Mean   :24  
#>  Max.nchar:  2   3rd Qu.: 1339.5   3rd Qu.:24  
#>                  Max.   :25371.0   Max.   :24
summary(fareas.non.trap)
#>       nafodiv         fishery        gear.name           fa           year     
#>  Length   :309   Length   :309   Length   :309   Length   :309   Min.   :2015  
#>  N.unique :  2   N.unique :  2   N.unique :  2   N.unique : 17   1st Qu.:2015  
#>  N.blank  :  0   N.blank  :  0   N.blank  :  0   N.blank  :  0   Median :2015  
#>  Min.nchar:  2   Min.nchar: 15   Min.nchar:  7   Min.nchar: 10   Mean   :2015  
#>  Max.nchar:  2   Max.nchar: 16   Max.nchar:  8   Max.nchar: 11   3rd Qu.:2016  
#>                                                                  Max.   :2016  
#>          sw        total.gear       soak.time          days      
#>  Length   :309   Min.   :  2.00   Min.   : 1.00   Min.   :1.000  
#>  N.unique : 31   1st Qu.: 12.00   1st Qu.: 8.00   1st Qu.:1.000  
#>  N.blank  :  0   Median : 21.00   Median :12.07   Median :1.000  
#>  Min.nchar:  2   Mean   : 48.79   Mean   :14.22   Mean   :1.178  
#>  Max.nchar:  2   3rd Qu.: 45.00   3rd Qu.:23.60   3rd Qu.:1.056  
#>                  Max.   :630.00   Max.   :24.00   Max.   :3.000
```

## Calculate proportion of week fished

``` r


prop.week.trap<-fs_proportion_week_fished(df=df.trap,
                                    fish.area.summary=fareas.trap,
                                    gear.type="trap",
                                    week.col="sw",
                                    fishing.area.col="fa",
                                    also.grp=c('year','nafodiv',"fishery" ,"gear.name")
                                    )
  

prop.week.non.trap<-fs_proportion_week_fished(df=df.non.trap,
                                    fish.area.summary=fareas.non.trap,
                                    gear.type="non.trap",
                                    week.col="sw",
                                    fishing.area.col="fa",
                                    also.grp=c('year','nafodiv',"fishery" ,"gear.name")
                                    )

summary(prop.week.trap)
#>       nafodiv         fishery        gear.name           fa           year     
#>  Length   :371   Length   :371   Length   :371   Length   :371   Min.   :2015  
#>  N.unique :  2   N.unique :  1   N.unique :  1   N.unique : 16   1st Qu.:2015  
#>  N.blank  :  0   N.blank  :  0   N.blank  :  0   N.blank  :  0   Median :2016  
#>  Min.nchar:  2   Min.nchar:  9   Min.nchar:  4   Min.nchar:  9   Mean   :2016  
#>  Max.nchar:  2   Max.nchar:  9   Max.nchar:  4   Max.nchar: 10   3rd Qu.:2016  
#>                                                                  Max.   :2016  
#>          sw        total.gear        soak.time  prop.week.fished
#>  Length   :371   Min.   :   17.0   Min.   :24   Min.   :0.1429  
#>  N.unique : 23   1st Qu.:  217.5   1st Qu.:24   1st Qu.:1.0000  
#>  N.blank  :  0   Median :  588.0   Median :24   Median :1.0000  
#>  Min.nchar:  2   Mean   : 1733.6   Mean   :24   Mean   :0.9291  
#>  Max.nchar:  2   3rd Qu.: 1339.5   3rd Qu.:24   3rd Qu.:1.0000  
#>                  Max.   :25371.0   Max.   :24   Max.   :1.0000
summary(prop.week.non.trap)
#>       nafodiv         fishery        gear.name           fa           year     
#>  Length   :309   Length   :309   Length   :309   Length   :309   Min.   :2015  
#>  N.unique :  2   N.unique :  2   N.unique :  2   N.unique : 17   1st Qu.:2015  
#>  N.blank  :  0   N.blank  :  0   N.blank  :  0   N.blank  :  0   Median :2015  
#>  Min.nchar:  2   Min.nchar: 15   Min.nchar:  7   Min.nchar: 10   Mean   :2015  
#>  Max.nchar:  2   Max.nchar: 16   Max.nchar:  8   Max.nchar: 11   3rd Qu.:2016  
#>                                                                  Max.   :2016  
#>          sw        total.gear       soak.time     prop.week.fished
#>  Length   :309   Min.   :  2.00   Min.   : 1.00   Min.   :0.1429  
#>  N.unique : 31   1st Qu.: 12.00   1st Qu.: 8.00   1st Qu.:0.1429  
#>  N.blank  :  0   Median : 21.00   Median :12.07   Median :0.2857  
#>  Min.nchar:  2   Mean   : 48.79   Mean   :14.22   Mean   :0.4039  
#>  Max.nchar:  2   3rd Qu.: 45.00   3rd Qu.:23.60   3rd Qu.:0.5714  
#>                  Max.   :630.00   Max.   :24.00   Max.   :1.0000
```

## Specify the number of vertical lines

In these examples, the number of vertical lines per unit of gear is one
for the Atlantic halibut longline and snow crab trap fisheries, and two
for the winter flounder gillnet fishery.  

``` r

dat<-dplyr::bind_rows(prop.week.trap,prop.week.non.trap)
dat$num.lines<-1
dat[which(dat$fishery=='Winter flounder'),'num.lines']<-2
```

## Calculate CEU

After running the code, remember to save the file for the fisheriescape
calculations.

``` r

df.ceu<-dat

names(df.ceu)
#>  [1] "nafodiv"          "fishery"          "gear.name"        "fa"              
#>  [5] "year"             "sw"               "total.gear"       "soak.time"       
#>  [9] "prop.week.fished" "num.lines"

df.ceu$ceu<-df.ceu$total.gear * df.ceu$num.lines * df.ceu$soak.time/24 *df.ceu$prop.week.fished

df.ceu<-as.data.frame(df.ceu)
head(df.ceu)
#>   nafodiv   fishery gear.name         fa year sw total.gear soak.time
#> 1      4S Snow crab      trap  GQ_CFA_12 2015 20        115        24
#> 2      4S Snow crab      trap GQ_CFA_12B 2015 14        346        24
#> 3      4S Snow crab      trap GQ_CFA_12B 2015 15        511        24
#> 4      4S Snow crab      trap GQ_CFA_12B 2015 16        771        24
#> 5      4S Snow crab      trap GQ_CFA_12B 2015 17        765        24
#> 6      4S Snow crab      trap GQ_CFA_12B 2015 18        642        24
#>   prop.week.fished num.lines       ceu
#> 1        0.1428571         1  16.42857
#> 2        0.4285714         1 148.28571
#> 3        1.0000000         1 511.00000
#> 4        1.0000000         1 771.00000
#> 5        1.0000000         1 765.00000
#> 6        1.0000000         1 642.00000
```
