workout01-Daniel-delCarpio
================
Daniel del Carpio
March 12, 2019

``` r
shots_data <- read.csv("C:/Users/danie/OneDrive/Documents/GitHub/stat133workout01/workout01/data/shots-data.csv", stringsAsFactors = FALSE)

curry_total2 <- nrow(shots_data[shots_data$name == 'Stephen Curry' & shots_data$shot_type == '2PT Field Goal',])
curry_made2 <- nrow(shots_data[shots_data$name == 'Stephen Curry' & shots_data$shot_type == '2PT Field Goal' & shots_data$shot_made_flag == 'shot_yes',])

klay_total2 <- nrow(shots_data[shots_data$name == 'Klay Thompson' & shots_data$shot_type == '2PT Field Goal',])
klay_made2 <- nrow(shots_data[shots_data$name == 'Klay Thompson' & shots_data$shot_type == '2PT Field Goal' & shots_data$shot_made_flag == 'shot_yes',])

kd_total2 <- nrow(shots_data[shots_data$name == 'Kevin Durant' & shots_data$shot_type == '2PT Field Goal',])
kd_made2 <- nrow(shots_data[shots_data$name == 'Kevin Durant' & shots_data$shot_type == '2PT Field Goal' & shots_data$shot_made_flag == 'shot_yes',])

draymond_total2 <- nrow(shots_data[shots_data$name == 'Draymond Green' & shots_data$shot_type == '2PT Field Goal',])
draymond_made2 <- nrow(shots_data[shots_data$name == 'Draymond Green' & shots_data$shot_type == '2PT Field Goal' & shots_data$shot_made_flag == 'shot_yes',])

iggy_total2 <- nrow(shots_data[shots_data$name == 'Stephen Curry' & shots_data$shot_type == '2PT Field Goal',])
iggy_made2 <- nrow(shots_data[shots_data$name == 'Andre Iguodala' & shots_data$shot_type == '2PT Field Goal' & shots_data$shot_made_flag == 'shot_yes',])

table1 <- matrix(c("Stephen Curry",curry_total2,curry_made2,(curry_made2/curry_total2),"Klay Thompson",klay_total2,klay_made2,(klay_made2/klay_total2),"Kevin Durant", kd_total2, kd_made2,(kd_made2/kd_total2),"Draymond Green", draymond_total2, draymond_made2,(draymond_made2/draymond_total2),"Andre Iguodala",iggy_total2,iggy_made2,(iggy_made2/iggy_total2)),ncol=4,byrow=TRUE)

colnames(table1) <- c("name","total","made","perc_made")

table1 <- as.table(table1)
table1 
```

    ##   name           total made perc_made        
    ## A Stephen Curry  563   304  0.539964476021314
    ## B Klay Thompson  640   329  0.5140625        
    ## C Kevin Durant   643   390  0.606531881804043
    ## D Draymond Green 346   171  0.494219653179191
    ## E Andre Iguodala 563   134  0.238010657193606

``` r
curry_total3 <- nrow(shots_data[shots_data$name == 'Stephen Curry' & shots_data$shot_type == '3PT Field Goal',])
curry_made3 <- nrow(shots_data[shots_data$name == 'Stephen Curry' & shots_data$shot_type == '3PT Field Goal' & shots_data$shot_made_flag == 'shot_yes',])

klay_total3 <- nrow(shots_data[shots_data$name == 'Klay Thompson' & shots_data$shot_type == '3PT Field Goal',])
klay_made3 <- nrow(shots_data[shots_data$name == 'Klay Thompson' & shots_data$shot_type == '3PT Field Goal' & shots_data$shot_made_flag == 'shot_yes',])

kd_total3 <- nrow(shots_data[shots_data$name == 'Kevin Durant' & shots_data$shot_type == '3PT Field Goal',])
kd_made3 <- nrow(shots_data[shots_data$name == 'Kevin Durant' & shots_data$shot_type == '3PT Field Goal' & shots_data$shot_made_flag == 'shot_yes',])

draymond_total3 <- nrow(shots_data[shots_data$name == 'Draymond Green' & shots_data$shot_type == '3PT Field Goal',])
draymond_made3 <- nrow(shots_data[shots_data$name == 'Draymond Green' & shots_data$shot_type == '3PT Field Goal' & shots_data$shot_made_flag == 'shot_yes',])

iggy_total3 <- nrow(shots_data[shots_data$name == 'Stephen Curry' & shots_data$shot_type == '3PT Field Goal',])
iggy_made3 <- nrow(shots_data[shots_data$name == 'Andre Iguodala' & shots_data$shot_type == '3PT Field Goal' & shots_data$shot_made_flag == 'shot_yes',])

table2 <- matrix(c("Stephen Curry",curry_total3,curry_made3,(curry_made3/curry_total3),"Klay Thompson",klay_total3,klay_made3,(klay_made3/klay_total3),"Kevin Durant", kd_total3, kd_made3,(kd_made3/kd_total3),"Draymond Green", draymond_total3, draymond_made3,(draymond_made3/draymond_total3),"Andre Iguodala",iggy_total3,iggy_made3,(iggy_made3/iggy_total3)),ncol=4,byrow=TRUE)

colnames(table2) <- c("name","total","made","perc_made")

table2 <- as.table(table2)
table2 
```

    ##   name           total made perc_made         
    ## A Stephen Curry  687   280  0.407569141193595 
    ## B Klay Thompson  580   246  0.424137931034483 
    ## C Kevin Durant   272   105  0.386029411764706 
    ## D Draymond Green 232   74   0.318965517241379 
    ## E Andre Iguodala 687   58   0.0844250363901019

``` r
table3 <- matrix(c("Stephen Curry",curry_total2 + curry_total3,curry_made3 +curry_made2,(curry_made3 +curry_made2)/(curry_total2 + curry_total3),"Klay Thompson",klay_total3+klay_total2,klay_made3+klay_made2,(klay_made3+klay_made2)/(klay_total2+klay_total3),"Kevin Durant", kd_total3+kd_total2, kd_made2+kd_made3,(kd_made2+kd_made3)/(kd_total2 +kd_total3),"Draymond Green", draymond_total3+draymond_total2, draymond_made2 + draymond_made3,(draymond_made2+draymond_made3)/(draymond_total2+draymond_total3),"Andre Iguodala",iggy_total2+iggy_total3,iggy_made3+iggy_made2,(iggy_made2+iggy_made3)/(iggy_total2 +iggy_total3)),ncol=4,byrow=TRUE)

colnames(table3) <- c("name","total","made","perc_made")

table3 <- as.table(table3)
table3 
```

    ##   name           total made perc_made        
    ## A Stephen Curry  1250  584  0.4672           
    ## B Klay Thompson  1220  575  0.471311475409836
    ## C Kevin Durant   915   495  0.540983606557377
    ## D Draymond Green 578   245  0.423875432525952
    ## E Andre Iguodala 1250  192  0.1536
