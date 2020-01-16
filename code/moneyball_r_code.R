#read in Moneyball csv file
library(readr)
MoneyBall <- read_csv("~/Northwestern University/Winter 2018 Quarter/R Studio CSV Files/MoneyBall.csv")
View(MoneyBall)
mb <- MoneyBall

#import libraries
library(tidyverse)
library(reshape2)
library(ggridges)
library(ggplot2)
library(randomForest)
library(moments)

#find null values
nulls <- data.frame(col=as.character(colnames(mb)),pct_null=colSums(is.na(mb))*100/(colSums(is.na(mb))+colSums(!is.na(mb))))%>%
  + filter(col != 'INDEX')
nulls

#remove values that are more than 15% null
remove_data_1 <- function(df){
  + # Remove the values that are more than 15% NULL
    + -TEAM_BATTING_HBP -TEAM_BASERUN_CS}#%>%
  "# Remove the values that are more than 15% NULL
-TEAM_BATTING_HBP,"
remove_data_1 <- function(df){
  + return(df%>%dplyr::select(# Remove the values that are more than 15% NULL
    + -TEAM_BATTING_HBP, -TEAM_BASERUN_CS)#%>%
    + # Replace the remaining numeric nulls with the median value of the column
      + mutate_if(is.numeric, na.roughfix)
      + )
  + }
mb <- remove_data_1(mb)
mb

str(mb)

summary(mb)

cleaned_cols <- list()
 for(c in colnames(mb[c(-1,-2)])){
  + column <- mb%>%select_(col = c)
  + iqr <- quantile(column$col, na.rm = T)[4] - quantile(column$col, na.rm = T)[2]
  + low <- quantile(column$col, na.rm = T)[2] - iqr
  + high <- quantile(column$col, na.rm = T)[4] + iqr
  + vals <- c()
  + for(i in seq(1:nrow(mb))){
    + ifelse(between(column$col[i], low - (1.5*iqr), high + (1.5*iqr)),
             + vals[i] <- column$col[i],
             + ifelse(is.na(column$col[i]), vals[i] <- NA, vals[i] <- NA))
    + }
  + ifelse(length(vals) == nrow(mb),
           + cleaned_cols[[c]] <- vals,
           + cleaned_cols[[c]] <- c(vals,NA))
  + }

mb <- bind_cols(INDEX = mb$INDEX, TARGET_WINS = mb$TARGET_WINS, cleaned_cols)

library(mice)

mb <- complete(temp_mb, 1)%>%
  + bind_cols(mb%>%dplyr::select(TARGET_WINS, INDEX))

summary(mb)

#correlations
cors <- data.frame(cors = cor(mb, method = 'pearson')['TARGET_WINS',]^2, vars = rownames(cor(mb)))
cors

#backward
mbbackward <- lm(TARGET_WINS ~ . -INDEX, data = mb)
mbbackward

library(MASS)

stepAIC(mbbackward, direction = "backward")

#forward
mbforward <- mbbackward
stepAIC(mbforward, direction = "forward")

mbforwardr <- stepAIC(mbforward, direction = "forward")
summary(mbforwardr)

#parsimony
summary(mbparsimony)

#creat per game statistics
bhpg <- MoneyBall$TEAM_BATTING_H/162
b2bpg <- MoneyBall$TEAM_BATTING_2B/162
b3bpg <- MoneyBall$TEAM_BATTING_3B/162
bhpg <- MoneyBall$TEAM_BATTING_H/162
b2bpg <- MoneyBall$TEAM_BATTING_2B/162
b3bpg <- MoneyBall$TEAM_BATTING_3B/162
bhrpg <- MoneyBall$TEAM_BATTING_HR/162
bbbpg <- MoneyBall$TEAM_BATTING_BB/162
bhbppg <- MoneyBall$TEAM_BATTING_HBP/162
bsopg <- MoneyBall$TEAM_BATTING_SO/162
brsbpg <- MoneyBall$TEAM_BASERUN_SB/162
brcspg <- MoneyBall$TEAM_BASERUN_CS/162
fepg <- MoneyBall$TEAM_FIELDING_E/162
fdppg <- MoneyBall$TEAM_FIELDING_DP/162
pbbpg <- MoneyBall$TEAM_PITCHING_BB/162
phpg <- MoneyBall$TEAM_PITCHING_H/162
phrpg <- MoneyBall$TEAM_PITCHING_HR/162
psopg <- MoneyBall$TEAM_PITCHING_SO/162
ipg <- MoneyBall$INDEX
twpg <- MoneyBall$TARGET_WINS

#create per game statistics data frame
mbpg <- data.frame(ipg, twpg, bhpg, b2bpg, b3bpg, bhrpg, bbbpg, bhbppg, bsopg, brsbpg, brcspg, fepg, fdppg, pbbpg, phpg, phrpg, psopg)
str(mbpg)

mbpg$m.bhpg <- ifelse(mbpg$bhpg > 9.6, "1", "0")
mbpg$m.b2bpg <- ifelse(mbpg$b2bpg > 2.2, "1", "0")
mbpg$m.b3bpg <- ifelse(mbpg$b3bpg > 0.3, "1", "0")
mbpg$m.bhrpg <- ifelse(mbpg$bhrpg > 1.5, "1", "0")
mbpg$m.bbbpg <- ifelse(mbpg$bbbpg > 4.1, "1", "0")
mbpg$m.bhbppg <- ifelse(mbpg$bhbppg > 0.6, "1", "0")
mbpg$m.bsopg <- ifelse(mbpg$bsopg < 6, "1", "0")
mbpg$m.brsbpg <- ifelse(mbpg$brsbpg > 0.9, "1", "0")
mbpg$m.brcspg <- ifelse(mbpg$brcspg > 0.3, "1", "0")
mbpg$m.fepg <- ifelse(mbpg$fepg > 0.8, "1", "0")
mbpg$m.fdppg <- ifelse(mbpg$fdppg > 1.1, "1", "0")
mbpg$m.pbbpg <- ifelse(mbpg$pbbpg > 4, "1", "0")
mbpg$m.phpg <- ifelse(mbpg$phpg > 10.1, "1", "0")
mbpg$m.phrpg <- ifelse(mbpg$phrpg > 1.6, "1", "0")
mbpg$m.psopg <- ifelse(mbpg$psopg > 10.2, "1", "0")

#contextual
mbcontextual1 <- lm(mbpg$twpg ~ (mbpg$m.bhpg == 1) + (mbpg$m.b2bpg == 1) + (mbpg$m.b3bpg == 1) + (mbpg$m.bhrpg == 1) + (mbpg$m.bbbpg == 1) + (mbpg$m.bhbppg == 1) + (mbpg$m.bsopg == 1) + (mbpg$m.brsbpg == 1) + (mbpg$m.brcspg == 1) + (mbpg$m.fepg == 1) + (mbpg$m.fdppg == 1) + (mbpg$m.pbbpg == 1) + (mbpg$m.phpg == 1) + (mbpg$m.phrpg == 1) + (mbpg$m.psopg == 1))
mbcontextual1
summary(mbcontextual1)

#contextual 2
mbcontextual2 <- c(mean(mbpg$m.bhpg == 0),mean(mbpg$m.b2bpg == 0),mean(mbpg$m.b3bpg == 0),mean(mbpg$m.bhrpg == 0),mean(mbpg$m.bbbpg == 0),mean(mbpg$m.bhbppg == 0),mean(mbpg$m.bsopg == 0),mean(mbpg$m.brsbpg == 0),mean(mbpg$m.brcspg == 0),mean(mbpg$m.fepg == 0),mean(mbpg$m.fdppg == 0),mean(mbpg$m.pbbpg == 0),mean(mbpg$m.phpg == 0),mean(mbpg$m.phrpg == 0),mean(mbpg$m.psopg == 0))
mbcontextual2

mean(MoneyBall$TARGET_WINS)
min(MoneyBall$TARGET_WINS)
max(MoneyBall$TARGET_WINS)

#export data
library(readr)
MoneyBall_EXPORT <- read_csv("~/Northwestern University/Winter 2018 Quarter/R Studio CSV Files/MoneyBall_EXPORT.csv")
View(MoneyBall_EXPORT)

Y_mean <- 80.79086
y_random <- runif(4144, 0, 146)
attach(MoneyBall_EXPORT)

mbe <- MoneyBall_EXPORT
library(moments)

nulls <- data.frame(col=as.character(colnames(mbe)),pct_null=colSums(is.na(mbe))*100/(colSums(is.na(mbe))+colSums(!is.na(mbe))))%>%
  + 
  + filter(col != 'INDEX')
nulls

nulls2 <- data.frame(col=as.character(colnames(mbe)),pct_null=colSums(is.na(mbe))*100/(colSums(is.na(mbe))+colSums(!is.na(mbe))))%>%
  + filter(col != 'INDEX')
nulls2

remove_data_1 <- function(df){
  + # Remove the values that are more than 15% NULL
    + }

remove_data_2 <- function(df){
  + return(df%>%dplyr::select(# Remove the values that are more than 15% NULL
    + -TEAM_BATTING_HBP, -TEAM_BASERUN_CS)#%>%
    + # Replace the remaining numeric nulls with the median value of the column
      + #mutate_if(is.numeric, na.roughfix)
      + )
  + }

mbe <- remove_data_2(mbe)
summary(mbe)

library(readr)
MoneyBall_EXPORT2 <- read_csv("~/Northwestern University/Winter 2018 Quarter/R Studio CSV Files/MoneyBall_EXPORT2.csv")
View(MoneyBall_EXPORT2)
mbe <- MoneyBall_EXPORT2

Y_BASE <- 35.6048 + 0.027*mbe$TEAM_BATTING_H + 0.0095*mbe$TEAM_BATTING_2B + 0.144*mbe$TEAM_BATTING_3B + 0.0446*mbe$TEAM_BATTING_HR + 0.0301*mbe$TEAM_BATTING_BB - 0.0065*mbe$TEAM_BATTING_SO + 0.0714*mbe$TEAM_BASERUN_SB + 0.0004*mbe$TEAM_PITCHING_H + 0.0391*mbe$TEAM_PITCHING_H - 0.008*mbe$TEAM_PITCHING_BB - 0.0049*mbe$TEAM_PITCHING_SO - 0.0577*mbe$TEAM_FIELDING_E - 0.0932*mbe$TEAM_FIELDING_DP

Y_PARSIMONY <- 3.568 + 0.0299*mbe$TEAM_BATTING_H + 0.0174*mbe$TEAM_BATTING_2B + 0.1065*mbe$TEAM_BATTING_3B + 0.0698*mbe$TEAM_BATTING_HR + 0.0231*mbe$TEAM_BATTING_BB + 0.0381*mbe$TEAM_BASERUN_SB

mean(abs(MoneyBall_ACTUAL$Y_Actual-Y_BASE))
mean(abs(MoneyBall_ACTUAL$Y_Actual-Y_PARSIMONY))

Y_CONTEXTUAL <- 79.630 + 8.868*(mbe$TEAM_BATTING_H/162) - 1.091*(mbe$TEAM_BATTING_2B/162) - 1.132*(mbe$TEAM_BATTING_3B/162) - 9.773*(mbe$TEAM_BATTING_HR/162) + 5.830*(mbe$TEAM_BATTING_BB/162) + 3.263*(mbe$TEAM_BATTING_SO/162) + 10.491*(mbe$TEAM_BASERUN_SB/162) - 3.563*(mbe$TEAM_BASERUN_CS/162) - 2.960*(mbe$TEAM_PITCHING_H/162) + 9.142*(mbe$TEAM_PITCHING_HR/162) + 9.067*(mbe$TEAM_PITCHING_BB/162) - 10.643*(mbe$TEAM_FIELDING_E/162) - 4.925*(mbe$TEAM_FIELDING_DP/162)

Y_BASE <- 35.6048 + 0.027*mbe$TEAM_BATTING_H + 0.0095*mbe$TEAM_BATTING_2B + 0.144*mbe$TEAM_BATTING_3B + 0.0446*mbe$TEAM_BATTING_HR + 0.0301*mbe$TEAM_BATTING_BB - 0.0065*mbe$TEAM_BATTING_SO + 0.0714*mbe$TEAM_BASERUN_SB + 0.0004*mbe$TEAM_PITCHING_H + 0.0391*mbe$TEAM_PITCHING_HR - 0.008*mbe$TEAM_PITCHING_BB - 0.0049*mbe$TEAM_PITCHING_SO - 0.0577*mbe$TEAM_FIELDING_E - 0.0932*mbe$TEAM_FIELDING_DP

mean(abs(MoneyBall_ACTUAL$Y_Actual-Y_BASE))
mean(abs(MoneyBall_ACTUAL$Y_Actual-Y_CONTEXTUAL))

TEAM_BATTING_H_PG <- (mb$TEAM_BATTING_H/162)
TEAM_BATTING_2B_PG <- (mb$TEAM_BATTING_2B/162)
TEAM_BATTING_3B_PG <- (mb$TEAM_BATTING_3B/162)
TEAM_BATTING_HR_PG <- (mb$TEAM_BATTING_HR/162)
TEAM_BATTING_BB_PG <- (mb$TEAM_BATTING_BB/162)
TEAM_BASERUN_SB_PG <- (mb$TEAM_BASERUN_SB/162)

youareuptobat <- lm(mb$TARGET_WINS ~ TEAM_BATTING_H_PG + TEAM_BATTING_2B_PG + TEAM_BATTING_3B_PG + TEAM_BATTING_HR_PG + TEAM_BATTING_BB_PG + TEAM_BASERUN_SB_PG)
youareuptobat
summary(youareuptobat)

mean(youareuptobat$fitted.values)
y <- 3.568 + 4.8461*(TEAM_BATTING_H_PG) + 2.8145*(TEAM_BATTING_2B_PG) + 17.2510*(TEAM_BATTING_3B_PG) + 11.3073*(TEAM_BATTING_HR_PG) + 3.7492*(TEAM_BATTING_BB_PG) + 6.1710*(TEAM_BASERUN_SB_PG)
