# M-SCore model
library(dplyr)
library(tidyr)
library(lubridate)
library(fpp3)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(plotly)
library(gplots)
library(pROC)
library(tidybayes)
library(brms)
library(furrr)

#Data
load("../Data/mdsMScore.Rdata")
(varsMscore <- c("DSRI","GMI","AQI","SGI","DEPI","SGAI","LVGI","TATA"))

# Table
library(tableone)
tbl2  <- print(
  CreateTableOne(vars = c("MScore", varsMscore), strata = "Fraud"
                 , data = mds, addOverall = T)
  , showAllLevels = T, test = T
  , quote = FALSE
)
tbl2
save(tbl2, file = "../Data/Table2.Rdata")

# Models
## Naive
mm01NaiveTrain <- sdsTrain %>%
  roc(Fraud, Manipulator)
mm01NaiveTrain
mm01NaiveTest <- sdsTest %>%
  roc(Fraud, Manipulator)
mm01NaiveTest
# Call:
#   roc.data.frame(data = ., response = Fraud, predictor = Manipulator)
# 
# Data: Manipulator in 1229 controls (Fraud 0) < 54 cases (Fraud 1).
# Area under the curve: 0.5038

# Add MScore
summary(mm01b <- brm(Fraud ~ Date + MScore  + (1|Industry), data = sdsTrain
                   , family = bernoulli(link = "logit")
                   , chains = 2, cores = 2
                   , backend = "cmdstanr"
                   , threads = threading(2))
)
auc(roc(sdsTest$Fraud, predict(mm01b, newdata = sdsTest, type = "response")[, 'Estimate']))

ff <- as.formula(paste("Fraud ~ Date +"
                       , paste(varsMscore, collapse = "+")
                       , "+ (1|Industry)")
                 )
summary(mm01c <- brm(ff, data = sdsTrain
                    , family = bernoulli(link = "logit")
                    , chains = 2, cores = 2
                    , backend = "cmdstanr"
                    , threads = threading(2))
)
auc(roc(sdsTest$Fraud, predict(mm01c, newdata = sdsTest, type = "response")[, 'Estimate']))

save(m01NaiveTrain, m01NaiveTest, mm01b, mm01c, file = "../Models/m01BaselineMScore.Rdata")

# # Auto-correlation
# tds <- mds[!is.na(mds$Date), ] %>% 
#   as_tsibble(key = c(Ticker, MScoreVariable), index = Date)
# gg1 <- tds %>% filter(MScoreVariable == "MScore") %>%
#   ggplot(aes(y = MScoreValues, x = Date, group = Ticker, colour = factor(Fraud))) +
#   geom_line() +
#   geom_point() +
#   guides(colour = FALSE)
# ggplotly(gg1)
# 
# # Auto-Correlation
# tds %>% filter(MScoreVariable == "MScore") %>%
#   fill_gaps() %>%
#   ACF(MScoreValues) %>%
#   autoplot() +
#   facet_wrap(~Ticker, scales = "free")
# tds %>% filter(MScoreVariable == "MScore") %>%
#   PACF(MScoreValues) %>%
#   autoplot() +
#   facet_wrap(~Ticker, scales = "free")