library(tidybayes)
library(caret)
library(brms)
library(furrr)
library(pROC)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(cowplot)
library(tableone)

# Data
load("../Data/mdsFirstDiff.Rdata")
head(sdsTrain)
# Models
## Default prior
summary(mb01 <- brm(Fraud ~ Date + (1|Industry), data = sdsTrain
                     , family = bernoulli(link = "logit")
                     , chains = 2, cores = 2
                     , backend = "cmdstanr"
                     , threads = threading(2)
                     , control = list(adapt_delta = 0.99))
)
auc(roc(sdsTest$Fraud, predict(mb01, newdata = sdsTest, type = "response")[,1]))

## Normal prior
get_prior(Fraud ~ Date + (1|Industry), data = sdsTrain)
bprior <- c(prior(normal(0,10), class = "Intercept")
            , prior(normal(1,2), class = "b", coef = "Date")
            , prior(cauchy(0, 1), class = sd))
summary(mb01a <- brm(Fraud ~ Date + (1|Industry), data = sdsTrain
                   , family = bernoulli(link = "logit")
                   , chains = 2, cores = 2
                   , prior = bprior
                   , backend = "cmdstanr"
                   , threads = threading(2)
                   , control = list(adapt_delta = 0.99))
)
auc(roc(sdsTest$Fraud, predict(mb01a, newdata = sdsTest, type = "response")[,1]))

# RIRS
summary(mb01b <- brm(Fraud ~ (1 + Date|Industry), data = sdsTrain
                   , family = bernoulli(link = "logit")
                   , chains = 2, cores = 2
                   , backend = "cmdstanr"
                   , threads = threading(2)
                   , control = list(adapt_delta = 0.99))
)
auc(roc(sdsTest$Fraud, predict(mb01b, newdata = sdsTest, type = "response")[,1]))

# Fixed date with random industry and normal prior is best: refit with more iterations
mb01 <- update(mb01a
               , chains = 7, cores = 7, iter = 4000, warmup = 1000
               , backend = "cmdstanr"
               , threads = threading(7)
               , control = list(adapt_delta = 0.99))
mb01
auc(roc(sdsTest$Fraud, predict(mb01, newdata = sdsTest, type = "response")[,1]))

# Full model intercepts
varsRHS <- colnames(sdsTrain)[!colnames(sdsTrain)%in%c("Industry", "Ticker","Fraud", "Date")]
varsRHS
ff <- as.formula(paste("Fraud ~ Date +"
                       , paste(varsRHS, collapse = "+")
                       , "+(1|Industry)"))
ff
## Big model
# summary(mb02 <- brm(ff, data = sdsTrain
#                    , family = bernoulli(link = "logit")
#                    , prior = bprior
#                    , chains = 7, cores = 7, iter = 4000, warmup = 1000
#                    , backend = "cmdstanr"
#                    , threads = threading(7)
#                    , control = list(adapt_delta = 0.99))
# )
# loo_model_weights(mb01a, mb02)
# auc(roc(sdsTest$Fraud, predict(mb02, newdata = sdsTest, type = "response")[,1]))

# step forward
aucAll <- ff <- pHat <- NULL
for(rhsVar in varsRHS){
  ff <- as.formula(paste("Fraud ~ Date +", rhsVar, "+ (1|Industry)"))
  ff
  foo <- brm(ff, data = sdsTrain
             , family = bernoulli(link = "logit")
             , prior = bprior
             , chains = 2, cores = 2
             , backend = "cmdstanr"
             , threads = threading(2))
  print(auc(roc(sdsTest$Fraud, predict(foo, newdata = sdsTest, type = "response")[,1])))
  aucAll <- c(aucAll
              , auc(roc(sdsTest$Fraud, predict(foo, newdata = sdsTest, type = "response")[,1])))
}
aucAll
which.max(aucAll)
varsRHS[which.max(aucAll)]
aucAll[which.max(aucAll)]
varsRHSOthers <- varsRHS[!varsRHS == "CashFlowNetPurchasesofPPE"]
varsRHSOthers

aucBest <- aucAll[which.max(aucAll)]
aucAll <- c(aucBest)
varList <- c("CashFlowNetPurchasesofPPE")
ff <- aucNow <- NULL
for(rhsVar in varsRHSOthers){
  ff <- as.formula(paste("Fraud ~ Date + CashFlowNetPurchasesofPPE *", rhsVar, "+ (1|Industry)"))
  ff
  foo <- brm(ff, data = sdsTrain
             , family = bernoulli(link = "logit")
             , prior = bprior
             , chains = 2, cores = 2
             , backend = "cmdstanr"
             , threads = threading(2)
             , control = list(adapt_delta = 0.99))
  print( aucNow <- auc(roc(sdsTest$Fraud, predict(foo, newdata = sdsTest, type = "response")[,1])) )
  if(round((aucNow - aucBest)/aucBest,2) > 0.01){
    cat(paste("Variable added:", rhsVar), "\n")
    aucAll <- c(aucAll, aucNow)
    varList <- c(varList, rhsVar)
    aucBest <- aucNow
    cat(paste("New AUC:", aucBest), "\n")
  }
}
aucAll
varList

aucBest <- aucAll[which.max(aucAll)]
ff <- aucNow <- NULL
varsRHSOthers <- varsRHS[!varsRHS %in% varList]
for(rhsVar in varsRHSOthers){
  ff <- as.formula(paste("Fraud ~ Date + CashFlowNetPurchasesofPPE * BalanceSheetOtherEquity + ", rhsVar, "+ (1|Industry)"))
  ff
  foo <- brm(ff, data = sdsTrain
             , family = bernoulli(link = "logit")
             , prior = bprior
             , chains = 2, cores = 2
             , backend = "cmdstanr"
             , threads = threading(2)
             , control = list(adapt_delta = 0.99))
  print( aucNow <- auc(roc(sdsTest$Fraud, predict(foo, newdata = sdsTest, type = "response")[,1])) )
  if(round((aucNow - aucBest)/aucBest,2) > 0.01){
    cat(paste("Variable added:", rhsVar), "\n")
    aucAll <- c(aucAll, aucNow)
    varList <- c(varList, rhsVar)
    aucBest <- aucNow
    cat(paste("New AUC:", aucBest), "\n")
  }
}
aucAll
varList

aucBest <- aucAll[which.max(aucAll)]
ff <- aucNow <- NULL
varsRHSOthers <- varsRHS[!varsRHS %in% varList]
for(rhsVar in varsRHSOthers){
  ff <- as.formula(paste("Fraud ~ Date + CashFlowNetPurchasesofPPE * BalanceSheetOtherEquity * IncomeStatementBasicSharesOutstanding + ", rhsVar, "+ (1|Industry)"))
  ff
  foo <- brm(ff, data = sdsTrain
             , family = bernoulli(link = "logit")
             , prior = bprior
             , chains = 2, cores = 2
             , backend = "cmdstanr"
             , threads = threading(2)
             , control = list(adapt_delta = 0.99))
  print( aucNow <- auc(roc(sdsTest$Fraud, predict(foo, newdata = sdsTest, type = "response")[,1])) )
  if(round((aucNow - aucBest)/aucBest,2) > 0.01){
    cat(paste("Variable added:", rhsVar), "\n")
    aucAll <- c(aucAll, aucNow)
    varList <- c(varList, rhsVar)
    aucBest <- aucNow
    cat(paste("New AUC:", aucBest), "\n")
  }
}
aucAll
varList

# Prior from above
get_prior(Fraud ~ Date + CashFlowNetPurchasesofPPE + BalanceSheetOtherEquity + IncomeStatementBasicSharesOutstanding + (1|Industry), data = sdsTrain)
bprior <- c(prior(normal(0,10), class = "Intercept")
            , prior(normal(1,2), class = "b", coef = "Date")
            , prior(cauchy(0, 1), class = sd))
mbStep <-  brm(Fraud ~ Date + CashFlowNetPurchasesofPPE + BalanceSheetOtherEquity + IncomeStatementBasicSharesOutstanding + (1|Industry)
               , data = sdsTrain
               , family = bernoulli(link = "logit")
               , prior = bprior
               , chains = 7, cores = 7
               , backend = "cmdstanr"
               , threads = threading(7)
               , control = list(adapt_delta = 0.99))
mbStep
auc(roc(sdsTest$Fraud, predict(mbStep, newdata = sdsTest, type = "response")[,1]))

# Default piors
mbStep2 <-  brm(Fraud ~ Date + CashFlowNetPurchasesofPPE + BalanceSheetOtherEquity + IncomeStatementBasicSharesOutstanding + (1|Industry)
               , data = sdsTrain
               , family = bernoulli(link = "logit")
               , chains = 7, cores = 7
               , backend = "cmdstanr"
               , threads = threading(7)
               , control = list(adapt_delta = 0.99))
mbStep2
auc(roc(sdsTest$Fraud, predict(mbStep2, newdata = sdsTest, type = "response")[,1]))

# All normal priors
bprior <- c(prior(normal(0,10), class = "Intercept")
            , prior(normal(1,2), class = "b")
            , prior(cauchy(0, 1), class = sd))
mbStep3 <-  brm(Fraud ~ Date + CashFlowNetPurchasesofPPE + BalanceSheetOtherEquity + IncomeStatementBasicSharesOutstanding + (1|Industry)
               , data = sdsTrain
               , family = bernoulli(link = "logit")
               , prior = bprior
               , chains = 7, cores = 7
               , backend = "cmdstanr"
               , threads = threading(7)
               , control = list(adapt_delta = 0.99))
mbStep3
auc(roc(sdsTest$Fraud, predict(mbStep3, newdata = sdsTest, type = "response")[,1]))

# Interactions
mbStep4 <-  brm(Fraud ~ Date + CashFlowNetPurchasesofPPE * BalanceSheetOtherEquity * IncomeStatementBasicSharesOutstanding + (1|Industry)
               , data = sdsTrain
               , family = bernoulli(link = "logit")
               , prior = bprior
               , chains = 7, cores = 7
               , backend = "cmdstanr"
               , threads = threading(7)
               , control = list(adapt_delta = 0.99))
mbStep4
auc(roc(sdsTest$Fraud, predict(mbStep4, newdata = sdsTest, type = "response")[,1]))
conditional_effects(mbStep4)
plot(mbStep4, pars = "^b")

plot(mbStep)
pp_check(mbStep)
phat <- predict(mbStep, newdata = sdsTest, type = "response")[,1]
summary(phat)
quantile(phat, probs = seq(0.90, 0.99, 0.01), na.rm = T)
table(Fraud = sdsTest$Fraud, phat = phat > median(phat, na.rm = T))[2:1,2:1]
table(Fraud = sdsTest$Fraud, phat = phat > 0.035)[2:1,2:1]
table(Fraud = sdsTest$Fraud, phat = phat > 0.063)[2:1,2:1]


save(mb01, mb01b, mbStep4, file = "../Models/bayesStep.Rdata")

# Table
# Table
tbl3  <- print(
  CreateTableOne(vars = c("CashFlowNetPurchasesofPPE","BalanceSheetOtherEquity","IncomeStatementBasicSharesOutstanding")
                 , strata = "Fraud"
                 , data = mds, addOverall = T)
  , showAllLevels = T, test = T
  , quote = FALSE
)
tbl3
save(tbl3, file = "../Data/Table3.Rdata")
