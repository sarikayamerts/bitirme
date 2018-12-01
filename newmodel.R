wide_first$winner_category <- sapply(as.character(wide_first$winner), switch, "odd1" = 1, "oddX" = 2, "odd2" = 3, USE.NAMES = F)
wide_first$winner = NULL
smp_size <- floor(0.70 * nrow(wide_first))
set.seed(123)
train_ind <- sample(seq_len(nrow(wide_first)), size = smp_size)
train <- wide_first[train_ind, ]
test <- wide_first[-train_ind, ]

glmnet(train[,-17], as.numeric(unlist(train[,17])))
cvFit <- cv.glmnet(x = as.matrix(train[,-17]), y = as.matrix(train[,17]), family = "multinomial", type.measure = "class" )

library(caret)
fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  savePredictions = TRUE
)
wide_first
lreg<-train(winner_category~
              shin_prob.odd1.188BET+
              shin_prob.odd2.188BET+
              shin_prob.oddX.188BET+
              shin_prob.odd1.1xBet+
              shin_prob.odd2.1xBet+
              shin_prob.oddX.1xBet+
              shin_prob.odd1.ComeOn+
              shin_prob.odd2.ComeOn+
              shin_prob.oddX.ComeOn+
              shin_prob.odd1.888sport+
              shin_prob.odd2.888sport+
              shin_prob.oddX.888sport+
              shin_prob.odd1.Betfair+
              shin_prob.odd2.Betfair+
              shin_prob.oddX.Betfair,
            data=wide_first,
            method="glm",
            family=binomial(),
            trControl=fitControl)

train(x = train[,-17], 
      y = as.matrix(train[,17]),
      method = 'multinom')

