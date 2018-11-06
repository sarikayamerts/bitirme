wide_first$winner_category <- sapply(as.character(wide_first$winner), switch, "odd1" = 1, "oddX" = 2, "odd2" = 3, USE.NAMES = F)
wide_first$winner = NULL
smp_size <- floor(0.70 * nrow(wide_first))
set.seed(123)
train_ind <- sample(seq_len(nrow(wide_first)), size = smp_size)
train <- wide_first[train_ind, ]
test <- wide_first[-train_ind, ]
glmnet(train[,-83], as.numeric(unlist(train[,83])))

cvFit <- cv.glmnet(x = as.matrix(train[,-83]), y = as.matrix(train[,83]), family = "multinomial", type.measure = "class" )

