install.packages("partykit")
install.packages("Rling")
install.packages("Hmisc")

library(Rling) 
library(partykit)
library(Hmisc)

do=read.table(file.choose(), header = T, sep = "\t", stringsAsFactors=T)

do.ctree <- ctree(CONSTRUCTION ~ POSITION + DIALECT + VERSE + PERIOD + INF_ORIGIN, data = do)

plot(do.ctree)

table(predict(do.ctree), do$CONSTRUCTION)

do.rf <- cforest(CONSTRUCTION ~ POSITION + DIALECT + VERSE + PERIOD + INF_ORIGIN, data = do, perturb=list(replace=TRUE), ntree = 2000, mtry = 2)

do.varimp <- varimp(do.rf, conditional = TRUE)

round(do.varimp, 3)

dotchart(sort(do.varimp), main = "Conditional importance of variables")

(table(predict(do.rf), do$CONSTRUCTION))

prob.crf.do <- unlist(predict(do.rf, type="prob", OOB=TRUE))[c(FALSE, TRUE)]

somers2(prob.crf.do, as.numeric(do$CONSTRUCTION) - 1)

pred.crf.train <- predict(do.rf, OOB = FALSE)
table(pred.crf.train, do$CONSTRUCTION)
