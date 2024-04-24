install.packages("irtrees")
library(irtrees)

mapping <- cbind(c(0, 1, 1), c(NA, 0, 1))
mapping

head(VerbAgg3[ , -(1:2)])

VerbAgg3T_wide <- WtoW_single.tree(VerbAgg3[ , -(1:2)], mapping)
head(VerbAgg3T_wide)


IRTree.mod2 = 'admission = 1-24
  affirmation = 25-48
  COV = admission*affirmation'

tree.mirt2 = mirt(data = VerbAgg3T_wide,
                  model = IRTree.mod2,
                  itemtype = 'Rasch')

coef(tree.mirt2, simplify = TRUE)

m2pl_map <- fscores(tree.mirt2, method = "MAP", full.scores = TRUE, full.scores.SE = TRUE)
head(m2pl_map)
