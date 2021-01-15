library(reshape2)
path <- ("Y:\\Unit_HEALTH/RD_SPEC/Platform Systems Biology/3 PhD & PostDoc/PhD_Xomics/02_Data analyses/R codes")
setwd(paste0(path))
Sepsis_anov_grouping <- read.csv("Copy of Sepsis_anov_grouping.csv", header = T, sep = ";")
Y:\\Unit_HEALTH/RD_SPEC/Platform Systems Biology/3 PhD & PostDoc/PhD_Xomics/02_Data analyses/R codes/Rdump/
iam = melt(iam, id.vars = c("ID", "Anova_groups"),
          variable.name = "assay", value.name = "npx")
x = unique(iam$assay)
head(x)

models = sapply(x, function(my) {
  lm(npx ~ Anova_groups, data = iam, assay==my)
}, simplify = FALSE)

ANOVA.tables <- sapply(models, aov, simplify=FALSE)

#get anova table for assay 1
summary(ANOVA.tables[[1]])
#multiple comparison for assay 1
TukeyHSD(ANOVA.tables[[1]])
anova_outcome <- 0
print("This loop calculates the square of the first 92 elements of vector u1")

# Initialize `usq`

for(i in 1:92) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  anova_outcome[i] <- TukeyHSD(ANOVA.tables[[i]])
  print(anova_outcome[i])
}

print(i)

