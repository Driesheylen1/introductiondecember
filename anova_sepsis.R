x = unique(Sepsis_anov_grouping$Anova_groups)
iam = Sepsis_anov_grouping
View(x)

iam = melt(iam, id.vars = c("ID", "Anova_groups"),
          variable.name = "assay", value.name = "npx")

models = sapply(x, function(my) {
  lm(npx ~ assay, data = iam, Anova_groups==my)
}, simplify = FALSE)

ANOVA.tables <- sapply(models, anova, simplify=FALSE)

summary(ANOVA.tables)
summary(models$group2) #I think I should switch assays and groups
