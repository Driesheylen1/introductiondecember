library(data.table)
library(ggplot2)
library(GGally)
library(ggrepel) 
library(randomcoloR)
library("readxl")
library("reshape2")
library(dplyr)
install.packages("writexl")
library(writexl)



iam = randomizing2
#not needed this
#iam = melt(iam, id.vars = c("ID", "Batch", "Sex", "Age", "Diagnosis", "In_Hospital_Mortality",
                            "ICU_admission", "severity_groups", "Diag_cat_number", "CCI", "Gram_stain", "SOFA_score", "WBC", "CRP",
          # variable.name = "assay", value.name = "npx")


tt = table(iam$severity_groups)



sepsis_unfavourable <- subset(iam, severity_groups %in% names(tt[tt = 1])) #to get only unfavourable outcome
sepsis_favourable <- subset(iam, severity_groups %in% names(tt[tt = 2])) #to get only favourable outcome
#t.test(sepsis_favourable$`CSF-1`, sepsis_unfavourable$IL8, var.equal = FALSE)
       
#cleaning data       
sepsis_unfavourable <- sepsis_unfavourable[ -c(1:14) ]
sepsis_favourable <- sepsis_favourable[ -c(1:14) ]

protein_expression <- as.data.frame(mapply(t.test, sepsis_favourable, sepsis_unfavourable))
protein_expression = unlist(protein_expression)
protein_expression = as.data.frame(protein_expression)
rownames = rownames(protein_expression)
protein_expression = cbind(rownames, protein_expression)
write_xlsx(x = protein_expression, "C:/Users/HEYLEND/Desktop/introductiondecember/differential_expressionrandom_severity_size2.xlsx",
            use_zip64 = T)
