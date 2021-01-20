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

iam = Sepsis_data_supervised_influenza_normalized_outliers_out

#not needed this
#iam = melt(iam, id.vars = c("ID", "Batch", "Sex", "Age", "Diagnosis", "In_Hospital_Mortality",
                            "ICU_admission", "severity_groups", "Diag_cat_number", "CCI", "Gram_stain", "SOFA_score", "WBC", "CRP",
          # variable.name = "assay", value.name = "npx")


tt = table(iam$Diagnosis)



sepsis_influenza <- subset(iam, Diagnosis %in% names(tt[tt = 2])) #to get only unfavourable outcome, Numbers refer to table row 
sepsis_non_influenza <- subset(iam, Diagnosis %in% names(tt[tt = 1])) #to get only favourable outcome
#t.test(sepsis_favourable$`CSF-1`, sepsis_unfavourable$IL8, var.equal = FALSE)
       
#cleaning data     p  
sepsis_influenza <- sepsis_influenza[ -c(1:2) ]
sepsis_non_influenza <- sepsis_non_influenza[ -c(1:2) ]

protein_expression <- as.data.frame(mapply(t.test, sepsis_influenza, sepsis_non_influenza)) #mapply crucial for taking all variants
protein_expression = unlist(protein_expression)
protein_expression = as.data.frame(protein_expression)
rownames = rownames(protein_expression)
protein_expression = cbind(rownames, protein_expression)
write_xlsx(x = protein_expression, "C:/Users/HEYLEND/Desktop/introductiondecember/differential_expressioninfluenza_supervised.xlsx",
            use_zip64 = T)
