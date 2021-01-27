library(data.table)
library(ggplot2)
library(GGally)
library(ggrepel) 
library(randomcoloR)
library("readxl")
library("reshape2")
library("viridis")      

'cleaned data '
'#' #additional but not crucial for creating PCA plots only


iam = Sepsis_data_normalized_outliers_out #3 QC outliers removed, 12 controls removed as no metadata and one sample with no data (+ 8 bridgers)
iam = melt(iam, id.vars = c("ID", "Batch", "Sex", "Age", "Diagnosis", "In_Hospital_Mortality",
                            "ICU_admission", "Diag_cat_number", "CCI", "Gram_stain", "WBC", "CRP", "severity_groups", "SOFA_score"),
           variable.name = "assay", value.name = "npx")


iam = as.data.table(iam)
iam$npx = as.numeric(iam$npx)
#iam = na.omit(iam) #cucial for attaching batch info to data after pca (cfr my colmuns)
dd = dcast.data.table(iam, ID ~ assay, value.var = "npx", fun.aggregate = mean) # reshape the data, 8 overlaps become mean therfore 410 to 402

dim(dd) #informative ==> code to remove na's: dd <- na.omit(dd)

View(dd[,-1,]) #these are ID's don't need to be taken up in actual pca computation
#therefore remove them in next codeline
pca = prcomp(dd[,-1,], center = TRUE, scale. = TRUE, )  # excl first column, these are the names
summary(pca)
# pca6 = prcomp(dd6[,-1], center = TRUE, scale. = TRUE)  # excl first column, these are the names
# summary(pca6)

# dim(pca$x) # the coordinates of the samples in pca-space

pca_df = as.data.frame(pca$x) #creating dataframe with all principle components
dim(pca_df)
#pca_df = pca_df[-298,392] or is this line needed though?
pca_df$sample_name = as.character(dd[,1]$ID)


# attach person_id and collecting_date, matching with iam in stead of dd!!
m = match(pca_df$sample_name, iam$ID) #crucial!!!! matching sample ID's to metadata in iam 
#matching same id's over datasets that do not have equal numbe of samples
#m = match(FAPIC_Proteomics_ClinicalData_ID_cleaned$Sample_ID, pca_df$sample_name) #matching same id's over datasets that do not have equal numbe of samples
my_columns =  c("ID","Batch","Sex","Age","Diagnosis", "ICU_admission","In_Hospital_Mortality",
                  "CCI", "Gram_stain", "WBC", "CRP","severity_groups", "SOFA_score")
#my_columns2 =  c("Sample_ID", "Sex")

pca_df[,my_columns] = as.data.frame(iam[m, ..my_columns]) #completes the magic of matching correctly need to study code built up again
#pca_df[,my_columns2] = as.data.frame(Fap[m, ..my_columns2])

# # same for normalizing on 6 controls
# pca_df6 = as.data.frame(pca6$x)
# pca_df6$sample_name = as.character(dd6[,1]$sample_name)
# m6 = match(pca_df6$sample_name, d$sample_name)
# pca_df6[,my_cols] = as.data.frame(d[m6, ..my_cols])
# ggpairs(pca_df6[,1:n])

## Conclusion: normalizing on 6 controls is almost identical to normalizing on all 8,
## so, let's continue with normalization on all 8 controls, i.e. value_n in d

figure_adjustments = ggplot(pca_df, aes(x = PC1, y = PC4)) +
      geom_point(aes(colour = SOFA_score))
   
figure_adjustments + scale_color_manual(values = c("#F9F2F2", "#F2DBDB", "#FFB2B2", "#FFA0A0", "#EE1E1E", "#EE1E1E", "#EE1E1E", "#EE1E1E", "#EE1E1E", "#F20707", "#C30101"))
   
   scale_color_continuous(type = 'viridis')


   
   
   
      scale_color_continuous(type = 'viridis')



+
   scale_color_viridis(option = 'B')

pca_df$SOFA_score = as.factor(pca_df$SOFA_score)             
pca_df$In_Hospital_Mortality = as.factor(pca_df$In_Hospital_Mortality)
pca_df$Gram_stain = as.factor(pca_df$Gram_stain)             
pca_df$CRP = as.numeric(pca_df$CRP)


   scale_colour_brewer(palette = "Paired")

   
   scale_color_continuous(palette.colors( palette = "Heat"))

   scale_colour_brewer(palette = "Paired")
  

 
 
#geom_text_repel(aes(label = ifelse(PC2 > 12 | PC2 < -10, sample_name,'')), force = 0.5)
ggplot(pca_df, aes(x = PC1, y = PC2))+ 
   geom_point() 

ggplot(pca_df, aes(x = PC2, y = PC3)) + 
   geom_point(aes(colour = sample_name)) 

ggplot(subset(pca_df, type=="control"), aes(x = PC1, y = PC2)) + 
   geom_point(aes(colour = person_id)) 
# => again Control2 and Control6 outliers: there 2 measurements differ most also in PC space

ggplot(subset(pca_df, type=="iam"), aes(x = PC1, y = PC2)) + 
   geom_point(aes(colour = person_id)) +
   scale_colour_brewer(palette = "Set1") # Look for better color scheme

mypal30 = distinctColorPalette(30)
ggplot(subset(pca_df, type=="iam"), aes(x = PC1, y = PC2)) + 
   geom_point(aes(colour = person_id)) +
   #scale_colour_brewer(palette = mypal30)
   scale_color_manual(values = mypal30)

# New plot: everything below here not used!! 
mypal6 = distinctColorPalette(6)

ggplot(subset(pca_df, type=="iam"), aes(x = PC1, y = PC2)) + 
   geom_point(aes(colour = sampling_period)) +
   geom_vline(xintercept = 0) + 
   geom_hline(yintercept = 0) +
   scale_color_manual(values = mypal6) +
   facet_wrap(~ person_id)
#ggsave("PC1_PC2_iam_normalized.pdf", width = 297, height = 210, units = "mm")

ggplot(pca_df, aes(x = PC1, y = PC2)) + 
   geom_point(aes(colour = sampling_period)) 
ggplot(pca_df, aes(x = PC1, y = PC3)) + 
   geom_point(aes(colour = sampling_period)) 
ggplot(pca_df, aes(x = PC2, y = PC3)) + 
   geom_point(aes(colour = sampling_period)) 

ggplot(pca_df, aes(x = PC1, y = PC2)) + 
   geom_point(aes(colour = collecting_date)) 
ggplot(pca_df, aes(x = PC1, y = PC3)) + 
   geom_point(aes(colour = collecting_date)) 
ggplot(pca_df, aes(x = PC3, y = PC2)) + 
   geom_point(aes(colour = collecting_date)) 

ggplot(pca_df, aes(x = collecting_date, y = PC1)) + 
   geom_point(aes(colour = type)) + 
   geom_text_repel(aes(label = ifelse(collecting_date > "2019-02-15" & collecting_date < "2019-03-15", sample_name,'')), force = 0.5)

ggplot(pca_df, aes(x = collecting_date, y = PC1)) + 
   geom_point(aes(colour = batch)) 

ggplot(pca_df, aes(x = collecting_date, y = PC1)) + 
   geom_point(aes(colour = sampling_period)) 

### Within and between variance of IAM samples
ggplot(subset(pca_df, type=="iam"), aes(x = PC1, y = PC2)) + 
   geom_point(aes(colour = sampling_period)) +
   geom_vline(xintercept = 0) + 
   geom_hline(yintercept = 0) +
   facet_wrap(~ person_id)

pca_iam = as.data.table(subset(pca_df, type=="iam"))
class(pca_iam)

pcs = c("PC1","PC2","PC3")

person_means = pca_iam[,sapply(.SD, function(x) list(mean = mean(x))), .SDcols = pcs, by = person_id]

overall_mean = colMeans(pca_iam[,pcs,with=FALSE])
# colMeans(person_means[,c("PC1.mean","PC2.mean","PC3.mean")])

person_means$sum_sq_diff = (1/3) * (person_means$PC1.mean - overall_mean["PC1"])^2 +
   (person_means$PC2.mean - overall_mean["PC2"])^2 + 
   (person_means$PC3.mean - overall_mean["PC3"])^2

overall_mean = colMeans(pca_iam[,pcs,with=FALSE])

pca_iam = merge(pca_iam, person_means)
dim(pca_iam)
pca_iam[1:3,1:20]

pca_iam$sum_sq_diff = (1/3) * (pca_iam$PC1 - pca_iam$PC1.mean)^2 +
   (pca_iam$PC2 - pca_iam$PC2.mean)^2 +
   (pca_iam$PC3 - pca_iam$PC3.mean)^2 

person_var = pca_iam[,.(pers_var = mean(sum_sq_diff)), by = person_id]

person_var # -------------- interersting to look at the highest ones: most suspect
x = person_var[order(person_var$pers_var, decreasing = TRUE)]
# View(x)

pca_iam$sum_sq_diff_tot = (1/3) * (pca_iam$PC1 - overall_mean["PC1"])^2 +
   (pca_iam$PC2 - overall_mean["PC2"])^2 +
   (pca_iam$PC3 - overall_mean["PC3"])^2

var_within = sum(person_var$pers_var) / nrow(person_var)

var_between = sum(person_means$sum_sq_diff) / nrow(person_means)

var_total = sum(pca_iam$sum_sq_diff_tot) / nrow(pca_iam)

var_within
var_between
var_total

var_within / var_total

var_within + var_between

### For each sample the distance to the persons mean
pca_iam$pca3_dist_to_mean = sqrt(3 * pca_iam$sum_sq_diff)
pca_iam[order(pca3_dist_to_mean,decreasing = TRUE), c("sample_id","person_id","batch","sampling_period","pca3_dist_to_mean")][1:30,]

names(pca_iam)[1:10]
tail(names(pca_iam), 15)

sample_list = pca_iam[,c("sample_id","extern_sample_id","sample_name","person_id", "batch", "sampling_period", "gender", "age", "pca3_dist_to_mean")]  # this data frame to keep as result

save(sample_list, file = "samples_pca_dist_to_mean.RData")

#length(unique(sample_list$sample_name))
#length(unique(sample_list$sample_id))

# plot means and variances
person_df = merge(person_means, person_var, by = 'person_id')
head(person_df)
person_df$pers_sd = sqrt(person_df$pers_var)

ggplot(person_df, aes(x = PC1.mean, y= PC2.mean)) + 
   geom_point(aes(color = person_id)) + 
   geom_point(aes(color = person_id), size = 2 * person_df$pers_sd, alpha = 0.3) + 
   geom_text_repel(aes(label = person_id)) + 
   scale_color_manual(values = mypal30)

ggplot(person_df, aes(x = PC1.mean, y= PC3.mean)) + 
   geom_point(aes(color = person_id)) + 
   geom_point(aes(color = person_id), size = 2 * person_df$pers_sd, alpha = 0.3) + 
   geom_text_repel(aes(label = person_id))

ggplot(person_df, aes(x = PC3.mean, y= PC2.mean)) + 
   geom_point(aes(color = person_id)) + 
   geom_point(aes(color = person_id), size = 2 * person_df$pers_sd, alpha = 0.3) + 
   geom_text_repel(aes(label = person_id))

# ### Regression - useful?
# fit1 = lm(PC1 ~ batch, pca_df)
# summary(fit1)
# t.test(PC1 ~ batch, pca_df) # not pairwise!
# 
# t.test(PC1 ~ batch, data = pca_df, paired = TRUE) # must do this
# 
# t.test(PC2 ~ batch, data = pca_df, paired = TRUE) 
# t.test(PC3 ~ batch, data = pca_df, paired = TRUE) 
# t.test(PC4 ~ batch, data = pca_df, paired = TRUE) 
# t.test(PC5 ~ batch, data = pca_df, paired = TRUE) 

# ggplot(pca_df, aes(x = PC1, fill = batch)) + 
#    geom_histogram(aes(y=..density..), bins = 40, position="dodge") +
#    geom_density(alpha=.2)


# regressions for the first few components on various covariates 
names(pca_df)

f1 = lm(PC1 ~ gender + age + batch + sampling_period + freezer_time_num, subset(pca_df, type == "iam"))
f2 = lm(PC2 ~ gender + age + batch + sampling_period + freezer_time_num, subset(pca_df, type == "iam"))
f3 = lm(PC3 ~ gender + age + batch + sampling_period + freezer_time_num, subset(pca_df, type == "iam"))
f4 = lm(PC4 ~ gender + age + batch + sampling_period + freezer_time_num, subset(pca_df, type == "iam"))
f5 = lm(PC5 ~ gender + age + batch + sampling_period + freezer_time_num, subset(pca_df, type == "iam"))
anova(f1)
anova(f2)
anova(f3)
anova(f4)
anova(f5)


