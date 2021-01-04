library(data.table)
library(ggplot2)
library(GGally)
library(ggrepel) 
library(randomcoloR)
library("readxl")
library("reshape2")
library(dplyr)

#adapted for OLINK control bridge check-up plot
iam = IAM_control_Olink_check

iam = melt(iam, id.vars = c("ID"),
         variable.name = "assay", value.name = "npx")


iam = as.data.table(iam)
iam$npx = as.numeric(iam$npx)
View(is.na(iam$npx))
### Standardize the data
# Note that value_n is the result of batch normalization
iam[, value_ns := scale(npx), by = assay]  # scale (mean zero, sd 1) #important to set as dataframe


x = as.data.frame(dcast.data.table(iam, ID ~ assay, 
                                   value.var = "value_ns"))


#sum(is.na(x_mat))
#summary(is.na(x))
#x = x[-393,] 
#x2 = x[-1,] # x 12 times; as first twelve lines (controls) as well as line 393 do not contain metadata
# remove first twelve lines

x_mat = as.matrix(x)
x_mat = x_mat[,-(1)] 
row.names(x_mat) = as.character((x$ID))  #difficulty because of duplicate bridge samples rownames reason for weir rows
t = t(x)
t = t[-1,]

colnames(t) = x$ID
x_matr = as.matrix(t)  



x_dist_L2 = dist(x_mat, method = "euclidean")# compute distances between samples, needed for clustering
x_dist_L2r = dist(x_matr, method = "euclidean")  # compute distances between samples, needed for clustering

x_dist_L1 = dist(x_mat, method = "manhattan")  # compute distances between samples, needed for clustering


#dim(as.matrix(x_dist_L1))
#dim(as.matrix(x_dist_L2))
#x_dist_L1[1:5, 1:5]



clust_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")

clust_list_L1 = list()
clust_list_L2 = list()

for (i in 1:length(clust_methods)) {
   clust_list_L1[[clust_methods[i]]] = hclust(x_dist_L1, method = clust_methods[i])
   clust_list_L2[[clust_methods[i]]] = hclust(x_dist_L2, method = clust_methods[i])
}

x_clust = hclust(x_dist_L2)
x_clustr = hclust(x_dist_L2r, method = 'ward.D')


class(clust_list_L1[[1]])
#summary(clust_list_L1[[1]])#to summarize clustering with ward D method

#x_clust
#summary(x_clust)
plot(x_clust) #use toch check matrix

# Look at using pheatmap (or aheatmap)

# refs
# https://slowkow.com/notes/pheatmap-tutorial/


library(pheatmap)

# pheatmap(mat = as.matrix(x_dist), show_colnames = FALSE) simplified no options code

my_annot = data.frame(gender = x$Sex, Batch = x$Batch,
                      age = x$Age, Comorbidities = x$CCI, GRAM_stain = x$Gram_stain, Diagnosis = x$Diagnosis,
                      person = x$ID, SOFA = x$SOFA_score, Severity_group = x$severity_groups) 
#dim(my_annot)
#rownames(my_annot) <- colnames(x[10:101])
#head(my_annot)
rownames(my_annot) = colnames(x_matr) #CRUCIAL ONE

pal6 = distinctColorPalette(20)
tail(my_annot)


#mycolors = list(period = c("M"))

get_colors = function(X) {
   res = list()
   for (v in names(X)) { 
      lev = levels(X[,v])
      p = distinctColorPalette(length(lev))
      names(p) = lev
      res[[v]] = p
   }
   return(res)
}

my_colors = get_colors(my_annot[,c("gender")])

my_colors[['batch']] = c('batch1'='#fc8d59', 'batch2'='#ffffbf') 
my_colors[['gender']] = c('1'='#FF69B4', '0'='#00BFFF')
my_colors[['Diagnosis']] = c('other'='#333131', 'Pneumonia'='#DA5353', 'Influenza' = '#1ECD41', 'BSI & Endocarditis' = '#3B77F0') 
my_colors[['Gram_stain']] = c('0'='#35A513', '1'='#25A2D8', '2' = '#A621C7') 
my_colors[['Severity_group']] = c('1'='#F6F6F6', '2'='#170606') 


#made for sepsis

#mat = as.matrix(x_dist_L2)   
#colnames(mat) = colnames(t) #was this important or not? 
# now use hclust results, rather than pheatmap default, which is k-means

pheatmap(mat = as.matrix(x_dist_L2),
         fontsize = 6,
         cluster_cols = x_clust,
         cluster_rows = x_clustr,
         main = "Hierarchical clustering (complete)")

# to files:
for (i in 1:length(clust_methods)) {
   clust_list_L1[[clust_methods[i]]] = hclust(x_dist_L1, method = clust_methods[i])
   clust_list_L2[[clust_methods[i]]] = hclust(x_dist_L2, method = clust_methods[i])
   
   fnL1 = paste("hclust_L1_", clust_methods[i], ".pdf", sep = "")
   fnL2 = paste("hclust_L2_", clust_methods[i], ".pdf", sep = "")

   
   pheatmap(mat = as.matrix(x_dist_L1), 
            show_colnames = TRUE, 
            show_rownames = TRUE,
            annotation_col = my_annot,
            fontsize = 5,
            cluster_cols = clust_list_L1[[i]],
            cluster_rows = x_clustr,
            annotation_colors = my_colors,
            main = paste("Hierarchical clustering (L1, ", clust_methods[i],")", sep = ""),
            filename = fnL1,
            width = 8.3,
            height = 11.7)
   
   flush.console()
   while (!is.null(dev.list()))  dev.off()
   flush.console()
   
   pheatmap(mat = as.matrix(x_dist_L2),
            show_colnames = F,
            show_rownames = TRUE,
            labels_row = rownames(t),
            annotation_col = my_annot,
            fontsize = 5,
            cluster_cols = clust_list_L2[[i]],
            cluster_rows = x_clustr,
            annotation_colors = my_colors,
            main = paste("Hierarchical clustering (L2, ", clust_methods[i], ")", sep = ""),
            filename = fnL2,
            width = 8.3,
            height = 11.7)
   flush.console()
   while (!is.null(dev.list()))  dev.off()
   flush.console()

}

#for sepsis project best clustering choice will also be based on this ID-Protein heatmap


### Conclusion, we proceed with L2, Ward.D: best clustering
the_clust = clust_list_L2[["ward.D"]]
class(the_clust)

the_clust$order

# if samples cluster by person, the we can use the 6x6 submatrices in x_dist_L2

# compute means by person, not relevant for sepsis
dim(x)
OIDcols = names(x)[grepl(pattern = "OID", x = names(x))]
length(OIDcols)
x_dt = data.table(x)
person_means = x_dt[,sapply(.SD, function(x) list(mean = mean(x))), .SDcols = OIDcols, by = person_id]
dim(person_means)
# person_means[1:5,1:8]
# x_dt[1:5, 1:8]
names(person_means) = sub(pattern = ".mean", replacement = "", x= names(person_means)) 
person_means_df = as.data.frame(person_means)
class(person_means_df)

x$dist_to_clust_mean = -1
for (i in 1:nrow(x)) {
   pers_i = x$person_id[i]
   mean_i = unlist(person_means_df[person_means_df$person_id == pers_i, OIDcols])
   this_i = unlist(x[i, OIDcols])
   x$dist_to_clust_mean[i] = sqrt(sum((mean_i - this_i)^2))
}

sample_list_clust = x[,c("sample_name","person_id", "batch", "sampling_period", "dist_to_clust_mean")]  # this data frame to keep as result
head(sample_list_clust)
save(sample_list_clust, file = "samples_clust_dist_to_mean.RData")


################################################################################
## now k-means 
dim(x_mat)

# NOTE THERE IS RANDOMNESS THAT AFFECTS THE RESULT, 
# Therefore choose centers to start from manually, eg. the first sample of each person
row.names(x_matr)
start_centers = x_mat[seq(from=1, to=180, by=6),]
start_centers = x_mat[seq(from=1, to=2),]


# x_kmeans = kmeans(x_mat, centers = start_centers, iter.max = 100) not for sepsis centers?
x_kmeans = kmeans(x_mat, centers = 2, iter.max = 50)
dim(x_kmeans)
x_kmeans$iter
summary(x_kmeans$cluster)


all.equal(names(x_kmeans$cluster), x$ID) #to check whether both objects take same samples into account 

x$cluster = x_kmeans$cluster

mypal30 = distinctColorPalette(4)

ggplot(x, aes(x = ID, y = cluster)) + 
   geom_point(aes(colour = Diagnosis)) + 
   scale_color_manual(values = mypal30) 

x$cluster = as.factor(x$cluster)
summary(x$severity_groups)
summary(x$cluster)
ggscatter(as.data.frame(x), x = "ID", y = "severity_groups", 
          color = 'cluster',
          size = 1, repel = FALSE)
          
          scale_color_continuous(type = 'viridis'))
sum(x$cluster)

x$cluster = as.factor(x$cluster)


ggplot(x, aes(x = sample_name, y = cluster)) + 
   geom_point(aes(colour = person_id), size=3) + 
   scale_color_manual(values = mypal30) + 
   facet_wrap(~ person_id)

a = as.data.frame(xtabs( ~ person_id + cluster, x))
head(a)
a = subset(a, Freq > 0)
a
a[order(a$person_id),]

#
