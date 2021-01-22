library(lsa)
install.packages("lsa")
library(lsa)
library(ggplot2)


#colnames(t) = x$ID
#t = t[-1,]
#row.names(xcos) = as.character((xcos$ID))  #difficulty because of duplicate bridge samples rownames reason for weir rows
#t = as.matrix(t)
#cos_sim = cosine(t)
#t = as.data.frame(t)

#Het dataframe in kwestie is een 'reshape2::melt' van de output van cosine()
cos_sim = as.matrix(scatterplots_input_test)
cos_sim = cosine(cos_sim)
cos_orig_melt = reshape2::melt(cos_sim)

ggplot(cos_orig_melt, aes(x=Var1, y=Var2, fill=value)) +
  
  geom_tile() +
  
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       
                       midpoint = 0.985, limit = c(0.9700, 1), space = "Lab",
                       
                       name = "cosine similarity") +
  
  theme_minimal() +
  
  xlab("") + ylab("") +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 1,size = 8, hjust = 1),
        
        axis.text.y = element_text(size = 8))
