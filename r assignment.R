data("mtcars")
df <- mtcars 
df
dim(mtcars)
names(mtcars)
str(mtcars)
summary(mtcars)


##the distribution and relationships between variables relevant to MPG

library(metan)
corrl<-corr_coef(df,method="pearson")
plot(corrl)

cor(df)

pairs(df)





#distribution and relationship between variables mpg and cyl

cor.test(mtcars$mpg, mtcars$cyl)

# correlation matrix
data <- mtcars[, c("mpg", "cyl")]
cor_matrix <- cor(data)

# heatmap with controlled size
library(pheatmap)
pheatmap(cor_matrix, 
         display_numbers = TRUE,  
         color = colorRampPalette(c("red", "white", "green"))(20), 
         main = "Correlation Heatmap: mpg vs cyl",
         cellwidth = 60,      
         cellheight = 60,     
         fontsize = 14,       
         fontsize_row = 14,   
         fontsize_col = 14    
)



#distribution and relationship between variables mpg and disp


cor.test(mtcars$mpg, mtcars$disp)




data <- mtcars[, c("mpg", "disp")]
cor_matrix <- cor(data)


library(pheatmap)
pheatmap(cor_matrix, 
         display_numbers = TRUE,  
         color = colorRampPalette(c("red", "white", "green"))(20), 
         main = "Correlation Heatmap: mpg vs disp",
         cellwidth = 60,      
         cellheight = 60,     
         fontsize = 14,       
         fontsize_row = 14,   
         fontsize_col = 14    
)


#distribution and relationship between variables mpg and hp

# Select variables of interest
vars <- c("hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")

# Loop through variables and perform correlation test with mpg
for (v in vars) {
  test <- cor.test(mtcars$mpg, mtcars[[v]],method="pearson")
  
  cat("Correlation Test: mpg vs", v, "\n")
  cat("  Correlation coefficient:", round(test$estimate, 3), "\n")
  cat("  t =", round(test$statistic, 4), 
      ", df =", test$parameter, 
      ", p-value =", signif(test$p.value, 5), "\n")
  cat("  95% CI:", paste(round(test$conf.int, 3), collapse = " to "), "\n\n")
}




install.packages("pheatmap")  
library(pheatmap)


vars <- c("hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")


for (v in vars) {
  test <- cor.test(mtcars$mpg, mtcars[[v]])
  
  cat("Correlation Test: mpg vs", v, "\n")
  cat("  Correlation coefficient:", round(test$estimate, 3), "\n")
  cat("  t =", round(test$statistic, 4), 
      ", df =", test$parameter, 
      ", p-value =", signif(test$p.value, 5), "\n")
  cat("  95% CI:", paste(round(test$conf.int, 3), collapse = " to "), "\n\n")
}


data <- mtcars[, c("mpg", vars)]
cor_matrix <- cor(data)


pheatmap(cor_matrix,
         display_numbers = TRUE,   # show correlation values                                         
         color = colorRampPalette(c("red", "white", "green"))(50),
         main = "Correlation Heatmap: mpg vs Selected Variables",
         cellwidth = 40, cellheight = 40, fontsize = 12)                                                          


library(ggplot2)
library(gridExtra)
df <- mtcars


vars <- c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")


plot_list <- list()


for (v in vars) {
  p <- ggplot(df, aes_string(x = v, y = "mpg")) +
    geom_point(color = "blue", size = 2) +
    ggtitle(paste("MPG vs", v)) +
    theme_minimal()
  plot_list[[v]] <- p
}


do.call(grid.arrange, c(plot_list, ncol=3))




