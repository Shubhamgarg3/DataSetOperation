NRows=100
NCols=30

ds1 <- matrix(runif(NCols*NRows, min=1, max=200), ncol=NCols)
class(ds1)
head(ds1)

# (i) 
ds1[10:60,] <- NA
num_rows_with_na <- sum(rowSums(is.na(ds1))>0)
cat("Number of rows with missing values:", num_rows_with_na)

# (ii)
ds1 <- apply(ds1, 2, function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))

#(iii)
correlation <- cor(ds1)
heatmap(correlation)
selected_columns <- which(apply(correlation, 2, function(x) sum(x<=0.7))>0)

# (iv)
ds1 <- apply(ds1, 2, function(x){
  min_val <- min(x)
  max_val <- max(x)
  (x-min_val)/(max_val-min_val) * 10
})

# (v)
ds1 <- ifelse(ds1<=0.5,1,0)
