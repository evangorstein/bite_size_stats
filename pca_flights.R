library(nycflights13)
library(tidyverse)
library(lubridate)

# add a column that represents date
data = flights %>%
  mutate(date = as.character(interaction(day, month)))

# cross-tabulate to obtain a matrix of counts of flights to each destination on each date
# dates index rows; destinations index columns
mat <- xtabs(~ date + dest, data=data) %>% #dense format
  DescTools::as.matrix.xtabs() 
sp_mat <- xtabs(~ date + dest, data=data, sparse=TRUE) #sparse format

# regularize this matrix of counts
## first, take the square root 
sp_mat@x = sqrt(sp_mat@x)
mat = sqrt(mat)
## further regularize it with the following function
glaplacian <- function(A, regularize = TRUE) {
  # deg_row <- Matrix::rowSums(abs(A))
  # deg_col <- Matrix::colSums(abs(A))
  deg_row <- Matrix::rowSums(A!=0)
  deg_col <- Matrix::colSums(A!=0)
  
  tau_row <- mean(deg_row)
  tau_col <- mean(deg_col)
  
  D_row = Matrix::Diagonal(nrow(A), 1 / sqrt(deg_row + tau_row))
  D_col = Matrix::Diagonal(ncol(A), 1 / sqrt(deg_col + tau_col))
  L = D_row %*% A %*% D_col
  rownames(L) = rownames(A)
  colnames(L) = colnames(A)
  return(L)
}
sp_mat = glaplacian(sp_mat)
mat = glaplacian(mat)

# perform pca with k=6 and extract "row features" (aka matrix U) and "column features" (aka matrix V)
k <- 6
pcs <- prcomp(mat, rank=k, center=FALSE) 
## the column features are easy to extract, as they correspond exactly to "rotation" from the prcomp output
col_features <- as_tibble(pcs$rotation, rownames = "dest")
## the row features are more difficult to extract, as prcomp doesn't just give us the U matrix, 
## but rather a version scaled by singular values 
### first, we must obtain the singular values by scaling pcs$sdev
scaling_factor <- sqrt(nrow(mat)-1)
singular_values <- scaling_factor * pcs$sdev[1:k]
### now we can obtain the row features by scaling pcs$x by the signular values
inv_sigma <- diag( 1 / singular_values )
U <- pcs$x %*% inv_sigma
colnames(U) <- paste0("PC", 1:k)
row_features <- as_tibble(U, rownames="date")

# plot row features
row_features %>%
  mutate(date = dmy(date, truncated=1)) %>% #convert type char -> Date
  select(date, contains("PC")) %>% 
  pivot_longer(contains("PC"), names_to = "pc_dimension", values_to = "loadings") %>% 
  ggplot(aes(x = date, y = loadings)) + geom_line() + 
  facet_wrap(~pc_dimension, scales= "free") + geom_smooth()

# plot column features
airport_data <- col_features %>%
  left_join(airports %>% select(dest=faa, lat,lon)) %>%
  pivot_longer(contains("PC"),
               names_to = "pc_dimension", values_to = "loadings") 
library(maps)
usa_map <- map_data("state")
p <- ggplot() + 
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +
  coord_fixed(1.3, xlim = c(-125, -65), ylim = c(25, 50))

p + geom_point(data = airport_data, aes(x = lon, y = lat, 
                                       size = abs(loadings), color = loadings)) +
  facet_wrap(~ pc_dimension)  +
  scale_color_gradient2(low = "red", high = "blue", mid = "white")


