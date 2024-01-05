library(igraph)
g <- make_tree(20, children = 3, mode = "undirected")
plot(g,
     vertex.size = 20, 
     vertex.color = "white")
x = eigen_centrality(g)
x$vector
x$value

plot(g, 
     vertex.size = 25, 
     vertex.color = "white",
     vertex.label = round(x$vector, 2),
     vertex.label.cex = 0.80)

# Eigenvector centrality (power method)
#INPUT
# g = graph
# t = precision
# OUTPUT
# A list with:
# vector = centrality vector
# value = eigenvalue
# iter = number of iterations

get_ev_centrality <- function(g, tol, max_iter = 100) {
  
  eps = 1/10^tol
  adj = as_adjacency_matrix(g)
  x = rep(1, nrow(adj))
  change = Inf
  iter = 0 
  while(change > eps && iter < max_iter) {
    
    xold = x
    x = as.vector(x %*% adj)
    m = x[which.max(abs(x))]
    x = x/m
    
    change = sqrt(sum((x - xold)^2))
    iter = iter + 1
  }
  if (iter == max_iter) warning("Power method did not converge with desired precision")
  
  out = list(vector = x, value = m, iter = iter)
  
}

my_x = get_ev_centrality(g, 1.5)
rbind(my_x$vector, x$vector)


g = make_graph("Bull")
plot(g)
x_bull = eigen_centrality(g)
my_x_bull = get_ev_centrality(g, 5)
rbind(my_x_bull$vector, x_bull$vector)



g = make_bipartite_graph(types = c(0,0,0, 1,1,1), 
                         edges = c(1,4, 1,5, 1,6, 2,4, 3,5))

plot(g)

x_bi = eigen_centrality(g)
my_x_bi = get_ev_centrality(g, 5)
rbind(my_x_bi$vector, x_bi$vector)













