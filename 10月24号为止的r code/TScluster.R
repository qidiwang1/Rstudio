library(TSclust)
library(dtw)
library(rJava)
library(dtwclust)
data('uciCT')
typeof(CharTraj)
View(CharTraj[[1]])
proxy::pr_DB$set_entry(FUN = diss.ACF, names = c("ACFD"),
                       loop = TRUE, type = "metric", distance = TRUE,
                       description = "Autocorrelation-based distance")
# Normalized DTW
ndtw <- function(x, y, ...) {
  dtw(x, y, ...,
      step.pattern = asymmetric,
      distance.only = TRUE)$normalizedDistance
}
# Register the distance with proxy
proxy::pr_DB$set_entry(FUN = ndtw, names = c("nDTW"),
                       loop = TRUE, type = "metric", distance = TRUE,
                       description = "Normalized, asymmetric DTW")
# Partitional clustering
tsclust(CharTraj[1L:10L], k = 2L,
        distance = "nDTW", seed = 838)
# Reinterpolate to same length
data <- reinterpolate(CharTraj, new.length = max(lengths(CharTraj)))

# Calculate the DTW distances between all elements
system.time(D1 <- proxy::dist(data[1L:5L], data[6L:100L],
                              method = "dtw_basic",
                              window.size = 20L))
# Nearest neighbors
NN1 <- apply(D1, 1L, which.min)
# Calculate the distance matrix with dtw_lb
system.time(D2 <- dtw_lb(data[1L:5L], data[6L:100L],
                         window.size=20L))
# Nearest neighbors
NN2 <- apply(D2, 1L, which.min)
# Same results?
all(NN1 == NN2)

# Exclude a series as an example
database <- data[-100L]
classify_series <- function(query) {
  d <- dtw_lb(database, query, window.size = 18L, nn.margin = 2L)
 # Return label of nearest neighbor
  CharTrajLabels[which.min(d)]
}
# 100-th series is a Z character
classify_series(data[100L])  

hc_sbd <- tsclust(CharTraj, type = "h", k = 20L,
                  preproc = zscore, seed = 899,
                  distance = "sbd", centroid = shape_extraction,
                  control = hierarchical_control(method = "average"))
# By default, the dendrogram is plotted in hierarchical clustering
plot(hc_sbd)  

# The series and the obtained prototypes can be plotted too
plot(hc_sbd, type = "sc")  
  
# Focusing on the first cluster
plot(hc_sbd, type = "series", clus = 1L)
plot(hc_sbd, type = "centroids", clus = 1L)  

library(cluster)
tsclust(CharTraj[1L:20L], type = "h", k = 4L,
        distance = "dtw_basic",
        control = hierarchical_control(method = diana),
        args = tsclust_args(dist = list(window.size = 18L)))

# Reinterpolate to same length
data <- reinterpolate(CharTraj, new.length = max(lengths(CharTraj)))

# z-normalization
data <- zscore(data[60L:100L])
pc_dtw <- tsclust(data, k = 4L,
                  distance = "dtw_basic", centroid = "dba",
                  trace = TRUE, seed = 8,
                  norm = "L2", window.size = 20L,
                  args = tsclust_args(cent = list(trace = TRUE)))

pc_dtwlb <- tsclust(data, k = 4L,
                    distance = "dtw_lb", centroid = "dba",
                    trace = TRUE, seed = 8,
                    norm = "L2", window.size = 20L,
                    control = partitional_control(pam.precompute = FALSE),
                    args = tsclust_args(cent = list(trace = TRUE)))

pc_ks <- tsclust(data, k = 4L,
                 distance = "sbd", centroid = "shape",
                 seed = 8, trace = TRUE)

pc_tp <- tsclust(data, k = 4L, type = "t",
                 seed = 8, trace = TRUE,
                 control = tadpole_control(dc = 1.5,
                                           window.size = 20L))

