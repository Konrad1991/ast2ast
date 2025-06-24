files <- list.files("./R/", full.names = TRUE)
trash <- lapply(files, source)

f <- function(a, b) {
  c <- 1
  if (a == 1) {
    if (b == 2) {
      c <- 4.
    }
    c <- 3
  }
  d <- c + b
}

type_infer <- type_inference$new(f)
ast <- process(body(f), "Start", TRUE, type_infer)
edges <- extract_edges(type_infer$all_nodes)
sorted_ids <- topo_sort(edges)
type_infer$all_nodes[sorted_ids]
type_infer$all_nodes
