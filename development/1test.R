files <- list.files("./R/", full.names = TRUE)
trash <- lapply(files, source)

f <- function(a, b) {
  c <- a + b
  r  <- 2
  pi <- 3.14
  if (c == 2) {
    if (c == 3) {
      r <- 4
    } else {
      r <- 5
    }
  }
  a <- r*pi*pi
}

env <- new.env(parent = emptyenv())
env$id <- 3 # 1 is function argument & 2 is mock
env$last_assignment <- create_last_assignment_list(f)
env$all_nodes <- list()
env$in_if_node <- FALSE
ast <- process(body(f), "Start", TRUE, env)
edges <- extract_edges(env$all_nodes)
sorted_ids <- topo_sort(edges)
env$all_nodes[sorted_ids]
env$all_nodes
