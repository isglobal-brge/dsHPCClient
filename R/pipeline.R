# Module: DAG Pipeline Constructors

#' Create a dsHPC DAG node
#'
#' @param id Character node identifier.
#' @param step A `dshpc_step` object.
#' @param inputs Optional upstream node ids, named inputs, or input descriptors.
#' @return A `dshpc_pipeline_node`.
ds_pipeline_node <- function(id, step, inputs = NULL) {
  .deprecated_domain_api("ds_pipeline_node")
  if (!is.character(id) || length(id) != 1 || !nzchar(id))
    stop("'id' must be a non-empty string.", call. = FALSE)
  if (!inherits(step, "dshpc_step") && !is.list(step))
    stop("'step' must be a dshpc_step or list.", call. = FALSE)
  step <- as.list(step)
  class(step) <- NULL
  node <- list(id = id, step = step)
  if (!is.null(inputs)) node$inputs <- inputs
  class(node) <- c("dshpc_pipeline_node", "list")
  node
}

#' Create a dsHPC DAG pipeline
#'
#' A pipeline is a directed acyclic graph of named nodes. The server validates
#' the graph, topologically orders it, and executes it through the normal
#' durable dsHPC job machinery.
#'
#' @param nodes List of nodes created by `ds_pipeline_node()`.
#' @return A `dshpc_pipeline`.
ds_pipeline <- function(nodes) {
  .deprecated_domain_api("ds_pipeline")
  if (!is.list(nodes) || length(nodes) == 0)
    stop("'nodes' must be a non-empty list.", call. = FALSE)
  node_names <- names(nodes) %||% rep("", length(nodes))
  out <- vector("list", length(nodes))
  for (i in seq_along(nodes)) {
    node <- nodes[[i]]
    if (!inherits(node, "dshpc_pipeline_node") && !is.list(node))
      stop("Node ", i, " is not a pipeline node.", call. = FALSE)
    node <- as.list(node)
    class(node) <- NULL
    if (is.null(node$id) && nzchar(node_names[[i]])) node$id <- node_names[[i]]
    out[[i]] <- node
  }
  pipeline <- list(nodes = out)
  class(pipeline) <- c("dshpc_pipeline", "list")
  pipeline
}

#' @export
print.dshpc_pipeline <- function(x, ...) {
  cat("dshpc_pipeline\n")
  cat("  Nodes:", length(x$nodes), "\n")
  for (node in x$nodes) {
    step <- node$step %||% node
    cat("  - ", node$id %||% "<unnamed>", ": ", step$type %||% "<unknown>",
        if (!is.null(step$runner)) paste0(" runner=", step$runner) else "",
        "\n", sep = "")
  }
  invisible(x)
}
