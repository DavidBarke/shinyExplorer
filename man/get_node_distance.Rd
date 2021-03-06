% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_node_distance}
\alias{get_node_distance}
\title{Get distance of a node to an ancestor node}
\usage{
get_node_distance(node, ancestor_node)
}
\arguments{
\item{node, ancestor_node}{An object of class \code{\link{ExplorerNode}}.

If \code{node} is the ancestor node, \code{0} is returned. If \code{ancestor_node}
is no ancestor of \code{node}, \code{\link[base:NULL]{NULL}} is returned.}
}
\description{
Get the distance of a node to another node in the tree
}
