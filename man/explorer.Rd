% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explorer.R
\name{explorer}
\alias{explorer}
\alias{explorer_ui}
\title{Explorer}
\usage{
explorer_ui(id)

explorer(
  input,
  output,
  session,
  .values,
  .root_node_r,
  .explorer_classes = list(),
  addable_r = shiny::reactive("__group__"),
  visible_r = shiny::reactive("__group__"),
  .display_header = TRUE,
  .label_list = label_explorer(),
  .state = list()
)
}
\arguments{
\item{id}{The module's id.}

\item{input, output, session}{Called by \code{\link[shiny:callModule]{callModule}}.}

\item{.values}{The \code{.values} list.}

\item{.root_node_r}{A \code{\link[shiny:reactive]{reactive}} returning an
object of class \code{\link{ExplorerNode}}. This needs not necessarily to be the
root node of an object of class \code{\link{ExplorerTree}}.}

\item{.explorer_classes}{A \code{\link[base]{list}} of objects of class
\code{\link{ExplorerClass}}.}

\item{addable_r}{A \code{\link[shiny:reactive]{reactive}}
returning a \code{\link[base:character]{character}} vector containing the labels
of explorer classes that are addable to the explorer.}

\item{visible_r}{A \code{\link[shiny:reactive]{reactive}}
returning a \code{\link[base:character]{character}} vector containing the labels
of explorer classes that are displayed to the user.}

\item{.display_header}{If \code{\link[base:logical]{TRUE}}, the navigation
header is displayed, otherwise it is not.}

\item{.label_list}{A \code{\link[base]{list}} created with \code{\link{label_explorer}}
containing labels for all buttons used inside the explorer module.}

\item{.state}{A \code{\link[base]{list}} which is passed to every explorer
class server function. Use this list to implement special behaviour of an
explorer class dependent on conditions outside of the explorer.}
}
\value{
The \code{explorer} module returns a list containing the following reactives, that
you may access in the calling server function.
\tabular{ll}{
  \code{contextmenued_node_r} \tab An object of class \code{\link{ExplorerNode}}. This is
  the node, which has been last contextmenued. \cr
  \code{current_node_r} \tab An object of class \code{\link{ExplorerNode}}. This is the node,
  whose children are currently displayed in the explorer's datatable.
  \code{selected_child_node_r} An object of class \code{\link{ExplorerNode}}. This is the
  node, which has been last clicked.
}
}
\description{
Shiny module representing an explorer usable for selecting elements of different
kinds and invoking actions on them.
}
