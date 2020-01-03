Person <- R6::R6Class(
  classname = "Person",
  public = list(
    initialize = function(first_name, last_name, rights = c("user", "tutor", "admin")) {
      if (purrr::is_null(private$static$counter)) {
        private$static$counter <- 1
      } else {
        private$static$counter <- private$static$counter + 1
      }

      private$id <- as.character(private$static$counter)

      private$first_name <- first_name

      private$last_name <- last_name

      private$rights <- match.arg(rights)
    },

    get_id = function() {
      private$id
    },

    get_name = function() {
      private$last_name
    }
  ),
  private = list(
    first_name = character(),
    last_name = character(),
    rights = "user"
  )
)
