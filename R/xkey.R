# The verb xkey sets the primary keys in a table.
xkey <- function(d, keys) {
  attributes(d)$key <- keys
  d
}