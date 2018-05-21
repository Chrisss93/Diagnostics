capitalize    <- function(x) paste0(toupper(substring(x, 0, 1)), substring(x, 2, nchar(x)))
ENTITY_FIELDS <- c("symptoms", "conditions")
EDIT_FIELDS   <- c("add", "delete", "change")