library(httr)

# Auth0 custom rule streams relevant user info to Keen.io after successful sign-in. Use a saved Keen.io query
# to retrieve the latest streamed event to identify the user within Shiny. It's lame having to use these third-party
# platforms, but as of now, unable to authenticate in open-source shiny-server AND read authentication credentials into
# Shiny. This is still a janky solution (ie. what if 2 users sign-in at the same time?). Would rather use the
# request IP to help identify the right user info in Keen.io but this is also not exposed in Shiny.
# Awaiting resolution of https://github.com/rstudio/shiny/issues/141
Sys.sleep(1) # Allow time for KeenIO to register data-stream from Auth0
keen_req <- GET(
	url = paste("https://api.keen.io/3.0", Sys.getenv("DIAGNOSTIC_KEEN_PROJECT_ID"), "queries/saved", 
		tolower(Sys.getenv("DIAGNOSTIC_KEEN_QUERY")), "result", sep = "/"),
	add_headers("Authorization" = Sys.getenv("DIAGNOSTIC_KEEN_READKEY"), "Content-Type" = "application/json")
)
USER <- content(keen_req)[["result"]]
if ( length(USER) < 1) {
	USER$name <- sQuote("Unknown")
	USER$group <- "Guest"
	USER$id    <- 0
} else if ( length(USER) > 1) {
	timestamp <- strptime( sapply(USER, function(x) x$keen$timestamp), "%Y-%m-%dT%H:%M:%OSZ")
	USER <- USER[[ which(timestamp == min(timestamp)) ]]
} else {
	USER <- USER[[1]]
}

capitalize    <- function(x) paste0(toupper(substring(x, 0, 1)), substring(x, 2, nchar(x)))
ENTITY_FIELDS <- c("symptoms", "conditions")
EDIT_FIELDS   <- c("add", "delete", "change")