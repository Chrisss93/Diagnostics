library(shiny)
library(shinyWidgets)
library(DBI)
library(visNetwork)
library(dplyr)
# TODO: Disconnect database better...consider switching to `pool`?
# TODO: Call keenIO user-info API asynchronously (needs a Sys.sleep call to wait on KeenIO) with `future`, `promises`
#		Or if that is complicated, in the add shinyjs loading screen in the meantime.
# TODO: Dynamic focus. 
# TODO: Consider switching network visualization to `igraph`. Don't like how vis.js looks right now
options(stringsAsFactors = FALSE)
cred <- list(
	drv      = RMariaDB::MariaDB(), 
	host     = Sys.getenv("DIAGNOSTIC_SQL_HOST"),
	user     = Sys.getenv("DIAGNOSTIC_SQL_USER"), 
	password = Sys.getenv("DIAGNOSTIC_SQL_PWD"), 
	db       = Sys.getenv("DIAGNOSTIC_SQL_DB"))
single     <- function(x) gsub("(?<!s)s$", "", x, perl = TRUE)
quickNames <- function(x, val, op) names(x[op(x, val)])
visBoiler  <- function(x, frozen = TRUE, ord = TRUE) {
	hfunc <- if (ord) visHierarchicalLayout else identity
	ffunc <- if (frozen) function(x) visInteraction(x, dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) 
	else identity
	
	x %>% hfunc() %>% ffunc() %>% 
		visEdges(color = "black") %>% 
		visNodes(font = list(size = 20))
}
import::from("glue", "glue_sql")

server <- function(input, output, session) {
	USER <- keenUser()
	db <- do.call("dbConnect", cred)
	session$onSessionEnded( function() { dbDisconnect(db); print("Database connection terminated.") } )
	
	# Some observers read from database, others write to database. I want the read-observers to react to  
	# the write-observers. The most elegant way I can think of doing this is to make the connection object
	# itself a reactive value. Works well, saves me boilerplate code, but final issue is how to disconnect?
	# onSessionEnded wouldn't work because the reactive connection object must be called in a reactive context. hmm...
	# isolate() ?
	react_val <- reactiveValues(
		dbChanged = expand.grid(Action = EDIT_FIELDS, Entity = ENTITY_FIELDS, Edits = 0, stringsAsFactors = FALSE),
		dbTrigger = 0,
		changeVisReset = 0,
		queries   = character(0)
	)
	observeEvent( react_val$dbChanged, {
		dbDisconnect(db)
		db <<- do.call("dbConnect", cred)
		react_val$dbTrigger <- react_val$dbTrigger + 1
	})

	STATES <- eventReactive(react_val$dbTrigger, {
		dta <- dbGetQuery(db, glue_sql("SELECT id, name, type FROM states WHERE userId = {user}", user = USER$id,
									   .con = db))
		
		labels  <- setNames(nm = ENTITY_FIELDS, 
			list(setNames(dta$id[dta$type == "symptom"], dta$name[dta$type == "symptom"]),
				setNames(dta$id[dta$type == "condition"], dta$name[dta$type == "condition"]))
		)
		attr(labels, "data") <- dta
		labels
	})
	otherType <- reactive( setdiff(ENTITY_FIELDS, input$editType) )
	typeFields <- reactive({
		if (input$editType == "symptoms") {
			func <- identity
		} else {
			func <- rev
		}
		func(c("symptomId", "conditionId"))
	})
	
	#########################
	# Dashboard
	#########################
	# 1. Symptom-condition look-up
	output$symptomSelect <- renderUI({
		pickerInput("symptoms", NULL, STATES()[["symptoms"]], multiple = TRUE, 
			options = list("live-search" = TRUE, "actions-box" = TRUE, "none-selected-text" = "Enter symptoms here"))
	})
	
	diagnosis <- eventReactive(input$symptoms, {
		query <- glue_sql(
			"SELECT d.*, s.name AS symptoms, c.name AS conditions, d.symptomId IN ({symp*}) AS symptomPresent", 
			" FROM diagnosis d", 
			" INNER JOIN states s ON s.id = d.symptomId",
			" INNER JOIN states c ON c.id = d.conditionId",
			" WHERE s.userId = {user} AND conditionId IN (",
				" SELECT conditionId FROM diagnosis WHERE symptomId IN ({symp*}))", 
			user = USER$id, symp = as.numeric(input$symptoms), .con = db)
		
		dbGetQuery(db, query)
	})
	
	output$conditionSelect <- renderTable({
		diagnosis() %>% 
			group_by(conditionId, conditions) %>% 
			summarize(Precision = length(intersect(as.numeric(input$symptoms), symptomId)) / n(),
					  Recall    = Precision * n() / length(input$symptoms)) %>% 
			arrange(desc(Recall), desc(Precision)) %>% 
			mutate_if(is.numeric, scales::percent) %>% 
			ungroup() %>% 
			select(Condition = conditions, Recall, Precision)
	})
	# 2. Symptom-condition network analysis
	output$net <- renderVisNetwork({
		nodes <- data.frame(
			id    = c(diagnosis()$symptomId, diagnosis()$conditionId),
			label = c(diagnosis()$symptoms, diagnosis()$conditions),
			group = as.numeric(c(diagnosis()$symptomPresent, rep(2, nrow(diagnosis()))))
		)
		visNetwork(distinct(nodes), select(diagnosis(), from = conditionId, to = symptomId), arrows = "from") %>%
			visBoiler(frozen = FALSE, ord = FALSE) %>% 
			visGroups(groupname = "1", color = "#39CCCC") %>%
			visGroups(groupname = "0", color = "grey") %>%
			visGroups(groupname = "2", shape = "icon", icon = list(code = "f21e", color = "#DD4B39"))
	})
	#########################
	# Edit Database
	#########################
	# Probably want to ditch this as the database grows, but I dunno how much data transparency is wanted
	output$dbSymptom   <- DT::renderDataTable({ react_val$dbTrigger
		filter(attr(STATES(), "data"), type == "symptom") %>% select(-type)
	}, rownames = FALSE)
	output$dbCondition <- DT::renderDataTable({ react_val$dbTrigger
		filter(attr(STATES(), "data"), type == "condition") %>% select(-type) 
	}, rownames = FALSE)
	
	# 1. Adding
	output$addEntity <- renderUI({
		textInput("addEntity", paste("Add", single(input$editType)))
	})
	
	observeEvent(input$addEntity, {
		validate(need(nchar(input$addEntity) > 1, "Must be a real entity name"))
		
		output$addButton <- renderUI( actionButton("addButton", "Add", icon("plus")) )
		
		output$addMenu <- renderUI({ 
			selectizeInput("addLinks", multiple = TRUE, choices = STATES()[[ otherType() ]],
				label = paste("Associated", otherType()))
		})

		lev <- as.numeric(adist(input$addEntity, names( STATES()[[ input$editType ]] ), ignore.case = TRUE))

		if (any(lev == 0)) {
			sendSweetAlert(session, "Error", paste(input$addEntity, "already exists!"), type = "error")
			updateSearchInput(session, "addEntity", value = "")
		} else if (any(lev < 2)) {
			confirmSweetAlert(session, "addConfirm", "Warning", type = "warning", 
				btn_labels = c("Cancel", "Continue anyways"),
				paste0("This new ", single(input$editType), " is similar to existing ", input$editType), ": ", 
					paste(sQuote(names( STATES()[[ otherType() ]] )[lev == 1]), collapse = ", "))
		}
	})
	observeEvent(input$addConfirm, {
		if (!input$addConfirm) {
			updateSearchInput(session, "addEntity", value = "")
		}
	})
	observeEvent(input$addButton, {
		validate(
			need(is.null(input$addConfirm) || input$addConfirm, "If received confirm-prompt, must have accepted it"),
			need(nchar(input$addEntity) > 1, "Must be a real entity name")
		)
		rs <- tryCatch(
			dbWriteTable(db, "states", append = TRUE,
				data.frame(name = input$addEntity, type = single(input$editType), userId = USER$id)),
			error = function(e) {
				sendSweetAlert(session, "Oops", "Unable to add entries. Contact for help.", type = "error")
		})
		title <- paste(capitalize(single(input$editType)), "added!")
		
		if (!is.null(input$addLinks) && nchar(input$addLinks) > 0) {
			query  <- glue_sql("SELECT id FROM states WHERE name = {nm} AND userId = {user}", 
				nm = input$addEntity, user = USER$id, .con = db)
			add_id <- dbGetQuery(db, query)[["id"]]
			dta    <- expand.grid(symptomId = add_id, conditionId = as.numeric(input$addLinks))

			if (input$editType == "conditions") {
				names(dta) <- rev(names(dta))
			}
			rs_links <- tryCatch(dbWriteTable(db, "diagnosis", dta, append = TRUE),
				error = function(e) {
					sendSweetAlert(session, "Oops", "Unable to add entry links. Contact for help.", type = "error")
			})
			sendSweetAlert(session, title, type = "success", html = TRUE, div(
				tags$style(".swal-modal {width: 50%;}"),
				renderVisNetwork({
					nodes <- data.frame(
						id    = c( add_id, dta[, 2]),
						label = c( input$addEntity, quickNames(STATES()[[ otherType() ]], dta[, 2], `%in%`) ),
						group = c( input$editType, rep(otherType(), nrow(dta)) )
					)
					visNetwork(nodes, setNames(dta, c("from", "to"))) %>% 
						visBoiler() %>%
						visGroups(groupname = "symptoms", color = "#39CCCC") %>%
						visGroups(groupname = "conditions", shape = "icon",
							icon = list(code = "f21e", color = "#DD4B39")) %>%
						addFontAwesome()
				})
			))
		} else {
			sendSweetAlert(session, title, type = "success", text = paste("But not associated with any", otherType()))
		}
		idx <- react_val[["dbChanged"]]$Action == "add" & react_val[["dbChanged"]]$Entity == input$editType
		react_val[["dbChanged"]]$Edits[idx] <- react_val[["dbChanged"]]$Edits[idx] + rs
	})

	
	# 2. Deletion
	output$deleteMenu <- renderUI({
		selectizeInput("deleteEntity", choices = STATES()[[input$editType]], multiple = TRUE,
			label = paste("Select", input$editType))
	})
	observeEvent(input$deleteButton, {
		del_name <- quickNames(STATES()[[input$editType]], as.numeric(input$deleteEntity), `==`)

		if ( length(input$deleteEntity) > 0) {
			confirmSweetAlert(session, "deleteConfirm", type = "warning", html = TRUE,
				paste0("Are you sure you want to delete these ", input$editType, "?"),
				tags$ul(style = "text-align: left", lapply(del_name, tags$li)))
		}
	})
	observeEvent(input$deleteConfirm, {
		validate(need(input$deleteConfirm, "User must confirm deletion"))

		query <- glue_sql("DELETE FROM states WHERE userId = {user} AND id IN ({vals*})",
			user = USER$id, vals = as.numeric(input$deleteEntity), .con = db)
		
		rs <- tryCatch(dbExecute(db, query), error = function(e) {
			sendSweetAlert(session, "Oops", "Unable to delete entries. Contact for help.", type = "error")
		})

		query <- glue_sql("DELETE FROM diagnosis WHERE {`field`} IN ({vals*})",
			field = ifelse(input$editType == "symptoms", "symptomId", "conditionId"),
			vals = input$deleteEntity, .con = db)

		rs_links <- tryCatch(dbExecute(db, query), error = function(e) {
			sendSweetAlert(session, "Oops", "Unable to delete entry links. Contact for help.", type = "error")
		})
		idx <- react_val[["dbChanged"]]$Action == "delete" & react_val[["dbChanged"]]$Entity == input$editType
		react_val[["dbChanged"]]$Edits[idx] <- react_val[["dbChanged"]]$Edits[idx] + rs
	})
	
	# 3.  Edit
	output$changeMenu <- renderUI({
		lab <- paste("Select", single(input$editType))
		selectInput("changeEntity", label = NULL, choices = c(setNames("", lab), STATES()[[input$editType]]))
	})
	
	output$changeAdd <- renderUI({
		dropdownButton(icon = icon("plus"),
			selectizeInput("changeLinks", choices = STATES()[[ otherType() ]], multiple = TRUE,
				label = paste("Add", otherType(), "to this", single(input$editType), "?"))
		)
	})
	
	output$changeVis <- renderVisNetwork({
		react_val$changeVisReset
		validate(need( length(input$changeEntity) > 0 && nchar(input$changeEntity) > 0, "Must be a real entity."))
		query <- glue_sql("SELECT {`mainField`} AS `from`, {`otherField`} AS `to`, name FROM diagnosis", 
			" LEFT JOIN states ON {`otherField`} = id",
			" WHERE {`mainField`} = {val}", 
			mainField = typeFields()[1], otherField = typeFields()[2], val = as.numeric(input$changeEntity), .con = db)
		dta <- dbGetQuery(db, query)
		nodes <- bind_rows(
			data.frame(
				id    = as.numeric(input$changeEntity), 
				label = quickNames(STATES()[[input$editType]], as.numeric(input$changeEntity), `==`),
				group = input$editType),
			data.frame(
				id    = dta$to,
				label = dta$name,
				group = otherType())
		)
		visNetwork(nodes, dta) %>% 
			# visOptions(nodesIdSelection = list(enabled = TRUE, style = "width: 400px; display: none"), 
			# 	manipulation = TRUE) %>% 
			visEvents(click = paste0(
				"function(nodes){ Shiny.onInputChange('", session$ns('current_node'),  "',nodes.nodes[0]);}")) %>%
			visBoiler() %>%
			visGroups(groupname = "symptoms", shape = "icon", icon = list(code = "f0f1", color = "#39CCCC")) %>%
			visGroups(groupname = "conditions", shape = "icon",
					  icon = list(code = "f21e", color = "#DD4B39")) 
	})
	# To 'change' an entity (ie. add or sever edges) there is some promising built in functionality via visOptions for 
	# selecting nodes and adding edges but there's only so much visNetwork lets me customize the HTML. 
	# Will return to this later to take advantage of visOptions(), but for now will do my own janky node/edge editing
	observeEvent(input$changeLinks, {
		if (is.null(input$changeLinks)) { react_val$changeVisReset <- react_val$changeVisReset + 1}
		react_val$dbAddLog <- append(react_val$dbAddLog, as.numeric(input$changeLinks))
		
		nodes <- data.frame(
			id     = as.numeric(input$changeLinks),
			label  = quickNames(STATES()[[ otherType() ]], as.numeric(input$changeLinks), `%in%`),
			group  = otherType())
		
		visNetworkProxy("changeVis", session) %>% 
			visUpdateNodes(nodes) %>% 
			visUpdateEdges(data.frame(from = as.numeric(input$changeEntity), to = nodes$id))
	})
	
	observeEvent(input$current_node, {
		if (input$current_node != as.numeric(input$changeEntity)) {
			react_val$dbDelLog <- append(react_val$dbDelLog, input$current_node)
		
			visNetworkProxy("changeVis", session) %>%
				visRemoveNodes(input$current_node)
		}
	})
	
	observeEvent(input$changeButton, {
		changes <- lapply(list(add = react_val$dbAddLog, del = react_val$dbDelLog), function(x) {
			quickNames(STATES() [[ otherType() ]], as.numeric(x), `%in%`)
		})
		change_name <- quickNames(STATES()[[input$editType]], as.numeric(input$changeEntity), `==`)

		confirmSweetAlert(session, "changeConfirm", 
			title = paste("Verify the changes you want to make to", sQuote(change_name)), 
			text  = div(style = "text-align: left", 
				paste(otherType(), "added:"), tags$ul(lapply(changes$add, tags$li)),
				paste(otherType(), "removed:"), tags$ul(lapply(changes$del, tags$li))
			)
		)
	})
	
	observeEvent(input$changeConfirm, {
		if (input$changeConfirm) {
			if (length(react_val$dbDelLog) > 0) {
				query <- glue_sql(
					"DELETE FROM diagnosis WHERE {`mainField`} = {mainVal} AND {`otherField`} IN ({otherVal*})",
					mainField = typeFields()[1], mainVal = as.numeric(input$changeEntity), otherField = typeFields()[2],
					otherVal = react_val$dbDelLog, .con = db)
				rs <- dbExecute(db, query)
			}
			if (length(react_val$dbAddLog) > 0) {
				dta <- data.frame(as.numeric(input$changeEntity), react_val$dbAddLog)
				names(dta) <- typeFields
				dbWriteTable(db, "diagnosis", dta, append = TRUE)
			}
			idx <- react_val[["dbChanged"]]$Action == "change" & react_val[["dbChanged"]]$Entity == input$editType
			react_val[["dbChanged"]]$Edits[idx] <- react_val[["dbChanged"]]$Edits[idx] + 1
		}
		react_val$dbAddLog <- react_val$dbDelLog <- NULL
		react_val$changeVisReset <- react_val$changeVisReset + 1
	})
	
	observeEvent(input$changeCancel, {
		react_val$dbAddLog <- react_val$dbDelog <- NULL
		react_val$changeVisReset <- react_val$changeVisRest  + 1
	})

	#########################
	# Diagnostics
	#########################
	output$userPanel <- renderUI({
		sidebarUserPanel(span("Logged in as: ", USER$name),
			subtitle = a(icon("sign-out"), "Logout", href = Sys.getenv("DIAGNOSTIC_AUTH0_LOGOUT")))
	})
	
	output$dbEdit <- renderMenu({
		msgs <- apply( filter(react_val$dbChanged, Edits > 0), 1, function(x) {
			notificationItem( paste(x["Edits"], x["Entity"], "have been", gsub("e?$", "ed", x["Action"])) )
		})
		dropdownMenu(type = "notifications", .list = msgs, headerText = "Database changes this session", 
			badgeStatus = "warning")
	})
	output$dbProgress <- renderMenu({
		total_s <- length(STATES()[["symptoms"]])
		total_c <- length(STATES()[["conditions"]])
		bad_s <- dbGetQuery(db, glue_sql("SELECT COUNT(*) AS c FROM states",
			" LEFT JOIN diagnosis ON id = symptomId",
			" WHERE userId = {user} AND symptomId IS NULL", user = USER$id, .con = db))
		bad_c <- dbGetQuery(db, glue_sql("SELECT COUNT(*) AS c FROM states",
			" LEFT JOIN diagnosis ON id = conditionId",
			" WHERE userId = {user} AND conditionId IS NULL", user = USER$id, .con = db))
		msgs <- list(
			taskItem("Register at least 15 conditions",        round(100 * total_s / 15), color = "red"),
			taskItem("Register at least 30 symptoms",          round(100 * total_c / 30), color = "aqua"),
			taskItem("All conditions have a symptom",          100 - round(100 * bad_c$c / total_c), color = "yellow"),
			taskItem("All symptoms are linked to a condition", 100 - round(100 * bad_s$c / total_s), color = "blue")
		)
		dropdownMenu(type = "tasks", .list = msgs)
	})
	
	output$n_symptom <- renderValueBox({
		valueBox( length(STATES()[["symptoms"]]), "registered symptoms", icon("stethoscope"), color = "teal")
	})
	output$n_condition <- renderValueBox({
		valueBox( length(STATES()[["conditions"]]), "registered conditions", icon("heartbeat"), color = "red")
	})
	
	output$inn <- renderPrint({
		for (nm in grep("^db[A-Z].*$", names(input), value = TRUE, invert = TRUE)) {
			cat(paste(nm, ifelse(is.null(input[[nm]]), "NULL", sQuote(input[[nm]])), sep = ":\t"), sep = "\n")
		}
	})
	observeEvent(input$help, {
		sendSweetAlert(session, html = TRUE, type = NULL,
			title = "Precision and Recall are joint measures of relevance", div(
			tags$style(".swal-modal {width: 80%;}"),
			tags$ul(style = "text-align: left",
				tags$li(strong("Precision"), "is how many of the condition's symptoms match symptoms you provided."),
				tags$li(strong("Recall"), "is how many of the symptoms you provided match the condition's symptoms")
			),
			p("If a condition has 100% precision but only 50% recall this means that",
				"the condition does not have any symptoms except those that you asked for, but also",
				"the condition is missing half the symptoms you asked for.", style = "text-align: left"),
			hr(), h4("Eg. My symptoms: 'A', 'B', 'C'"),
			column(6, renderVisNetwork({
				visNetwork(data.frame(id = seq(3), label = c("X", "B", "C")), expand.grid(from = 1, to = 2:3), 
					main = "Condition X with 100% precision and 67% recall", 
					submain = "X is missing our symptom 'A'") %>% 
				visBoiler()
			})),
			column(6, renderVisNetwork({
				visNetwork(data.frame(id = seq(5), label = LETTERS[c(1:4, 24)]), expand.grid(from = 5, to = 1:4),
					main = "Condition X with 75% precision and 100% recall", 
					submain = "X has a symptom 'D' which we don't have") %>% 
				visBoiler()
			}))
		))
	})
	
	output$queryList <- renderPrint( cat(react_val$queries, sep = "\n\n") )
}