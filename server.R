library(shiny)
library(shinyWidgets)
library(DBI)
library(visNetwork)
library(dplyr)
# TODO: Disconnect database better...consider switching to `pool`?
# TODO: Play better with Auth0 authentication
# TODO: Dynamic focus. 
# TODO: Consider switching network visualization to `igraph`. Don't like how vis.js looks right now
options(stringsAsFactors = FALSE)
cred <- list(
	drv      = RMariaDB::MariaDB(), 
	user     = Sys.getenv("diagUser"), 
	password = Sys.getenv("diagPwd"), 
	db       = Sys.getenv("diagDb"))

single     <- function(x) gsub("(?<!s)s$", "", x, perl = TRUE)
quickNames <- function(x, val, op) names(sort(x[op(x, val)]))
visBoiler  <- function(x, frozen = TRUE, ord = TRUE) {
	hfunc <- if (ord) visHierarchicalLayout else identity
	ffunc <- if (frozen) function(x) visInteraction(x, dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) 
	else identity
	
	x %>% hfunc() %>% ffunc() %>% 
		visEdges(color = "black") %>% 
		visNodes(font = list(size = 20))
}

server <- function(input, output, session) {
	db <- do.call("dbConnect", cred)
	session$onSessionEnded( function() { dbDisconnect(db); print("Database connection terminated.") } )
	
	# Some observers read from database, others write to database. I want the read-observers to react to  
	# the write-observers. The most elegant way I can think of doing this is to make the connection object
	# itself a reactive value. Works well, saves me boilerplate code, but final issue is how to disconnect?
	# onSessionEnded wouldn't work because the reactive connection object must be called in a reactive context. hmm...
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

	ENTITIES <- eventReactive(react_val$dbTrigger, {
		symptom   <- dbGetQuery(db, "SELECT id, name FROM symptoms")
		condition <- dbGetQuery(db, "SELECT id, name FROM conditions")

		setNames(nm = ENTITY_FIELDS, list(setNames(symptom$id, symptom$name), setNames(condition$id, condition$name)))
	})
	otherTable <- reactive( setdiff(ENTITY_FIELDS, input$editTable) )
	
	#########################
	# Dashboard
	#########################
	# 1. Symptom-condition look-up
	output$symptomSelect <- renderUI({
		pickerInput("symptoms", NULL, ENTITIES()[["symptoms"]], multiple = TRUE, 
			options = list("live-search" = TRUE, "actions-box" = TRUE, "none-selected-text" = "Enter symptoms here"))
	})
	
	diagnosis <- eventReactive(input$symptoms, {
		# TODO: Rewrite this disaster of a query!
		query <- glue::glue_sql(paste(
			"SELECT sc.*, c.name AS conditions, s.name AS symptom, s.id IN ({vals*}) AS symptomPresent", 
			"FROM symptomCondition sc", 
			"LEFT JOIN conditions c ON sc.conditionId = c.id", 
			"LEFT JOIN symptoms s ON sc.symptomId = s.id", 
			"WHERE conditionId IN (", 
				"SELECT conditionId FROM symptomCondition", 
				"LEFT JOIN symptoms ON symptomId = id", 
				"WHERE id IN ({vals*}))"), vals = as.numeric(input$symptoms), .con = db)
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
		nodes <- bind_rows(
			diagnosis() %>% 
				transmute(id = paste0(symptomId, "S"), group = as.character(symptomPresent), label = symptom) %>% 
				distinct(),
			diagnosis() %>% 
				transmute(id = paste0(conditionId, "C"), group = "2", label = conditions) %>% 
				distinct()
		)
		edges <- transmute(diagnosis(), from = paste0(conditionId, "C"), to = paste0(symptomId, "S"))
		visNetwork(mutate(nodes, level = group != "2"), edges, arrows = "from") %>%
			visBoiler(frozen = FALSE, ord = FALSE) %>% 
			visGroups(groupname = "1", color = "#39CCCC") %>%
			visGroups(groupname = "0", color = "grey") %>%
			visGroups(groupname = "2", shape = "icon", icon = list(code = "f21e", color = "#DD4B39"))
	})
	#########################
	# Edit Database
	#########################
	# Probably want to ditch this as the database grows, but I dunno how much data transparency is wanted
	output$dbSymptom   <- DT::renderDataTable({ react_val$dbTrigger; dbReadTable(db, "symptoms") }, rownames = FALSE)
	output$dbCondition <- DT::renderDataTable({ react_val$dbTrigger; dbReadTable(db, "conditions") }, rownames = FALSE)
	
	# 1. Adding
	output$addEntity <- renderUI({
		searchInput("addEntity", btnSearch = icon("search"), paste("Add", single(input$editTable)))
	})
	
	observeEvent(input$addEntity, {
		validate(need(nchar(input$addEntity) > 1, "Must be a real entity name"))
		
		output$addButton <- renderUI( actionButton("addButton", "Add", icon("plus")) )
		
		output$addMenu <- renderUI({ 
			selectizeInput("addLinks", multiple = TRUE, choices = ENTITIES()[[ otherTable() ]],
				label = paste("Associated", otherTable()))
		})

		lev <- as.numeric(adist(input$addEntity, names( ENTITIES()[[ input$editTable ]] ), ignore.case = TRUE))

		if (any(lev == 0)) {
			sendSweetAlert(session, "Error", paste(input$addEntity, "already exists!"), type = "error")
			updateSearchInput(session, "addEntity", value = "")
		} else if (any(lev < 2)) {
			confirmSweetAlert(session, "addConfirm", "Warning", type = "warning", 
				btn_labels = c("Cancel", "Continue anyways"),
				paste0("This new ", single(input$editTable), " is similar to existing ", input$editTable), ": ", 
					paste(sQuote(names( ENTITIES()[[ otherTable() ]] )[lev == 1]), collapse = ", "))
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
			dbWriteTable(db, input$editTable, data.frame(name = input$addEntity), append = TRUE),
			error = function(e) {
				sendSweetAlert(session, "Oops", "Unable to add entries. Contact for help.", type = "error")
		})
		title <- paste(capitalize(single(input$editTable)), "added!")
		
		if (!is.null(input$addLinks) && nchar(input$addLinks) > 0) {
			query  <- glue::glue_sql("SELECT id FROM {`tab`} WHERE name = {val}", tab = input$editTable, 
				val = input$addEntity, .con = db)
			add_id <- dbGetQuery(db, query)[["id"]]
			dta    <- expand.grid(symptomId = add_id, conditionId = as.numeric(input$addLinks))
			prefix <- c("S", "C")
			
			if (input$editTable == "conditions") {
				names(dta) <- rev(names(dta))
				prefix     <- rev(prefix)
			}
			rs_links <- tryCatch(dbWriteTable(db, "symptomCondition", dta, append = TRUE),
				error = function(e) {
					sendSweetAlert(session, "Oops", "Unable to add entry links. Contact for help.", type = "error")
			})
			sendSweetAlert(session, title, type = "success", html = TRUE, div(
				tags$style(".swal-modal {width: 50%;}"),
				renderVisNetwork({
					nodes <- data.frame(
						id    = c( paste0(add_id, prefix[1]), paste0(sort(dta[, 2]), prefix[2]) ),
						label = c( input$addEntity, quickNames(ENTITIES()[[ otherTable() ]], dta[, 2], `%in%`) ),
						group = c( input$editTable, rep(otherTable(), nrow(dta)) )
					)
					edges <- mutate(dta, symptomId = paste0(symptomId, "S"), conditionId = paste0(conditionId, "C"))
					visNetwork(nodes, setNames(edges, c("from", "to"))) %>% 
						visBoiler() %>%
						visGroups(groupname = "symptoms", color = "#39CCCC") %>%
						visGroups(groupname = "conditions", shape = "icon",
							icon = list(code = "f21e", color = "#DD4B39")) %>%
						addFontAwesome()
				})
			))
		} else {
			sendSweetAlert(session, title, type = "success", text = paste("But not associated with any", otherTable()))
		}
		idx <- react_val[["dbChanged"]]$Action == "add" & react_val[["dbChanged"]]$Entity == input$editTable
		react_val[["dbChanged"]]$Edits[idx] <- react_val[["dbChanged"]]$Edits[idx] + rs
	})

	
	# 2. Deletion
	output$deleteMenu <- renderUI({
		selectizeInput("deleteEntity", choices = ENTITIES()[[input$editTable]], multiple = TRUE,
			label = paste("Select", input$editTable))
	})
	observeEvent(input$deleteButton, {
		del_name <- quickNames(ENTITIES()[[input$editTable]], as.numeric(input$deleteEntity), `==`)

		if ( length(input$deleteEntity) > 0) {
			confirmSweetAlert(session, "deleteConfirm", type = "warning", html = TRUE,
				paste0("Are you sure you want to delete these ", input$editTable, "?"),
				tags$ul(style = "text-align: left", lapply(del_name, tags$li)))
		}
	})
	observeEvent(input$deleteConfirm, {
		validate(need(input$deleteConfirm, "User must confirm deletion"))

		query <- glue::glue_sql("DELETE FROM {`tab`} WHERE id IN ({vals*})",
			tab = input$editTable, vals = as.numeric(input$deleteEntity), .con = db)
		
		rs <- tryCatch(dbExecute(db, query), error = function(e) {
			sendSweetAlert(session, "Oops", "Unable to delete entries. Contact for help.", type = "error")
		})

		query <- glue::glue_sql("DELETE FROM symptomCondition WHERE {`field`} IN ({vals*})",
			field = ifelse(input$editTable == "symptoms", "symptomId", "conditionId"),
			vals = input$deleteEntity, .con = db)

		rs_links <- tryCatch(dbExecute(db, query), error = function(e) {
			sendSweetAlert(session, "Oops", "Unable to delete entry links. Contact for help.", type = "error")
		})
		idx <- react_val[["dbChanged"]]$Action == "delete" & react_val[["dbChanged"]]$Entity == input$editTable
		react_val[["dbChanged"]]$Edits[idx] <- react_val[["dbChanged"]]$Edits[idx] + rs
	})
	
	# 3.  Edit
	output$changeMenu <- renderUI({
		selectizeInput("changeEntity", choices = ENTITIES()[[input$editTable]], 
			label = paste("Select", single(input$editTable)))
	})
	
	output$changeAdd <- renderUI({
		dropdownButton(icon = icon("plus"),
			selectizeInput("changeLinks", choices = ENTITIES()[[ otherTable() ]], multiple = TRUE,
				label = paste("Which", otherTable(), "do you want to add to this", single(input$editTable), "?"))
		)
	})
	
	output$changeVis <- renderVisNetwork({
		react_val$changeVisReset
		
		validate(need( !is.null(input$changeEntity), "Must be a real entity."))
		fields <- c("symptomId", "conditionId")
		if (input$editTable == "conditions") {
			fields <- rev(fields)
		}
		
		query <- glue::glue_sql(paste(
			"SELECT symptomId, conditionId, name FROM symptomCondition", 
			"LEFT JOIN {`tab`} ON {`otherField`} = id",
			"WHERE {`mainField`} = {val}"
		), tab = otherTable(), mainField = fields[1], otherField = fields[2], val = input$changeEntity, .con = db)
		dta <- dbGetQuery(db, query)
		nodes <- bind_rows(
			data.frame(
				id    = paste(input$changeEntity, "A"), 
				label = quickNames(ENTITIES()[[input$editTable]], as.numeric(input$changeEntity), `==`),
				group = input$editTable),
			data.frame(
				id    = paste(dta[, fields[2]], "B"),
				label = dta$name,
				group = otherTable())
		)
		dta$from <- paste(dta[, fields[1]], "A")
		dta$to   <- paste(dta[, fields[2]], "B")
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
		react_val$dbAddLog <- append(react_val$dbAddLog, input$changeLinks)
		
		nodes <- data.frame(
			id     = paste(input$changeLinks, "C"),
			label  = quickNames(ENTITIES()[[ otherTable() ]], input$changeLinks, `%in%`),
			group  = otherTable())
		
		visNetworkProxy("changeVis", session) %>% 
			visUpdateNodes(nodes) %>% 
			visUpdateEdges(data.frame(from = paste(input$changeEntity, "A"), to = nodes$id))
	})
	
	observeEvent(input$current_node, {
		if (input$current_node != paste(input$changeEntity, "A")) {
			react_val$dbDelLog <- append(react_val$dbDelLog, gsub("([0-9]+)\\s?([A-Z])","\\1", input$current_node))
		
			visNetworkProxy("changeVis", session) %>%
				visRemoveNodes(input$current_node)
		}
	})
	
	observeEvent(input$changeButton, {
		# My database schema SUCKS. WHy did I separate condition and symptoms into separate tables???
		# I'm going to have to refactor this whole damn thing later. 
		changes <- lapply(list(add = react_val$dbAddLog, del = react_val$dbDelLog), function(x) {
			quickNames(ENTITIES() [[ otherTable() ]], as.numeric(x), `%in%`)
		})
		change_name <- quickNames(ENTITIES()[[input$editTable]], as.numeric(input$changeEntity), `==`)

		confirmSweetAlert(session, "changeConfirm", 
			title = paste("Verify the changes you want to make to", sQuote(change_name)), 
			text  = div(style = "text-align: left", 
				paste(otherTable(), "added:"), tags$ul(lapply(changes$add, tags$li)),
				paste(otherTable(), "removed:"), tags$ul(lapply(changes$del, tags$li))
			)
		)
	})
	
	observeEvent(input$changeConfirm, {
		if (input$changeConfirm) {
			fields <- c("symptomId", "conditionId")
			if (input$editTable == "conditions") {
				fields <- rev(fields)
			}
			if (length(react_val$dbDelLog) > 0) {
				query <- glue::glue_sql(
					"DELETE FROM symptomCondition WHERE {`mainField`} = {mainVal} AND {`otherField`} IN ({otherVal*})",
					mainField = fields[1], mainVal = as.numeric(input$changeEntity), otherField = fields[2], 
					otherVal = as.numeric(react_val$dbDelLog), .con = db)
				rs <- dbExecute(db, query)
			}
			if (length(react_val$dbAddLog) > 0) {
				dta <- data.frame(as.numeric(input$changeEntity), as.numeric(react_val$dbAddLog))
				names(dta) <- fields
				dbWriteTable(db, "symptomCondition", dta, append = TRUE)
			}
			idx <- react_val[["dbChanged"]]$Action == "change" & react_val[["dbChanged"]]$Entity == input$editTable
			react_val[["dbChanged"]]$Edits[idx] <- react_val[["dbChanged"]]$Edits[idx] + 1
		}
		react_val$dbAddLog <- react_val$dbDelLog <- NULL
		react_val$changeVisReset <- react_val$changeVisReset + 1
	})

	#########################
	# Diagnostics
	#########################
	output$dbEdit <- renderMenu({
		msgs <- apply( filter(react_val$dbChanged, Edits > 0), 1, function(x) {
			notificationItem( paste(x["Edits"], x["Entity"], "have been", gsub("e?$", "ed", x["Action"])) )
		})
		dropdownMenu(type = "notifications", .list = msgs, headerText = "Database changes this session", 
			badgeStatus = "warning")
	})
	output$dbProgress <- renderMenu({
		total_s <- length(ENTITIES()[["symptoms"]])
		total_c <- length(ENTITIES()[["conditions"]])
		bad_s <- dbGetQuery(db, paste("SELECT COUNT(*) AS c FROM symptoms",
												"LEFT JOIN symptomCondition ON id = symptomId",
												"WHERE symptomId IS NULL"))
		bad_c <- dbGetQuery(db, paste("SELECT COUNT(*) AS c FROM conditions",
												"LEFT JOIN symptomCondition ON id = conditionId",
												"WHERE conditionId IS NULL"))
		msgs <- list(
			taskItem("Register at least 15 conditions",        round(100 * total_s / 15), color = "red"),
			taskItem("Register at least 30 symptoms",          round(100 * total_c / 30), color = "aqua"),
			taskItem("All conditions have a symptom",          100 - round(100 * bad_c$c / total_c), color = "yellow"),
			taskItem("All symptoms are linked to a condition", 100 - round(100 * bad_s$c / total_s), color = "blue")
		)
		dropdownMenu(type = "tasks", .list = msgs)
	})
	
	output$n_symptom <- renderValueBox({
		valueBox( length(ENTITIES()[["symptoms"]]), "registered symptoms", icon("stethoscope"), color = "teal")
	})
	output$n_condition <- renderValueBox({
		valueBox( length(ENTITIES()[["conditions"]]), "registered conditions", icon("heartbeat"), color = "red")
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