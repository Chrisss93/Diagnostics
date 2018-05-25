library(shiny)
library(shinydashboard)
library(shinyWidgets)

sidebar <- dashboardSidebar(
	uiOutput("userPanel"),
	sidebarMenu(
		menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), badgeLabel = "new", badgeColor = "green"),
		menuItem("Edit database", tabName = "database", icon = icon("database"), badgeLabel = "new", badgeColor = "green"),
		menuItem("Analytics", tabName = "analytics", icon = icon("chart-bar"), badgeLabel = "Soon", badgeColor = "purple"),
		menuItem("Diagnostics", tabName = "diagnostics", icon = icon("wrench"), badgeLabel = "dev", badgeColor = "red")
	),
	div(div(style="position : absolute; bottom: 5px", 
			valueBoxOutput("n_symptom", 12), 
			valueBoxOutput("n_condition", 12)
	))
)
sbs_style <- "display: inline-block;vertical-align:top;"

body <- dashboardBody(
	tabItems(
		tabItem(tabName = "dashboard",
			fluidRow(
				box(uiOutput("symptomSelect"), title = "Symptoms", status = "primary"),
				box(title = "Possible conditions", status = "primary",
					conditionalPanel("input.symptoms != null",
						span(style = sbs_style, tableOutput("conditionSelect")),
						span(style = sbs_style, circleButton("help", icon("question"), "info", size = "xs"))
					)
			)),
			fluidRow(
				box(visNetwork::visNetworkOutput("net"), title = "Symptom network", status = "success", width = 12, 
					solidHeader = TRUE, collapsible = TRUE))
		),
		tabItem(tabName = "database",
			fluidRow(
				box(DT::dataTableOutput("dbSymptom"), title = "Symptom table"),
				box(DT::dataTableOutput("dbCondition"), title = "Condition table")),
			fluidRow(
				box(width = 12, status = "danger", solidHeader = TRUE, collapsible = TRUE, title = "Edit database",
					column(width = 4,
						prettyRadioButtons("editType", choiceValues = ENTITY_FIELDS, animation = "pulse",
							choiceNames = capitalize(ENTITY_FIELDS), label = "Entity", status = "info"),
						prettyRadioButtons("editAction", choiceValues = EDIT_FIELDS, animation = "pulse",
							choiceNames = capitalize(EDIT_FIELDS), label = "Action", status = "info")),
					column(width = 8,
						conditionalPanel("input.editAction == 'add'",
							uiOutput("addEntity"),
							uiOutput("addMenu"),
							uiOutput("addButton")),
						conditionalPanel("input.editAction == 'delete'",
							uiOutput("deleteMenu"),
							actionButton("deleteButton", "Delete", icon("trash"))),
						conditionalPanel("input.editAction == 'change'", 
							uiOutput("changeMenu"),
							uiOutput("changeAdd"),
							visNetwork::visNetworkOutput("changeVis"),
							div(actionButton("changeCancel", "Cancel", icon("times")), 
								actionButton("changeButton", "Save", icon("save")))
							)
					)
				)
			)
		),
		tabItem(tabName = "analytics", fluidPage(p("Not done yet..."))),
		tabItem(tabName = "diagnostics", 
			column(width = 6,
				box(verbatimTextOutput("inn"), title = "Inputs", status = "warning", collapsible = TRUE, width = 12)),
			column(width = 6, 
				box(verbatimTextOutput("queryList"), title = "SQL calls", status = "warning", width = 12)))
	)
)

ui <- dashboardPage(
	dashboardHeader(title = "Symptom - Condition tool",
		dropdownMenuOutput("dbProgress"),
		dropdownMenuOutput("dbEdit"),
		dropdownMenu(type = "messages", messageItem("dev", "This app is still experimental", icon("user")), 
			badgeStatus = "danger")),
	sidebar, 
	body, 
	skin = "purple"
)