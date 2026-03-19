# ============================================================
# Museum Catalog - R Shiny Data Entry App
# ============================================================
# Required packages:
#   install.packages(c("shiny", "DBI", "RSQLite", "DT",
#                      "bslib", "shinyFeedback", "pool"))
# ============================================================

library(shiny)
library(DBI)
library(RSQLite)
library(DT)
library(bslib)
library(shinyFeedback)
library(pool)
options(shiny.launch.browser = TRUE)
# Null-coalescing helper (empty string or NA -> b)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a) && trimws(a) != "") a else b

# ── Database connection ───────────────────────────────────────
# Using SQLite for portability. Swap driver + args for PostgreSQL:
#   pool::dbPool(RPostgres::Postgres(), dbname=..., host=..., ...)
db <- dbConnect(RSQLite::SQLite(), "museum.db")
onStop(function() dbDisconnect(db))
# ── Create tables if they don't exist ────────────────────────
dbExecute(db, "PRAGMA foreign_keys = ON")

dbExecute(db, "
CREATE TABLE IF NOT EXISTS person (
  person_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  first_name  TEXT NOT NULL,
  last_name   TEXT NOT NULL,
  birth_date  TEXT,
  death_date  TEXT,
  nationality TEXT,
  biography   TEXT,
  created_at  TEXT DEFAULT (datetime('now'))
)")

dbExecute(db, "
CREATE TABLE IF NOT EXISTS collection (
  collection_id INTEGER PRIMARY KEY AUTOINCREMENT,
  name          TEXT NOT NULL,
  description   TEXT,
  start_date    TEXT,
  end_date      TEXT,
  curator_id    INTEGER REFERENCES person(person_id),
  created_at    TEXT DEFAULT (datetime('now'))
)")

dbExecute(db, "
CREATE TABLE IF NOT EXISTS item (
  item_id          INTEGER PRIMARY KEY AUTOINCREMENT,
  collection_id    INTEGER NOT NULL REFERENCES collection(collection_id),
  accession_number TEXT UNIQUE NOT NULL,
  title            TEXT NOT NULL,
  description      TEXT,
  category         TEXT,
  date_created     TEXT,
  date_acquired    TEXT,
  provenance       TEXT,
  condition        TEXT,
  location         TEXT,
  is_on_display    INTEGER DEFAULT 0,
  created_at       TEXT DEFAULT (datetime('now'))
)")

dbExecute(db, "
CREATE TABLE IF NOT EXISTS item_person (
  item_person_id INTEGER PRIMARY KEY AUTOINCREMENT,
  item_id        INTEGER NOT NULL REFERENCES item(item_id),
  person_id      INTEGER NOT NULL REFERENCES person(person_id),
  role           TEXT NOT NULL,
  notes          TEXT
)")

dbExecute(db, "
CREATE TABLE IF NOT EXISTS image (
  image_id    INTEGER PRIMARY KEY AUTOINCREMENT,
  item_id     INTEGER NOT NULL REFERENCES item(item_id),
  filename    TEXT NOT NULL,
  caption     TEXT,
  is_primary  INTEGER DEFAULT 0,
  uploaded_at TEXT DEFAULT (datetime('now'))
)")

# ── Helper functions ──────────────────────────────────────────
get_persons <- function() {
  dbGetQuery(db, "SELECT person_id, first_name || ' ' || last_name AS name FROM person ORDER BY last_name")
}

get_collections <- function() {
  dbGetQuery(db, "SELECT collection_id, name FROM collection ORDER BY name")
}
options(shiny.useragg = FALSE)
# ── UI ────────────────────────────────────────────────────────
ui <- page_navbar(
  title = tags$span(
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/2936/2936886.png",
             height = "28px", style = "margin-right:8px; vertical-align:middle;"),
    "Museum Catalog"
  ),
  header = useShinyFeedback(),
  theme = bs_theme(
    bootswatch  = "flatly",
    primary     = "#2C4A6E",
    secondary   = "#8B6F5E",
  ),
  # ── PERSONS tab ─────────────────────────────────────────────
  nav_panel(
    title = tagList(icon("user"), " People"),
    layout_sidebar(
      sidebar = sidebar(
        style = "overflow-y: auto; max-height: calc(100vh - 80px);",
        title = "Add / Edit Person",
        textInput("p_first",   "First Name *"),
        textInput("p_last",    "Last Name *"),
        textInput("p_dob",     "Birth Date", placeholder = "YYYY-MM-DD"),
        textInput("p_dod",     "Death Date", placeholder = "YYYY-MM-DD"),
        textInput("p_nation",  "Nationality"),
        textAreaInput("p_bio", "Biography", rows = 3),
        actionButton("p_save", "Save Person",
                     class = "btn-primary w-100", icon = icon("save")),
        hr(),
        actionButton("p_clear", "Clear Form", class = "btn-outline-secondary w-100")
      ),
      card(
        card_header("People Records"),
        DTOutput("person_table")
      )
    )
  ),

  # ── COLLECTIONS tab ──────────────────────────────────────────
  nav_panel(
    title = tagList(icon("folder-open"), " Collections"),
    layout_sidebar(
      sidebar = sidebar(
        style = "overflow-y: auto; max-height: calc(100vh - 80px);",
        title = "Add / Edit Collection",
        textInput("c_name",        "Collection Name *"),
        textAreaInput("c_desc",    "Description", rows = 3),
        textInput("c_start",       "Start Date", placeholder = "YYYY-MM-DD"),
        textInput("c_end",         "End Date",   placeholder = "YYYY-MM-DD"),
        uiOutput("c_curator_ui"),
        actionButton("c_save", "Save Collection",
                     class = "btn-primary w-100", icon = icon("save")),
        hr(),
        actionButton("c_clear", "Clear Form", class = "btn-outline-secondary w-100")
      ),
      card(
        card_header("Collection Records"),
        DTOutput("collection_table")
      )
    )
  ),

  # ── ITEMS tab ────────────────────────────────────────────────
  nav_panel(
    title = tagList(icon("archive"), " Items"),
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        open = TRUE,
        style = "overflow-y: auto; max-height: calc(100vh - 80px);",
        title = uiOutput("i_sidebar_title"),
        uiOutput("i_collection_ui"),
        textInput("i_accession", "Accession Number *"),
        textInput("i_title",     "Title *"),
        textAreaInput("i_desc",  "Description", rows = 2),
        selectInput("i_category", "Category",
                    choices = c("", "Painting", "Sculpture", "Photograph",
                                "Document", "Textile", "Ceramic", "Jewellery",
                                "Furniture", "Natural Specimen", "Other")),
        textInput("i_date_created",  "Date Created",  placeholder = "e.g. c.1850"),
        textInput("i_date_acquired", "Date Acquired", placeholder = "YYYY-MM-DD"),
        textAreaInput("i_provenance","Provenance", rows = 2),
        selectInput("i_condition", "Condition",
                    choices = c("", "Excellent", "Good", "Fair", "Poor", "Fragmentary")),
        textInput("i_location", "Physical Location"),
        checkboxInput("i_display", "Currently on Display", value = FALSE),
        hr(),
        h6("Associate People"),
        uiOutput("i_people_ui"),
        textInput("i_role", "Role(s)", placeholder = "e.g. Artist, Donor"),
        hr(),
        actionButton("i_save", "Save Item",
                     class = "btn-primary w-100", icon = icon("save")),
        actionButton("i_clear", "Clear Form",
                     class = "btn-outline-secondary w-100 mt-2")
      ),
      card(
        card_header("Item Records"),
        DTOutput("item_table")
      )
    )
  ),

  # ── IMAGES tab ───────────────────────────────────────────────
  nav_panel(
    title = tagList(icon("image"), " Images"),
    layout_sidebar(
      sidebar = sidebar(
        style = "overflow-y: auto; max-height: calc(100vh - 80px);",
        title = "Upload Image",
        uiOutput("img_item_ui"),
        fileInput("img_file", "Choose Image File",
                  accept = c("image/png", "image/jpeg", "image/gif", "image/webp")),
        textInput("img_caption", "Caption"),
        checkboxInput("img_primary", "Set as Primary Image", value = FALSE),
        actionButton("img_save", "Save Image Record",
                     class = "btn-primary w-100", icon = icon("upload")),
        hr(),
        p(class = "text-muted small",
          "Note: This records image metadata. Store image files in an",
          code("www/images/"), "subfolder of the app directory.")
      ),
      card(
        card_header("Image Records"),
        DTOutput("image_table")
      )
    )
  ),

  # ── BROWSE tab ───────────────────────────────────────────────
  nav_panel(
    title = tagList(icon("search"), " Browse"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Filter"),
        selectInput("br_collection", "Collection",
                    choices = c("All" = ""), width = "100%"),
        selectInput("br_category", "Category",
                    choices = c("All" = "", "Painting", "Sculpture", "Photograph",
                                "Document", "Textile", "Ceramic", "Jewellery",
                                "Furniture", "Natural Specimen", "Other"),
                    width = "100%"),
        selectInput("br_display", "On Display",
                    choices = c("All" = "", "Yes" = "1", "No" = "0"),
                    width = "100%"),
        textInput("br_search", "Search title / description"),
        actionButton("br_go", "Search", class = "btn-primary w-100", icon = icon("search"))
      ),
      layout_columns(
        col_widths = 12,
        card(
          card_header("Results — click a row to view full report"),
          DTOutput("browse_table")
        ),
        uiOutput("item_report_ui")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Reactive triggers to refresh tables
  refresh_persons     <- reactiveVal(0)
  refresh_collections <- reactiveVal(0)
  refresh_items       <- reactiveVal(0)
  refresh_images      <- reactiveVal(0)

  # Tracks which item is selected for editing (NULL = add mode)
  selected_item_id  <- reactiveVal(NULL)
  item_table_data   <- reactiveVal(data.frame())

  # ── Dynamic UIs fed from DB ──────────────────────────────────
  output$c_curator_ui <- renderUI({
    refresh_persons()
    p <- get_persons()
    choices <- setNames(c("", p$person_id), c("— none —", p$name))
    selectInput("c_curator", "Curator", choices = choices)
  })

  output$i_collection_ui <- renderUI({
    refresh_collections()
    col <- get_collections()
    choices <- setNames(c("", col$collection_id), c("— select —", col$name))
    selectInput("i_collection", "Collection *", choices = choices)
  })

  output$i_people_ui <- renderUI({
    refresh_persons()
    p <- get_persons()
    choices <- setNames(p$person_id, p$name)
    selectizeInput("i_people", "People", choices = choices,
                   multiple = TRUE,
                   options = list(placeholder = "Search people…"))
  })

  output$img_item_ui <- renderUI({
    refresh_items()
    items <- dbGetQuery(db, "SELECT item_id, accession_number || ' – ' || title AS label FROM item ORDER BY title")
    choices <- setNames(c("", items$item_id), c("— select item —", items$label))
    selectInput("img_item", "Item *", choices = choices)
  })

  # ── PERSON save ──────────────────────────────────────────────
  observeEvent(input$p_save, {
    feedbackDanger("p_first", input$p_first == "", "Required")
    feedbackDanger("p_last",  input$p_last  == "", "Required")
    req(input$p_first != "", input$p_last != "")

    na_if_empty <- function(x) if (is.null(x) || length(x) == 0 || trimws(x) == "") NA_character_ else trimws(x)
    dbExecute(db,
      "INSERT INTO person (first_name, last_name, birth_date, death_date, nationality, biography)
       VALUES (?, ?, ?, ?, ?, ?)",
      list(trimws(input$p_first), trimws(input$p_last),
           na_if_empty(input$p_dob), na_if_empty(input$p_dod),
           na_if_empty(input$p_nation), na_if_empty(input$p_bio))
    )
    refresh_persons(refresh_persons() + 1)
    showNotification("Person saved.", type = "message")
  })

  observeEvent(input$p_clear, {
    updateTextInput(session, "p_first",  value = "")
    updateTextInput(session, "p_last",   value = "")
    updateTextInput(session, "p_dob",    value = "")
    updateTextInput(session, "p_dod",    value = "")
    updateTextInput(session, "p_nation", value = "")
    updateTextAreaInput(session, "p_bio", value = "")
  })

  output$person_table <- renderDT({
    refresh_persons()
    dbGetQuery(db, "SELECT person_id AS ID, first_name AS First, last_name AS Last,
                           birth_date AS Born, death_date AS Died,
                           nationality AS Nationality FROM person ORDER BY last_name")
  }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
     selection = "single")

  # ── COLLECTION save ──────────────────────────────────────────
  observeEvent(input$c_save, {
    feedbackDanger("c_name", input$c_name == "", "Required")
    req(input$c_name != "")

    curator <- if (!is.null(input$c_curator) && input$c_curator != "") as.integer(input$c_curator) else NA

    na_if_empty <- function(x) if (is.null(x) || length(x) == 0 || trimws(x) == "") NA_character_ else trimws(x)
    dbExecute(db,
      "INSERT INTO collection (name, description, start_date, end_date, curator_id)
       VALUES (?, ?, ?, ?, ?)",
      list(trimws(input$c_name), na_if_empty(input$c_desc),
           na_if_empty(input$c_start), na_if_empty(input$c_end), curator)
    )
    refresh_collections(refresh_collections() + 1)
    showNotification("Collection saved.", type = "message")
  })

  observeEvent(input$c_clear, {
    updateTextInput(session, "c_name",  value = "")
    updateTextAreaInput(session, "c_desc", value = "")
    updateTextInput(session, "c_start", value = "")
    updateTextInput(session, "c_end",   value = "")
  })

  output$collection_table <- renderDT({
    refresh_collections()
    dbGetQuery(db,
      "SELECT c.collection_id AS ID, c.name AS Name, c.description AS Description,
              c.start_date AS Start, c.end_date AS End,
              p.first_name || ' ' || p.last_name AS Curator
       FROM collection c
       LEFT JOIN person p ON p.person_id = c.curator_id
       ORDER BY c.name")
  }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
     selection = "single")

  # ── ITEM save ────────────────────────────────────────────────
  # Dynamic sidebar title
  output$i_sidebar_title <- renderUI({
    if (is.null(selected_item_id())) "Add Item" else "Edit Item"
  })

  observeEvent(input$i_save, {
    feedbackDanger("i_collection", is.null(input$i_collection) || input$i_collection == "", "Required")
    feedbackDanger("i_accession",  input$i_accession == "", "Required")
    feedbackDanger("i_title",      input$i_title     == "", "Required")
    req(input$i_collection != "", input$i_accession != "", input$i_title != "")

    na_if_empty <- function(x) if (is.null(x) || length(x) == 0 || trimws(x) == "") NA_character_ else trimws(x)

    vals <- list(
      as.integer(input$i_collection),
      trimws(input$i_accession), trimws(input$i_title),
      na_if_empty(input$i_desc),         na_if_empty(input$i_category),
      na_if_empty(input$i_date_created), na_if_empty(input$i_date_acquired),
      na_if_empty(input$i_provenance),   na_if_empty(input$i_condition),
      na_if_empty(input$i_location),     as.integer(isTRUE(input$i_display))
    )

    if (is.null(selected_item_id())) {
      # ── INSERT new item ───────────────────────────────────────
      dbExecute(db,
        "INSERT INTO item (collection_id, accession_number, title, description, category,
                           date_created, date_acquired, provenance, condition, location, is_on_display)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", vals)
      item_id <- dbGetQuery(db, "SELECT last_insert_rowid() AS id")$id
      msg <- "Item added."
    } else {
      # ── UPDATE existing item ──────────────────────────────────
      item_id <- selected_item_id()
      dbExecute(db,
        "UPDATE item SET collection_id=?, accession_number=?, title=?, description=?,
                         category=?, date_created=?, date_acquired=?, provenance=?,
                         condition=?, location=?, is_on_display=?
         WHERE item_id=?", c(vals, list(item_id)))
      # Remove old person links before re-inserting
      dbExecute(db, "DELETE FROM item_person WHERE item_id = ?", list(item_id))
      msg <- "Item updated."
    }

    # Save item_person links
    if (!is.null(input$i_people) && length(input$i_people) > 0) {
      role <- if (trimws(input$i_role) == "") "Unspecified" else trimws(input$i_role)
      for (pid in input$i_people) {
        dbExecute(db,
          "INSERT INTO item_person (item_id, person_id, role) VALUES (?, ?, ?)",
          list(item_id, as.integer(pid), role))
      }
    }

    refresh_items(refresh_items() + 1)
    showNotification(msg, type = "message")
  })

  observeEvent(input$i_clear, {
    selected_item_id(NULL)
    updateSelectInput(session,   "i_collection",    selected = "")
    updateTextInput(session,     "i_accession",     value = "")
    updateTextInput(session,     "i_title",         value = "")
    updateTextAreaInput(session, "i_desc",          value = "")
    updateSelectInput(session,   "i_category",      selected = "")
    updateTextInput(session,     "i_date_created",  value = "")
    updateTextInput(session,     "i_date_acquired", value = "")
    updateTextAreaInput(session, "i_provenance",    value = "")
    updateSelectInput(session,   "i_condition",     selected = "")
    updateTextInput(session,     "i_location",      value = "")
    updateCheckboxInput(session, "i_display",       value = FALSE)
    updateSelectizeInput(session,"i_people",        selected = character(0))
    updateTextInput(session,     "i_role",          value = "")
  })

  output$item_table <- renderDT({
    refresh_items()
    df <- dbGetQuery(db,
      "SELECT i.item_id,
              i.accession_number AS Accession,
              i.title AS Title, i.category AS Category,
              c.name AS Collection,
              i.condition AS Condition,
              CASE i.is_on_display WHEN 1 THEN 'Yes' ELSE 'No' END AS [On Display]
       FROM item i
       JOIN collection c ON c.collection_id = i.collection_id
       ORDER BY i.title")
    item_table_data(df)
    df[, !names(df) %in% "item_id"]
  }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
     selection = "single")

  # Populate sidebar when a row is clicked
  observeEvent(input$item_table_rows_selected, {
    row <- input$item_table_rows_selected
    df  <- item_table_data()
    req(length(row) > 0, nrow(df) >= row)
    id  <- df$item_id[row]
    selected_item_id(id)

    it <- dbGetQuery(db, "SELECT * FROM item WHERE item_id = ?", list(id))
    people <- dbGetQuery(db,
      "SELECT person_id, role FROM item_person WHERE item_id = ?", list(id))

    updateSelectInput(session,   "i_collection",    selected = as.character(it$collection_id))
    updateTextInput(session,     "i_accession",     value = it$accession_number)
    updateTextInput(session,     "i_title",         value = it$title)
    updateTextAreaInput(session, "i_desc",          value = it$description %||% "")
    updateSelectInput(session,   "i_category",      selected = it$category %||% "")
    updateTextInput(session,     "i_date_created",  value = it$date_created %||% "")
    updateTextInput(session,     "i_date_acquired", value = it$date_acquired %||% "")
    updateTextAreaInput(session, "i_provenance",    value = it$provenance %||% "")
    updateSelectInput(session,   "i_condition",     selected = it$condition %||% "")
    updateTextInput(session,     "i_location",      value = it$location %||% "")
    updateCheckboxInput(session, "i_display",       value = as.logical(it$is_on_display))
    if (nrow(people) > 0) {
      updateSelectizeInput(session, "i_people", selected = as.character(people$person_id))
      updateTextInput(session, "i_role", value = paste(unique(people$role), collapse = ", "))
    } else {
      updateSelectizeInput(session, "i_people", selected = character(0))
      updateTextInput(session, "i_role", value = "")
    }
  })

  # ── IMAGE save ───────────────────────────────────────────────
  observeEvent(input$img_save, {
    feedbackDanger("img_item", is.null(input$img_item) || input$img_item == "", "Required")
    req(input$img_item != "")

    filename <- if (!is.null(input$img_file)) input$img_file$name else "unknown"

    # Copy uploaded file to www/images/ if provided
    if (!is.null(input$img_file)) {
      dir.create("www/images", showWarnings = FALSE, recursive = TRUE)
      file.copy(input$img_file$datapath,
                file.path("www/images", input$img_file$name),
                overwrite = TRUE)
    }

    # If setting as primary, unset existing primary for this item
    if (isTRUE(input$img_primary)) {
      dbExecute(db,
        "UPDATE image SET is_primary = 0 WHERE item_id = ?",
        list(as.integer(input$img_item)))
    }

    na_if_empty <- function(x) if (is.null(x) || length(x) == 0 || trimws(x) == "") NA_character_ else trimws(x)
    dbExecute(db,
      "INSERT INTO image (item_id, filename, caption, is_primary) VALUES (?, ?, ?, ?)",
      list(as.integer(input$img_item), filename,
           na_if_empty(input$img_caption), as.integer(isTRUE(input$img_primary)))
    )
    refresh_images(refresh_images() + 1)
    showNotification("Image record saved.", type = "message")
  })

  output$image_table <- renderDT({
    refresh_images()
    dbGetQuery(db,
      "SELECT img.image_id AS ID, i.title AS Item,
              img.filename AS Filename, img.caption AS Caption,
              CASE img.is_primary WHEN 1 THEN 'Yes' ELSE 'No' END AS [Is Primary],
              img.uploaded_at AS Uploaded
       FROM image img
       JOIN item i ON i.item_id = img.item_id
       ORDER BY img.uploaded_at DESC")
  }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
     selection = "single")

  # ── BROWSE ───────────────────────────────────────────────────
  observe({
    refresh_collections()
    col <- get_collections()
    choices <- setNames(c("", col$collection_id), c("All", col$name))
    updateSelectInput(session, "br_collection", choices = choices)
  })


  # Store browse results so row-click can identify the accession number
  browse_results <- reactiveVal(data.frame())

  observeEvent(input$br_go, {
    output$browse_table <- renderDT({
      q <- "SELECT i.item_id AS item_id,
                   i.accession_number AS Accession, i.title AS Title,
                   i.category AS Category, c.name AS Collection,
                   i.date_created AS Created, i.condition AS Condition,
                   i.location AS Location,
                   CASE i.is_on_display WHEN 1 THEN 'Yes' ELSE 'No' END AS [On Display]
            FROM item i
            JOIN collection c ON c.collection_id = i.collection_id
            WHERE 1=1"
      params <- list()

      if (!is.null(input$br_collection) && input$br_collection != "") {
        q <- paste0(q, " AND i.collection_id = ?")
        params <- c(params, list(as.integer(input$br_collection)))
      }
      if (!is.null(input$br_category) && input$br_category != "") {
        q <- paste0(q, " AND i.category = ?")
        params <- c(params, list(input$br_category))
      }
      if (!is.null(input$br_display) && input$br_display != "") {
        q <- paste0(q, " AND i.is_on_display = ?")
        params <- c(params, list(as.integer(input$br_display)))
      }
      if (!is.null(input$br_search) && input$br_search != "") {
        q <- paste0(q, " AND (i.title LIKE ? OR i.description LIKE ?)")
        term <- paste0("%", input$br_search, "%")
        params <- c(params, list(term, term))
      }

      q <- paste0(q, " ORDER BY i.title")
      df <- if (length(params) == 0) dbGetQuery(db, q) else dbGetQuery(db, q, params)
      browse_results(df)
      df[, !names(df) %in% "item_id"]   # hide item_id from display
    }, options = list(pageLength = 15, scrollX = TRUE, rownames = FALSE),
       selection = "single")
  }, ignoreNULL = FALSE)

  # ── ITEM REPORT on row click ──────────────────────────────────
  output$item_report_ui <- renderUI({
    req(input$browse_table_rows_selected)
    row   <- input$browse_table_rows_selected
    df    <- browse_results()
    req(nrow(df) >= row)
    item_id <- df$item_id[row]

    # Pull all data for this item
    item <- dbGetQuery(db,
      "SELECT i.*, c.name AS collection_name
       FROM item i JOIN collection c ON c.collection_id = i.collection_id
       WHERE i.item_id = ?", list(item_id))

    people <- dbGetQuery(db,
      "SELECT p.first_name || ' ' || p.last_name AS name,
              p.nationality, ip.role, ip.notes
       FROM item_person ip
       JOIN person p ON p.person_id = ip.person_id
       WHERE ip.item_id = ?", list(item_id))

    images <- dbGetQuery(db,
      "SELECT filename, caption, is_primary FROM image WHERE item_id = ?",
      list(item_id))

    # ── Build report card ───────────────────────────────────────
    field <- function(label, val) {
      if (is.null(val) || length(val) == 0) return(NULL)
      val <- as.character(val)
      if (is.na(val) || trimws(val) == "") return(NULL)
      tags$div(class = "mb-2",
        tags$span(class = "fw-bold text-muted small", paste0(label, ": ")),
        tags$span(val)
      )
    }

    people_section <- if (nrow(people) > 0) {
      tags$div(
        tags$h6("Associated People", class = "mt-3 border-bottom pb-1"),
        tags$table(class = "table table-sm table-striped",
          tags$thead(tags$tr(
            tags$th("Name"), tags$th("Role"), tags$th("Nationality"), tags$th("Notes")
          )),
          tags$tbody(
            lapply(seq_len(nrow(people)), function(i) {
              tags$tr(
                tags$td(people$name[i]),
                tags$td(people$role[i]),
                tags$td(people$nationality[i] %||% "—"),
                tags$td(people$notes[i]   %||% "—")
              )
            })
          )
        )
      )
    } else tags$p(class = "text-muted fst-italic", "No associated people recorded.")

    images_section <- if (nrow(images) > 0) {
      tags$div(
        tags$h6("Images", class = "mt-3 border-bottom pb-1"),
        tags$ul(
          lapply(seq_len(nrow(images)), function(i) {
            primary_badge <- if (images$is_primary[i] == 1)
              tags$span(class = "badge bg-success ms-2", "Primary") else NULL
            tags$li(
              tags$code(images$filename[i]), primary_badge,
              if (!is.na(images$caption[i]) && images$caption[i] != "")
                tags$span(class = "text-muted ms-2", paste0("— ", images$caption[i]))
            )
          })
        )
      )
    } else tags$p(class = "text-muted fst-italic", "No images recorded.")

    card(
      card_header(
        class = "bg-primary text-white",
        tags$div(class = "d-flex justify-content-between align-items-center",
          tags$span(tags$strong(item$title)),
          tags$span(class = "badge bg-light text-dark", item$accession_number)
        )
      ),
      tags$div(class = "p-3",
        layout_columns(
          col_widths = c(6, 6),
          tags$div(
            tags$h6("Item Details", class = "border-bottom pb-1"),
            field("Collection",    item$collection_name),
            field("Category",      item$category),
            field("Date Created",  item$date_created),
            field("Date Acquired", item$date_acquired),
            field("Condition",     item$condition),
            field("Location",      item$location),
            field("On Display",    if (item$is_on_display == 1) "Yes" else "No")
          ),
          tags$div(
            tags$h6("Description & Provenance", class = "border-bottom pb-1"),
            if (!is.na(item$description) && item$description != "")
              tags$p(item$description)
            else tags$p(class = "text-muted fst-italic", "No description."),
            if (!is.na(item$provenance) && item$provenance != "") tags$div(
              tags$strong("Provenance:"),
              tags$p(item$provenance)
            )
          )
        ),
        people_section,
        images_section
      )
    )
  })


}



shinyApp(ui, server)
