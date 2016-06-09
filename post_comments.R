suppressPackageStartupMessages({
  library(hypothesisr)
  library(github)
  library(lubridate)
  library(dplyr)
  library(stringr)
  library(jsonlite)
  library(markdown)
  library(purrr)
})

# Read in necessary GitHub and hypothes.is tokens. To replicate, create an .rda
# file by save()-ing:
# 1. gh_id: The client ID of a github developer application
# 2. gh_secret: The applicaiton secret of a github developer application
# 3. gh_token: A personal GitHub access token that you create
# 4. h_token: A hypothes.is developer token
load("gh_token.rda")
cts <- create.github.context(client_id = gh_id, client_secret = gh_secret,
                             personal_token = gh_token)

# A very hacky way to establish which annotations are new or not based on the
# last run-time of the script. The default time is set to allow annotations that
# happened after the most recent wave of PH responses to annotations.
last_run_file <- "last_run.rds"
if(!file.exists(last_run_file)) {
  last_run <- ymd_hms("2016-04-30 12:00:00")
} else {
  last_run <- readRDS(last_run_file)
}

# Search for the most recently-updated annotations
ph <- hs_search_all(any = "programminghistorian", sort = "updated",
                    order = "desc")

# Get those annotations made after the last run of this script
new_comments <- ph %>%
  mutate(date = ymd_hms(updated)) %>%
  filter(date > last_run)

# Produces the JSON needed to submit an HTML-styled issue based on an annotation
format_comment <- function(a) {
  i_title <- paste("h annotation:", a$document.highwire.title)
  a_uri <- a$uri
  a_date <- format(a$date, format = "%Y-%m-%d")
  a_text <- a$text
  a_context <- a$links.incontext
  a_highlight <- a$target[[1]]$selector[[1]]$exact[3]

  i_body <- paste(
    paste0("**Lesson**: [", i_title, "](", a_uri, ")"),
    paste0("**Annotation submitted**: ", a_date),
    paste0("[**Context**](", a_context, ")"),
    paste0(">", a_highlight),
    "**Annotation**",
    paste0(">", a_text),
    "---",
    "_This issue was automatically generated._",
    sep = "\n\n"
  )

  toJSON(list(
    title = unbox(i_title),
    body = unbox(markdownToHTML(text = i_body, fragment.only = TRUE))
  ))
}

# Create a new annotation replying to the original, with a link to the generated
# issue
post_reply <- function(a, i) {
  reply_text <- paste("An issue based on your annotation has been created in the Programming Historian GitHub repository: ", i)
  hs_reply(token = h_token, user = "acct:mdlincoln@hypothes.is", id = a$id, text = reply_text)
}

# Step through all new comments
message("Creating ", nrow(new_comments), " new issue(s).")
invisible(by_row(new_comments, function(x) {
  ir <- create.issue("programminghistorian", "jekyll",
                     content = format_comment(x))
  post_reply(x, ir$content$url)
  message("Issue created: ", ir$content$title)
}))

# Save the run time of this script
saveRDS(Sys.time(), file = last_run_file)
