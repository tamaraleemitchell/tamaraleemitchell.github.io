library(stringr)
na_blank <- function(x) {
  x[is.na(x)] <- ""
  x
}

fmt_names <- function(x, max_names = 4) {
  x <- x %>%
    mutate(fn = substr(first_name, 1, 1),
           mn = substr(na_blank(middle_name), 1, 1),
           nn = str_glue("{last_name}, {fn}{mn}")) %>%
    pull(nn) %>%
    as.character()
  if (length(x) > max_names) x <- c(x[1:max_names], "et al.")
  paste(x, collapse = ", ")
}

clean_str <- function(str) {
  str <- str_remove_all(str, "\\{")
  str <- str_remove_all(str, "\\}")
  str <- str_replace(str, '\\\\+\"', "")
  str <- str_replace(str, "\\+", "")
  tools::toTitleCase(str)
}

fmt_title <- function(title) {
  unname(str_glue("<h5>{clean_str(title)}</h5>"))
}

fmt_jvpy <- function(bibtype, journal, conference, volume, pgs, year) {
  jc <- case_when(
    !is.na(journal) ~ journal,
    is.na(journal) & !is.na(conference) ~ conference,
    bibtype %in% c("TECHREPORT", "MISC") ~ "Technical report",
    bibtype == "PHDTHESIS" ~ "Ph.D. Dissertation",
    TRUE ~ ""
  )
  jc <- na_blank(clean_str(jc))
  vol <- case_when(
    !is.na(volume) ~ str_glue("{volume}, {pgs}, {year}"),
    is.na(volume) ~ str_glue("{clean_str(year)}"),
  )
  unname(str_glue("<p>{jc}, {vol}</p>"))
}

clean_urls <- function(url) {
  str <- str_remove_all(url, "\\{")
  str <- str_remove_all(url, "\\}")
  str
}

mk_link <- function(x, colour, fa, link_text) {
  x <- na_blank(clean_urls(x))
  x[nchar(x) > 0L] <- str_glue(
    "<a href={x[nchar(x) > 0L]} class='btn-links mr2 ba dib' ",
    " target='_blank'><button type='button' class='btn btn-outline-primary btn-sm'>",
    "<i class='{fa}'></i> ",
    "{link_text}",
    "</button></a></li>")
  x
}


fmt_links <- function(url, arxiv, pdf, code) {
  url <- mk_link(url, "blue", "bi bi-link-45deg", "journal")
  arxiv <- mk_link(arxiv, "red", "ai ai-arxiv", "arXiv")
  pdf <- mk_link(pdf, "green", "bi bi-file-earmark-pdf", "pdf")
  code <- mk_link(code, "gold", "bi bi-code-slash", "code")
  all_links <- paste0("<footer><p class='db pt1'>", url, arxiv, pdf, code, "</p></footer>")
  all_links
}

