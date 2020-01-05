data_navbar <- function(pkg = ".", depth = 0L) {
  pkg <- as_pkgdown(pkg)

  # Take structure as is from meta
  navbar <- purrr::pluck(pkg, "meta", "navbar")
  structure <- navbar$structure %||% navbar_structure()

  # Merge components from meta
  components <- navbar_components(pkg)
  components_meta <- navbar$components %||% list()
  components[names(components_meta)] <- components_meta
  components <- purrr::compact(components)

  # Any unplaced components go to the right of the left navbar
  right_comp <- intersect(structure$right, names(components))
  left_comp <- intersect(structure$left, names(components))
  extra_comp <- setdiff(names(components), c(left_comp, right_comp))

  # Backward compatiblity
  left <- navbar$left %||% components[c(left_comp, extra_comp)]
  right <- navbar$right %||% components[right_comp]

  list(
    type = navbar$type %||% "default",
    left = render_navbar_links(left, depth = depth),
    right = render_navbar_links(right, depth = depth)
  )
}

render_navbar_links <- function(x, depth = 0L) {
  stopifnot(is.integer(depth), depth >= 0L)

  tweak <- function(x) {
    if (!is.null(x$menu)) {
      x$menu <- lapply(x$menu, tweak)
      x
    } else if (!is.null(x$href) && !grepl("://", x$href, fixed = TRUE)) {
      x$href <- paste0(up_path(depth), x$href)
      x
    } else {
      x
    }
  }

  if (depth != 0L) {
    x <- lapply(x, tweak)
  }

  navbar_links_html(x)
}

navbar_links_html <- function(links) {
  as.character(navbar_links_tags(links))
}

#' @importFrom htmltools tags tagList
navbar_links_tags <- function(links, depth = 0) {
  if (is.null(links)) {
    return(htmltools::tagList())
  }

  tags <- lapply(links, function(x) {
    if (!is.null(x$menu)) {
      menu_class <- "dropdown"
      link_text <- navbar_link_text(x, " ", tags$span(class = "caret"))

      submenuLinks <- navbar_links_tags(x$menu, depth = depth + 1)

      tags$li(
        class = "nav-item dropdown",
        tags$a(
          href = "#",
          class = "nav-link dropdown-toggle",
          `data-toggle` = "dropdown",
          role = "button",
          `aria-expanded` = "false",
          link_text
        ),
        tags$ul(
          class = "dropdown-menu dropdown-menu-right",
          submenuLinks
        )
      )
    } else if (!is.null(x$text) && grepl("^\\s*-{3,}\\s*$", x$text)) {
      tags$li(class = "dropdown-divider")
    } else if (!is.null(x$text) && is.null(x$href)) {
      tags$li(
        tags$h6(class = "dropdown-header", x$text)
      )
    } else if (depth > 0) {
      textTags <- navbar_link_text(x)
      tags$a(
        class = "dropdown-item",
        href = x$href,
        textTags
      )
    } else {
      textTags <- navbar_link_text(x)
      tags$li(
        class = "nav-item",
        tags$a(
          class = "nav-link",
          href = x$href,
          textTags
        )
      )
    }
  })

  tagList(tags)
}

navbar_link_text <- function(x, ...) {
  if (is.null(x$icon)) {
    return(tagList(x$text, ...))
  }

  split <- strsplit(x$icon, "-")

  if (length(split[[1]]) > 1) {
    iconset <- split[[1]][[1]]
  } else {
    iconset <- ""
  }

  tagList(
    tags$span(
      class = paste(iconset, x$icon)
    ),
    " ",
    x$text,
    ...
  )
}

# Default navbar ----------------------------------------------------------

navbar_structure <- function() {
  print_yaml(list(
    left = c("home", "intro", "reference", "articles", "tutorials", "news"),
    right = "github"
  ))
}

navbar_components <- function(pkg = ".") {
  pkg <- as_pkgdown(pkg)

  menu <- list()
  menu$home <- menu_icon("home", "index.html")
  menu$reference <- menu_link("Reference", "reference/index.html")
  menu$tutorials <- menu("Tutorials",
    menu_links(pkg$tutorials$title, pkg$tutorials$file_out)
  )

  vignettes <- pkg$vignettes
  pkg_intro <- vignettes$name == pkg$package
  if (any(pkg_intro)) {
    intro <- vignettes[pkg_intro, , drop = FALSE]
    vignettes <- vignettes[!pkg_intro, , drop = FALSE]

    menu$intro <- menu_link("Get started", intro$file_out)
  }
  menu$articles <-  menu("Articles", menu_links(vignettes$title, vignettes$file_out))
  menu$news <- navbar_news(pkg)

  if (!is.null(pkg$github_url)) {
    menu$github <- menu_icon("github", pkg$github_url, style = "fab")
  }

  print_yaml(menu)
}


# Menu helpers -------------------------------------------------------------

menu <- function(text, children) {
  if (length(children) == 0)
    return()
  list(text = text, menu = children)
}
menu_link <- function(text, href) {
  list(text = text, href = href)
}
menu_links <- function(text, href) {
  purrr::map2(text, href, ~ list(text = .x, href = .y))
}
menu_icon <- function(icon, href, style = "fas") {
  list(icon = paste0(style, " fa-", icon, " fa-lg"), href = href)
}
menu_text <- function(text) {
  list(text = text)
}
menu_spacer <- function() {
   menu_text("---------")
}

