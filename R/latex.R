# LaTeX output for generated figures: one figure environment per figure,
# collected into a single .tex file with a section per paper part.

#' Escape text for LaTeX (outside math mode)
latex_escape <- function(x) {
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
  gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
}

#' A LaTeX figure environment
#'
#' @param file Graphics path as LaTeX should see it (relative to `\graphicspath`).
#' @param caption Caption (LaTeX; not escaped).
#' @param label Label, e.g. `"fig:1b-main-A"`.
#' @param width Width, default `\linewidth`.
#' @param placement Float placement.
latex_figure <- function(file, caption, label, width = "\\linewidth", placement = "htbp") {
  paste0("\\begin{figure}[", placement, "]\n",
         "  \\centering\n",
         "  \\includegraphics[width=", width, "]{", file, "}\n",
         "  \\caption{", caption, "}\n",
         "  \\label{", label, "}\n",
         "\\end{figure}\n")
}

#' Write figure environments for a manifest of figures into one .tex file
#'
#' @param manifest `data.table` with `section`, `group`, `file` (relative to
#'   the .tex file), `caption`, `label`; rows are written in order.
#' @param path Output `.tex` path.
#' @param sections Section order and headings, named by `section` value.
#' @param clear_every Insert `\clearpage` after this many figures so LaTeX
#'   never holds too many unplaced floats.
#' @param header Comment lines for the top of the file.
write_latex_figures <- function(manifest, path, sections = c(body = "Main-text figures", appendix = "Appendix figures",
                                                             supplementary = "Supplementary figures"),
                                clear_every = 4L, header = character()) {
  out <- c(paste0("% ", header), "")
  for (s in names(sections)) {
    m <- manifest[section == s]
    if (!nrow(m)) next
    out <- c(out, sprintf("%% %s", strrep("=", 70)), sprintf("\\section*{%s}", sections[[s]]), "")
    k <- 0L
    for (g in unique(m$group)) {
      mg <- m[group == g]
      out <- c(out, sprintf("\\subsection*{%s}", g), "")
      for (i in seq_len(nrow(mg))) {
        out <- c(out, latex_figure(mg$file[i], mg$caption[i], mg$label[i]))
        k <- k + 1L
        if (k %% clear_every == 0L) out <- c(out, "\\clearpage", "")
      }
    }
    out <- c(out, "\\clearpage", "")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(out, path)
  invisible(path)
}
