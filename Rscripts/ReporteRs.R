    library(ReporteRs)
    formatP <- function (p) { format(p, scientific = -1, digits = 2) }

    font <- function (x) {theme(text = element_text(size = x))}
    poslegend <- function (c) {theme(legend.position = c)}

    thFormat <- function (x) {
        format(x, big.mark = ",")
    }

    office.table <- function (dataset, add.rownames = FALSE, color = "#5B9BD5", fontsize = 10) {
        for (j in names(dataset)) {
            if (is.numeric(dataset[, j])) {
                dataset[, j] <- format(dataset[, j])
            } else {}
        }
        ft <- FlexTable(dataset, add.rownames = add.rownames)
        ft <- setFlexTableBackgroundColors(ft, i = 1, colors = color, to = "header")
        ft[, , to = "header"] <- textBold(font.size = fontsize, color = "white")
        ft[, , to = "header"] <- parRight(padding.left = 4, padding.right = 4)
        ft[, , to = "body"] <- textNormal(font.size = fontsize)
        ft[, , to = "body"] <- parRight(padding.left = 4, padding.right = 4)
        ft <- setFlexTableBorders(
            ft,
            inner.vertical = borderProperties(width = 0, color = color),
            inner.horizontal = borderProperties(width = 1, color = color),
            outer.vertical = borderProperties(width = 0, color = color),
            outer.horizontal = borderProperties(width = 1, color = color)
            )
        return(ft)
    }

    addNewTab <- function (data, legend, header.columns = TRUE, header = NULL, header.colspan = ncol(data), footer = NULL, footer.colspan = ncol(data), doc, pagebreak = TRUE) {
        restable <- FlexTable(
            data = data,
            header.text.props = textProperties(font.weight = "bold"),
            header.columns = header.columns
        )
        if (!is.null(footer)) {
            restable <- addFooterRow(
                restable,
                value = footer,
                colspan = footer.colspan,
                text.properties = textProperties(font.size = 8, font.style = "italic")
            )
        } else {}
        if (!is.null(header)) {
            addHeaderRow(restable, value = header, header.colspan)
        } else {}
        restable <- setFlexTableBorders(
            restable,
            inner.vertical = borderNone(),
            inner.horizontal = borderNone(),
            outer.vertical = borderNone(),
            outer.horizontal = borderProperties(style = "solid", width = 2),
            footer = TRUE
        )
        doc <- addParagraph(
            doc,
            pot(
                legend,
                textProperties(
                    font.size = 12,
                    font.weight = "bold",
                    font.family = "Times New Roman"
                )
            )
        )
        doc <- addFlexTable(doc, restable, parProperties(text.align = "center"))
        if (pagebreak) {
            doc <- addPageBreak(doc)
        } else {
            doc <- addParagraph(doc, "")
        }
        return(invisible(doc))
    }


    # report <- docx(template = "../../PROJECT/Mickael/DEV/DefaultProjectFiles/template.docx")
    # report <- addParagraph(doc = report, "Title", stylename = "Titre")
    # report <- addParagraph(doc = report, "Mickaël Canouil (UMR8199 CNRS / Université de Lille 2 / Institut Pasteur de Lille)", stylename = "Authors")
    # report <- addParagraph(doc = report, "Mickaël Canouil (University of Lille, CNRS, Institut Pasteur de Lille, UMR 8199 - EGID, F-59000 Lille, France)", stylename = "Authors")
    # report <- addParagraph(doc = report, as.character(Sys.Date()), stylename = "Date")
    # report <- addTitle(report, "Table of contents", level =  1)
    # report <- addTOC(doc = report)

    # report <- addPageBreak(doc = report)
    # report <- addParagraph(doc = report, "Section", stylename = "Titre1")

    # report <- addFlexTable(doc = report, office.table(TABLE), par.properties = parCenter())
    # report <- addParagraph(doc = report, 'Caption', stylename = "TableCaption")
    # report <- addParagraph(doc = report, '', stylename = "Corpsdetexte")

    # report <- addPlot(doc = report, fun = function(){print(p)}, par.properties = parCenter(), width = 7.5/1.25, height = 6/1.25)
    # report <- addParagraph(doc = report, 'Caption', stylename = "ImageCaption")
    # report <- addParagraph(doc = report, '', stylename = "Corpsdetexte")

    # writeDoc(report, file = paste0("../../PROJECT/.../Report/..._Report-REV-", format(Sys.time(), "%d-%m-%y"), ".docx"))