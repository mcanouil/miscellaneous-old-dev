#---------------------------------------------------------------------------------
# Name - S4generator.R
# Desc - Build a S4 class object with several methods
# Author - Mickael Canouil
# Source code - https://github.com/mcanouil/DEV/S4generator.R
#---------------------------------------------------------------------------------


header.S4 <- function (name, file = paste0(name, "-Class.R"), append = TRUE) {
    nb <- paste0(rep("#", floor((70 - nchar(paste0(" Class ", name, " ")))/2)), collapse = "")
    title <- paste(nb, "Class", name, nb)
    if (nchar(title)<70) {
        code <- c(rep("#", 70), "\n", title, "#\n", rep("#", 30), " Creation ", rep("#", 30), "\n", rep("#", 70), "\n\n\n")
    } else {
        code <- c(rep("#", 70), "\n", title, "\n", rep("#", 30), " Creation ", rep("#", 30), "\n", rep("#", 70), "\n\n\n")
    }
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

setClass.S4 <- function (name, field, type, file = paste0(name, "-Class.R"), append = TRUE) {
    lengthField <- length(field)
    code <- c("### Class definition ###\n",
        "setClass(\n\tClass = \"", name, "\", \n\trepresentation = representation(\n",
        paste0(paste0(sapply(seq(lengthField), function (i) { paste0("\t\t", field[i], " = \"", type[i], "\"") }), collapse = ", \n"), "\n"),
        "\t), \n\tprototype = prototype(\n",
        paste0(paste0(sapply(seq(lengthField), function (i) {
            if(type[i]=="matrix") {
                paste0("\t\t", field[i], " = ", "matrix(, nrow = 0, ncol = 0)")
            } else {
                paste0("\t\t", field[i], " = ", type[i], "()")
            } }), collapse = ", \n"), "\n"),
        "\t)# , \n\t# validity = function (object) {\n\t\t# cat(\"**** validity ", name, " <empty> ****\\n\")\n\t\t# return(TRUE)\n\t# }\n)\n\n\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

new.S4 <- function (name, field, type, file = paste0(name, "-Class.R"), append = TRUE) {
    lengthField <- length(field)
    code <- c("### Constructor ###\n",
        "setGeneric(name = \"new.", name, "\", def = function (", paste0(field, collapse = ", "), ") {standardGeneric(\"new.", name, "\")})\n",
        "setMethod(f = \"new.", name, "\", signature = c(", paste0(rep("\"missing\"", lengthField), collapse = ", "),
            "), definition = function (", paste0(field, collapse = ", "), ") {new(\"", name, "\")})\n",
        paste0("setMethod(f = \"new.", name, "\", signature = c("),
        paste0(rep("\"ANY\"", lengthField), collapse = ", "), "), definition = function (", paste0(field, collapse = ", "), ") {\n",
        paste0("\tif (missing(", field, ")) {", field, " <- ", type, "()} else {}\n", collapse = ""),
        "\treturn(new(\"", name, "\"", paste0(", ", field, " = ", field, collapse = ""), "))\n})\n\n\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

is.S4 <- function (name, file = paste0(name, "-Class.R"), append = TRUE) {
    code <- c("### Is ###\n",
        "setGeneric(name = \"is.", name, "\", def = function (object) {standardGeneric(\"is.", name, "\")})\n",
        "setMethod(f = \"is.", name, "\", signature = \"ANY\", definition = function (object) {\n",
        "\tif (length(object)>1) {\n",
        "\t\treturn(sapply(object, is.", name, "))\n",
        "\t} else {\n",
        "\t\tif (class(object) == \"", name, "\") {\n",
        "\t\t\treturn(TRUE)\n",
        "\t\t} else {\n",
        "\t\t\treturn(FALSE)\n",
        "\t\t}\n",
        "\t}\n",
        "})\n\n\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

show.S4 <- function (name, file = paste0(name, "-Class.R"), append = TRUE) {
    code <- c("### Show ###\n",
        "setMethod(f = \"show\", signature = \"", name, "\", definition = function (object){\n",
        # "\tcat(\"\t~~~ Class:\", class(object), \"~~~\\n\")\n",
        "\tshowSlot <- function (slot) {\n",
        "\t\tsNames <- gsub(\"^[^@]*@(.*)\", \"\\\\1\", slot)\n",
        "\t\teSlot <- eval(parse(text = slot))\n",
        "\t\ttmp <- switch(EXPR = class(eSlot),\n",
        "\t\t\t\"matrix\" = {\n",
        "\t\t\t\tcat(paste0(\" ~ \", sNames, \" : [\", nrow(eSlot), \"x\", ncol(eSlot), \"]\", collapse = \"\"))\n",
        "\t\t\t\tif (all(dim(eSlot)==0)) {\n",
        "\t\t\t\t\tcat(\"NA\")\n",
        "\t\t\t\t} else {\n",
        "\t\t\t\t\tcat(\"\\n",
        "\")\n",
        "\t\t\t\t\tnrowShow <- seq(min(5, nrow(eSlot)))\n",
        "\t\t\t\t\tncolShow <- seq(min(5, ncol(eSlot)))\n",
        "\t\t\t\t\tshortObject <- eSlot[nrowShow, ncolShow]\n",
        "\t\t\t\t\tif (is.null(rownames(shortObject))) {\n",
        "\t\t\t\t\t\trownames(shortObject) <- seq(nrow(shortObject))\n",
        "\t\t\t\t\t} else {}\n",
        "\t\t\t\t\tif (is.null(colnames(shortObject))) {\n",
        "\t\t\t\t\t\tcolnames(shortObject) <- seq(ncol(shortObject))\n",
        "\t\t\t\t\t} else {}\n",
        "\t\t\t\t\tresFormat <- format(cbind(c(\"\", rownames(shortObject)), rbind(colnames(shortObject), format(shortObject, digits = 4))), justify = \"centre\")\n",
        "\t\t\t\t\tif (nrow(shortObject)!=nrow(eSlot)) {\n",
        "\t\t\t\t\t\tresFormat <- rbind(resFormat, c(\".\", sapply(seq(colnames(shortObject)), function (iCol) {paste0(rep(\".\", nchar(resFormat[1, 1])), collapse = \"\")})))\n",
        "\t\t\t\t\t} else {}\n",
        "\t\t\t\t\tif (ncol(shortObject)!=ncol(eSlot)) {\n",
        "\t\t\t\t\t\tresFormat <- cbind(resFormat, c(\".\", rep(paste0(rep(\".\", nchar(resFormat[1, 1])), collapse = \"\"), nrow(resFormat)-1)))\n",
        "\t\t\t\t\t} else {}\n",
        "\t\t\t\t\tcat(paste0(\"     \", apply(format(resFormat, justify = \"centre\"), 1, paste, collapse = \" \"), \"\\n",
        "\", collapse = \"\"))\n",
        "\t\t\t\t}\n",
        "\t\t\t\tcat(\"\\n\")\n",
        "\t\t\t},\n",
        "\t\t\t\"data.frame\" = {\n",
        "\t\t\t\tcat(paste0(\" ~ \", sNames, \" : [\", nrow(eSlot), \"x\", ncol(eSlot), \"]\", collapse = \"\"))\n",
        "\t\t\t\tif (all(dim(eSlot)==0)) {\n",
        "\t\t\t\t\tcat(\"NA\")\n",
        "\t\t\t\t} else {\n",
        "\t\t\t\t\tcat(\"\\n",
        "\")\n",
        "\t\t\t\t\tnrowShow <- seq(min(5, nrow(eSlot)))\n",
        "\t\t\t\t\tncolShow <- seq(min(5, ncol(eSlot)))\n",
        "\t\t\t\t\tshortObject <- eSlot[nrowShow, ncolShow]\n",
        "\t\t\t\t\tif (is.null(rownames(shortObject))) {\n",
        "\t\t\t\t\t\trownames(shortObject) <- seq(nrow(shortObject))\n",
        "\t\t\t\t\t} else {}\n",
        "\t\t\t\t\tif (is.null(colnames(shortObject))) {\n",
        "\t\t\t\t\t\tcolnames(shortObject) <- seq(ncol(shortObject))\n",
        "\t\t\t\t\t} else {}\n",
        "\t\t\t\t\tresFormat <- format(cbind(c(\"\", rownames(shortObject)), rbind(colnames(shortObject), format(shortObject, digits = 4))), justify = \"centre\")\n",
        "\t\t\t\t\tif (nrow(shortObject)!=nrow(eSlot)) {\n",
        "\t\t\t\t\t\tresFormat <- rbind(resFormat, c(\".\", sapply(seq(colnames(shortObject)), function (iCol) {paste0(rep(\".\", nchar(resFormat[1, 1])), collapse = \"\")})))\n",
        "\t\t\t\t\t} else {}\n",
        "\t\t\t\t\tif (ncol(shortObject)!=ncol(eSlot)) {\n",
        "\t\t\t\t\t\tresFormat <- cbind(resFormat, c(\".\", rep(paste0(rep(\".\", nchar(resFormat[1, 1])), collapse = \"\"), nrow(resFormat)-1)))\n",
        "\t\t\t\t\t} else {}\n",
        "\t\t\t\t\tcat(paste0(\"     \", apply(format(resFormat, justify = \"centre\"), 1, paste, collapse = \" \"), \"\\n",
        "\", collapse = \"\"))\n",
        "\t\t\t\t}\n",
        "\t\t\t\tcat(\"\\n\")\n",
        "\t\t\t},\n",
        "\t\t\t\"numeric\" = {\n",
        "\t\t\t\tcat(paste0(\" ~ \", sNames, \" : \", collapse = \"\"))\n",
        "\t\t\t\tif (length(eSlot) == 0) {\n",
        "\t\t\t\t\tcat(\"NA\")\n",
        "\t\t\t\t} else {\n",
        "\t\t\t\t\tif (length(eSlot)>1) {\n",
        "\t\t\t\t\t\tcat(paste0(\"[\", length(eSlot), \"] \", paste0(format(head(eSlot), digits = 4), collapse = \" \")))\n",
        "\t\t\t\t\t} else {\n",
        "\t\t\t\t\t\tcat(format(eSlot, digits = 4))\n",
        "\t\t\t\t\t}\n",
        "\t\t\t\t}\n",
        "\t\t\t\tcat(\"\\n",
        "\")\n",
        "\t\t\t},\n",
        "\t\t\t\"character\" = {\n",
        "\t\t\t\tcat(paste0(\" ~ \", sNames, \" : \", collapse = \"\"))\n",
        "\t\t\t\tif (length(eSlot) == 0) {\n",
        "\t\t\t\t\tcat(\"NA\")\n",
        "\t\t\t\t} else {\n",
        "\t\t\t\t\tif (length(eSlot)>1) {\n",
        "\t\t\t\t\t\tcat(\"[\", length(eSlot), \"] \\\"\", paste0(head(eSlot), collapse = \"\\\" \\\"\"), \"\\\"\", sep = \"\")\n",
        "\t\t\t\t\t} else {\n",
        "\t\t\t\t\t\tcat(paste0(\"\\\"\", eSlot, \"\\\"\"))\n",
        "\t\t\t\t\t}\n",
        "\t\t\t\t}\n",
        "\t\t\t\tcat(\"\\n",
        "\")\n",
        "\t\t\t},\n",
        "\t\t\t{\n",
        "\t\t\t\tcat(paste0(\" ~ \", sNames, \" : \", collapse = \"\"))\n",
        "\t\t\t\tif (length(eSlot) == 0) {\n",
        "\t\t\t\t\tcat(\"NA\")\n",
        "\t\t\t\t} else {\n",
        "\t\t\t\t\tif (length(eSlot)>1) {\n",
        "\t\t\t\t\t\tcat(paste0(\"[\", length(eSlot), \"] \", paste0(head(eSlot), collapse = \" \")))\n",
        "\t\t\t\t\t} else {\n",
        "\t\t\t\t\t\tcat(eSlot)\n",
        "\t\t\t\t\t}\n",
        "\t\t\t\t}\n",
        "\t\t\t\tcat(\"\\n",
        "\")\n",
        "\t\t\t}\n",
        "\t\t)\n",
        "\t\treturn(invisible())\n",
        "\t}\n",
        "\tshowObject <- function (object) {\n",
        "\t\tcat(\"\t~~~ Class:\", class(object), \"~~~\\n",
        "\")\n",
        "\t\tsNames <- paste0(\"object@\", slotNames(object))\n",
        "\t\ttrash <- sapply(sNames, showSlot)\n",
        "\t\treturn(invisible())\n",
        "\t}\n",
        "\tshowObject(object)\n",
        "\treturn(invisible(object))\n",
        "})\n\n\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

get.S4 <- function (name, field, type, file = paste0(name, "-Class.R"), append = TRUE) {
    lengthField <- length(field)
    code <- c("### Getteur ###\n",
        "setMethod(f = \"[\", signature = \"", name, "\", definition = function (x, i, j, drop){\n\tswitch(EXPR = i, \n",
        sapply(seq(lengthField), function (i) {
            if (type[i]=="list") {
                paste0("\t\t\"", field[i], "\" = {\n\t\t\tif (missing(j)) {\n\t\t\t\treturn(x@", field[i], ")\n\t\t\t} else {\n",
                    "\t\t\t\tif (j>length(x@", field[i], ")) {\n\t\t\t\t\tstop(\"[", name, ":get] indice out of limits\")\n\t\t\t\t} else {\n",
                    "\t\t\t\t\treturn(x@", field[i], "[[j]])\n\t\t\t\t}\n\t\t\t}\n\t\t}, \n")
            } else {
                if (type[i]=="matrix" | type[i]=="data.frame") {
                    paste0("\t\t\"", field[i], "\" = {return(x@", field[i], ")}, \n")
                } else {
                    paste0("\t\t\"", field[i], "\" = {\n\t\t\tif (missing(j)) {\n\t\t\t\treturn(x@", field[i], ")\n\t\t\t} else {\n",
                        "\t\t\t\tif (j>length(x@", field[i], ")) {\n\t\t\t\t\tstop(\"[", name, ":get] indice out of limits\")\n",
                        "\t\t\t\t} else {\n\t\t\t\t\treturn(x@", field[i], "[j])\n\t\t\t\t}\n\t\t\t}\n\t\t}, \n")
                }
            }
        }),
        "\t\tstop(\"[", name, ":get] \", i, \" is not a \\\"", name, "\\\" slot\")\n\t)\n})\n\n\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

set.S4 <- function (name, field, type, file = paste0(name, "-Class.R"), append = TRUE) {
    lengthField <- length(field)
    code <- c("### Setteur ###\n",
        "setMethod(f = \"[<-\", signature = \"", name, "\", definition = function (x, i, j, value){\n    switch(EXPR = i, \n",
        sapply(seq(lengthField), function (i) {
            if (type[i]=="list") {
                paste0("\t\t\"", field[i], "\" = {\n\t\t\tif (missing(j)) {\n\t\t\t\tx@", field[i], " <- value\n\t\t\t} else {\n",
                    "\t\t\t\tif (j>length(x@", field[i], ")) {\n\t\t\t\t\tstop(\"[", name, ":set] indice out of limits\")\n",
                    "\t\t\t\t} else {\n\t\t\t\t\tx@", field[i], "[[j]] <- value\n\t\t\t\t}\n\t\t\t}\n\t\t}, \n")
            } else {
                if (type[i]=="matrix" | type[i]=="data.frame") {
                    paste0("\t\t\"", field[i], "\" = {x@", field[i], " <- value}, \n")
                } else {
                    paste0("\t\t\"", field[i], "\" = {\n\t\t\tif (missing(j)) {\n\t\t\t\tx@", field[i], " <- value\n\t\t\t} else {\n",
                    "\t\t\t\tif (j>length(x@", field[i], ")) {\n\t\t\t\t\tstop(\"[", name, ":set] indice out of limits\")\n",
                    "\t\t\t\t} else {\n\t\t\t\t\tx@", field[i], "[j] <- value\n\t\t\t\t}\n\t\t\t}\n\t\t}, \n")
                }
            }
        }),
        "\t\tstop(\"[", name, ":set] \", i, \" is not a \\\"", name, "\\\" slot\")\n",
        "\t)\n\tvalidObject(x)\n\treturn(invisible(x))\n})\n\n\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

summary.S4 <- function (name, file = paste0(name, "-Class.R"), append = TRUE) {
    code <- c("### Summary ###\n",
        "setMethod(f = \"summary\", signature = \"", name , "\", definition = function (object){\n",
        "\tif (missing(object)){\n",
        "\t\tstop(\"[", name, ":summary] \\\"object\\\" is missing\", call. = FALSE)\n",
        "\t\treturn(invisible())\n",
        "\t} else {}\n",
        "\twarning(\"[", name, ":summary] No summary method defined for \\\"", name, "\\\" object!\", call. = FALSE)\n",
        "\treturn(invisible(object))\n",
        "})\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

helpClass.S4 <- function (clName, filename = NULL, where = topenv(parent.frame())) {
    type = "class"
    keywords = c("classes", "class", clName)
    generatorName <- clName
    .makeCallString <- function (def, name = substitute(def), args = formalArgs(def)) {
        if (is.character(def)) {
            if (missing(name)) {
                name <- def
            } else {}
            def <- getFunction(def)
        }
        if (is(def, "function")) {
            paste0(name, "(", paste(args, collapse = ", "), ")")
        } else {
            ""
        }
    }
    .fileDesc <- function (file) {
        if (is.character(file)) {
            if (nzchar(file)) {
                paste(" to the file", sQuote(file))
            } else {
                " to the standard output connection"
            }
        } else {
            if(inherits(file, "connection")) {
                paste(" to the connection", sQuote(summary(file)$description))
            } else {
                ""
            }
        }
    }
    classInSig <- function (g, where, cl) {
        cl %in% unique(unlist(findMethods(g, where)@signatures))
    }
    genWithClass <- function (cl, where) {
        allgen <- getGenerics(where = where)
        ok <- as.logical(unlist(lapply(allgen, classInSig, cl = cl, where = where)))
        allgen[ok]
    }
    sigsList <- function (g, where) {
        methods <- findMethods(g, where)
        value <- methods@signatures
        args <- methods@arguments
        if (length(value)) {
            length(args) <- length(value[[1]])
            value <- lapply(value, function (x) {
                names(x) <- args
                x
            })
        } else {}
        value
    }
    slotClassWithSource <- function (clname) {
        clDef <- getClassDef(clname)
        extds <- names(clDef@contains)
        allslots <- getSlots(clDef)
        for (j in rev(seq_along(extds))) {
            i <- extds[[j]]
            slotsi <- getSlots(getClass(i))
            if (length(slotsi)) {
                allslots[names(slotsi)] <- paste0("\"", as.character(slotsi), "\", from class \"", i, "\"")
            } else {}
        }
        slotsi <- getSlots(clDef)
        if (length(slotsi)) {
            allslots[names(slotsi)] <- paste0("\"", as.character(slotsi), "\"")
        } else {}
        allslots
    }
    cleanPrompt <- function (object, name) {
        value <- prompt(object, name = name, filename = NA)
        for (i in seq_along(value)) {
            item <- value[[i]]
            bad <- grepl("^ *%", item)
            if (any(bad)) {
                value[[i]] <- item[!bad]
            } else {}
        }
        value
    }
    pastePar <- function (x) {
        xn <- names(x)
        x <- as.character(x)
        xn <- if (length(xn) == length(x)) {
            paste(xn, "= ")
        } else {
            ""
        }
        paste0("(", paste0(xn, "\"", x, "\"", collapse = ", "), ")")
    }
    escape <- function (txt) {
        gsub("%", "\\\\%", txt)
    }

    if (is.null(filename)) {
        filename <- paste0(utils:::topicName(type, clName), ".Rd")
    } else {}

    if (!missing(where) && !is.na(match(clName, getClasses(where)))) {
        whereClass <- where
    } else {
        whereClass <- find(classMetaName(clName))
        if (length(whereClass) == 0L) {
            stop(gettextf("no definition of class %s found", dQuote(clName)), domain = NA)
        } else {
            if (length(whereClass) > 1L) {
                if (identical(where, topenv(parent.frame()))) {
                    whereClass <- whereClass[[1L]]
                    warning(gettextf("multiple definitions of %s found; using the one on %s", dQuote(clName), whereClass), domain = NA)
                } else {
                    if (exists(classMetaName(clName), where, inherits = FALSE)) {
                        whereClass <- where
                    } else {
                        stop(sprintf(ngettext(length(whereClass),
                            "no definition of class %s in the specified position, %s, definition on : %s",
                            "no definition of class %s in the specified position, %s, definitions on : %s"),
                            dQuote(clName), where, paste(whereClass, collapse = ", ")), domain = NA)
                    }
                }
            } else {}
        }
    }

    fullName <- utils:::topicName("class", clName)
    clDef <- getClass(clName, where = whereClass)
    .name <- paste0("\\name{", fullName, "}")
    .type <- paste0("\\docType{", type, "}")
    .alias <- paste0("\\alias{", fullName, "}")
    .title <- paste0("\n\\title{Class \\code{\"", clName, "\"}}")
    .desc <- paste0("\n\\description{", "\n%%\t~~ A concise (1-5 lines) description of what the class is. ~~\n}")
    slotclasses <- getSlots(clDef)
    slotnames <- names(slotclasses)
    slotclasses <- as.character(slotclasses)
    nslots <- length(slotclasses)
    clNameQ <- paste0("\"", clName, "\"")
    .usage <- "\n\\section{Objects from the Class}"
    virtualClass <- isVirtualClass(clName)

    if (virtualClass) {
        .usage <- paste0(.usage, "{\nA virtual Class: No objects may be created from it.\n}")
        generator <- NULL
    } else {
        if (exists(generatorName, where, inherits = FALSE)) {
            generator <- get(generatorName, where, inherits = FALSE)
        } else {
            generator <- NULL
        }
        if (is(generator, "classGeneratorFunction")) {
            promptGenerator <- cleanPrompt(generator, generatorName)
            callString <- .makeCallString(generator, generatorName)
            .alias <- c(.alias, promptGenerator$aliases)
        } else {
            initMethod <- unRematchDefinition(selectMethod("initialize", clName))
            argNames <- formalArgs(initMethod)
            argNames[[1L]] <- clNameQ
            callString <- .makeCallString(initMethod, "new", argNames)
        }
        .usage <- paste0(.usage, "{\nObjects can be created by calls of the form \\code{", callString, "}.\n%%\t~~ describe objects here ~~ \n}")
    }

    .slots <- if (nslots > 0) {
        slotclasses <- slotClassWithSource(clName)
        slotnames <- names(slotclasses)
        .slots.head <- c("\n\\section{Slots}{", "\t\\describe{")
        .slots.body <- paste0("\t\t\\item{\\code{", slotnames, "}}", "{[", slotclasses, "]: Object of class \\code{", slotclasses, "} }")
        .slots.tail <- c("\t}", "}")
        c(.slots.head, .slots.body, .slots.tail)
    } else {
        character()
    }

    .extends <- clDef@contains
    if (length(.extends)) {
        .extends <- showExtends(.extends, printTo = FALSE)
        .extends <- c("\n\\section{Extends}{\n", paste0("Class \\code{\"\\linkS4class{", .extends$what, "}\"}, ", gsub("^(by class) (\".*\")$", "\\1 \\\\code{\\2}", .extends$how), "."), "\n}")
    } else {
        .extends <- character()
    }
    nmeths <- length(methnms <- genWithClass(clName, where = whereClass))
    .meths.head <- "\n\\section{Methods}{"
    .methAliases <- ""

    if (nmeths > 0) {
        .meths.body <- "\t\\describe{"
        for (i in 1L:nmeths) {
            .sig <- sigsList(methnms[i], where = whereClass)
            for (j in seq_along(.sig)) {
                if (!all(is.na(match(.sig[[j]], clName)))) {
                    methn.i <- escape(methnms[i])
                    .meths.body <- c(.meths.body, paste0("\t\t\\item{", methn.i, "}{\\code{signature", pastePar(.sig[[j]]), "}: ... }"))
                    cur <- paste(.sig[[j]], collapse = ",")
                    .methAliases <- paste0(.methAliases, "\\alias{", methn.i, ",", cur, "-method}\n")
                } else {}
            }
        }
        .meths.body <- c(.meths.body, "\t}")
    } else {
        .meths.head <- "\n\\section{Methods}{"
        .meths.body <- paste("No methods defined with class", clNameQ, "in the signature.")
    }

    .meths.tail <- "}"
    .keywords <- paste0("\n", paste0("\\keyword{", keywords, "}"), collapse = "")
    Rdtxt <- list(
        name = .name,
        type = .type,
        aliases = .alias,
        methAliases = .methAliases,
        title = .title,
        description = .desc,
        `section{Objects from the Class}` = .usage,
        `section{Slots}` = .slots,
        `section{Extends}` = .extends,
        `section{Methods}` = paste0(c(.meths.head, .meths.body, .meths.tail), collapse = "\n"),
        references = "\n\\references{\n%%\t~~put references to the literature/web site here~~\n}",
        author = "\n\\author{\n%%\t~~who you are~~\n}",
        note = "\n\\note{\n%%\t~~further notes~~\n}\n\n%%  ~~Make other sections like Warning with \\section{Warning }{....} ~~",
        seealso = "\n\\seealso{\n%%\t~~objects to See Also as \\code{\\link{~~fun~~}}, ~~~%%\t~~or \\code{\\linkS4class{CLASSNAME}} for links to other classes ~~~\n}",
        examples = paste0("\n\\examples{\nshowClass(", clNameQ, ")\n}"),
        keywords = .keywords
    )

    if (is(clDef, "refClassRepresentation")) {
        Rdtxt <- refClassPrompt(clDef, Rdtxt, nmeths, nslots, .meths.head)
    } else {
        if (is(generator, "classGeneratorFunction")) {
            what <- c("usage", "arguments")
            Rdtxt[what] <- promptGenerator[what]
        } else {}
    }

    if (is.na(filename)) {
        return(Rdtxt)
    } else {}

    cat(unlist(Rdtxt), file = filename, sep = "\n")
    # message("A shell of class documentation has been written", .fileDesc(filename), ".\n")
    return(invisible(paste0(unlist(Rdtxt), collapse = "")))
}

helpMethod.S4 <- function (f, filename = NULL, where = topenv(parent.frame())) {
    .requirePackage <- function (package, mustFind = TRUE) {
        value <- package

        if (nzchar(package)) {
            if(package %in% loadedNamespaces())
                value <- getNamespace(package)
            else {
                if (identical(package, ".GlobalEnv")) {
                    return(.GlobalEnv)
                } else {}
                if (identical(package, "methods")) {
                    return(topenv(parent.frame()))
                } else {}
                if (exists(package, envir = .PackageEnvironments, inherits = FALSE)) {
                    return(get(package, envir = .PackageEnvironments))
                } else {}
            }
        }

        if (is.environment(value)) {
            return(value)
        } else {}

        topEnv <- options()$topLevelEnvironment

        if(is.null(topEnv)) {
            topEnv <- .GlobalEnv
        } else {}

        if (exists(".packageName", topEnv, inherits=TRUE) && .identC(package, get(".packageName", topEnv))) {
            return(topEnv)
        } else {}

        if (!(nzchar(package) && require(package, character.only = TRUE))) {
            if (mustFind) {
                stop(gettextf("unable to find required package %s", sQuote(package)), domain = NA)
            } else {
                return(NULL)
            }
        } else {}

        value <- .asEnvironmentPackage(package)
        assign(package, value, envir = .PackageEnvironments)
        return(value)
    }

    .genEnv <-  function (f, default = .requirePackage("methods"), package = "") {
        if (!nzchar(package)) {
            package <- packageSlot(f)
        } else {}
        if (is.null(package)) {
            value <- default
            def <- .getGeneric(f, value)
            if(is.null(def)) {
                value <- .GlobalEnv
                def <- .getGeneric(f, value)
                if (is.null(def)) {
                    value <- .requirePackage("methods")
                    if  (!identical(default, value)) {
                        def <- .getGeneric(f, value)
                    } else {}
                } else {}
            } else {}
            if(is.null(def)) {
                baseenv()
            } else {
                value
            }
        } else {
            .requirePackage(package)
        }
    }

    escape <- function (txt) {
        gsub("%", "\\\\%", txt)
    }
    packageString <- ""
    fdef <- getGeneric(f)

    if (!isGeneric(f, fdef = fdef)) {
        stop(gettextf("no generic function found corresponding to %s", sQuote(f)), domain = NA)
    } else {}

    methods <- findMethods(fdef)
    where <- .genEnv(fdef, where)

    if (!identical(where, .GlobalEnv)) {
        packageString <- sprintf("in Package \\pkg{%s}", getPackageName(where))
    } else {}

    fullName <- utils:::topicName("methods", f)
    n <- length(methods)
    labels <- character(n)
    aliases <- character(n)
    signatures <- findMethodSignatures(methods = methods, target = TRUE)
    args <- colnames(signatures)

    for (i in seq_len(n)) {
        sigi <- signatures[i, ]
        labels[[i]] <- sprintf("\\code{signature(%s)}", paste(sprintf("%s = \"%s\"", args, escape(sigi)), collapse = ", "))
        aliases[[i]] <- paste0("\\alias{", utils:::topicName("method", c(f, signatures[i, ])), "}")
    }
    text <- paste0("\t\t\\item{", labels, "}{ ~~describe this method here~~ }")
    text <- c("\n\\section{Methods}{\n\t\\describe{", text, "\t}\n}")
    aliasText <- c(paste0("\\alias{", escape(fullName), "}"), escape(aliases))

    if (identical(filename, FALSE)) {
        return(c(aliasText, text))
    } else {}

    if (is.null(filename) || identical(filename, TRUE)) {
        filename <- paste0(fullName, ".Rd")
    } else {}

    Rdtxt <- list(
        name = paste0("\\name{", fullName, "}"),
        type = "\\docType{methods}",
        aliases = aliasText,
        title = sprintf("\n\n\\title{Methods for Function \\code{%s} %s}", f, packageString),
        description = paste0("\n\\description{\n%%  ~~ Methods for function", " \\code{", f, "} ", sub("^in Package", "in package", packageString), " ~~\n}"),
        `section{Methods}` = text,
        keywords = c("\n\\keyword{methods}", "\\keyword{method}", "\\keyword{new}", paste0("\\keyword{", f, "}"))
    )

    if (is.na(filename)) {
        return(Rdtxt)
    } else {}

    cat(unlist(Rdtxt), file = filename, sep = "\n")
    # message("A shell of methods documentation has been written", .fileDesc(filename), ".\n")
    return(invisible(paste0(unlist(Rdtxt), collapse = "")))
}

createS4 <- function (name, field, type, file = paste0(name, "-class.R")) {
    .fileDesc <- function (file) {
        if (is.character(file)) {
            if (nzchar(file)) {
                paste(" to the file", sQuote(file))
            } else {
                " to the standard output connection"
            }
        } else {
            if(inherits(file, "connection")) {
                paste(" to the connection", sQuote(summary(file)$description))
            } else {
                ""
            }
        }
    }

    # dir.create(name)
    # dirName <- paste0("./", name, "/")
    dirName <- ""

    header.part <- header.S4(name, file = paste0(dirName, file), append = FALSE)
    setClass.part <- setClass.S4(name, field, type, file = paste0(dirName, file), append = TRUE)
    new.part <- new.S4(name, field, type, file = paste0(dirName, file), append = TRUE)
    is.part <- is.S4(name, file = paste0(dirName, file), append = TRUE)
    show.part <- show.S4(name, file = paste0(dirName, file), append = TRUE)
    get.part <- get.S4(name, field, type, file = paste0(dirName, file), append = TRUE)
    set.part <- set.S4(name, field, type, file = paste0(dirName, file), append = TRUE)
    summary.part <- summary.S4(name, file = paste0(dirName, file), append = TRUE)
    code <- paste0(c(header.part, setClass.part, new.part, is.part, show.part, get.part, set.part, summary.part), collapse = "")
    envTmp <- new.env()
    eval(parse(text = code), envir = envTmp)
    message(paste("A shell of class script has been written", .fileDesc(file = paste0(dirName, file)), "."))

    # helpClass.part <- helpClass.S4(clName = name, filename = paste0(dirName, name, "-class.Rd"), where = envTmp)
    # message("A shell of class documentation has been written", .fileDesc(paste0(file = paste0(dirName, file), "d")), ".")
    # helpMethod.part <- sapply(c(paste0("new.", name), paste0("is.", name)), function (f) {
        # helpMethod.part <- helpMethod.S4(f = f, filename = paste0(dirName, f, "-methods.Rd"), where = envTmp)
        # message("A shell of methods documentation has been written", .fileDesc(paste0(dirName, f, "-methods.Rd")), ".")
        # return(invisible(helpMethod.part))
    # })
    return(invisible(code))
}

### Run Test
# source("/disks/PROJECT/Mickael/DEV/R/S4generator.R")
# SLOT <- c("numeric", "list", "matrix", "data.frame", "integer", "character", "array")
# createS4(name = "Generator", field = paste0("F", SLOT), type = SLOT)
