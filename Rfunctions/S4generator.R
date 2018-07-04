#---------------------------------------------------------------------------------
# Name - S4generator.R
# Desc - Build a S4 class object with several methods
# Author - Mickael Canouil
# Source code - https://github.com/mcanouil/DEV/S4generator.R
#---------------------------------------------------------------------------------


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @param file PARAM_DESCRIPTION, Default: paste0(name, "-Class.R")
#' @param append PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname header.S4
#' @export 
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

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @param field PARAM_DESCRIPTION
#' @param type PARAM_DESCRIPTION
#' @param file PARAM_DESCRIPTION, Default: paste0(name, "-Class.R")
#' @param append PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname setClass.S4
#' @export 
setClass.S4 <- function (name, field, type, file = paste0(name, "-Class.R"), append = TRUE) {
    lengthField <- length(field)
    code <- c("### Class definition ###\n",
        "setClass(\n  Class = \"", name, "\", \n  representation = representation(\n",
        paste0(paste0(sapply(seq(lengthField), function (i) { paste0("    ", field[i], " = \"", type[i], "\"") }), collapse = ", \n"), "\n"),
        "  ), \n  prototype = prototype(\n",
        paste0(paste0(sapply(seq(lengthField), function (i) {
            if(type[i]=="matrix") {
                paste0("    ", field[i], " = ", "matrix(, nrow = 0, ncol = 0)")
            } else {
                paste0("    ", field[i], " = ", type[i], "()")
            } }), collapse = ", \n"), "\n"),
        "  )# , \n  # validity = function (object) {\n    # cat(\"**** validity ", name, " <empty> ****\\n\")\n    # return(TRUE)\n  # }\n)\n\n\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @param field PARAM_DESCRIPTION
#' @param type PARAM_DESCRIPTION
#' @param file PARAM_DESCRIPTION, Default: paste0(name, "-Class.R")
#' @param append PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname new.S4
#' @export 
new.S4 <- function (name, field, type, file = paste0(name, "-Class.R"), append = TRUE) {
    lengthField <- length(field)
    code <- c("### Constructor ###\n",
        "setGeneric(name = \"new.", name, "\", def = function (", paste0(field, collapse = ", "), ") {standardGeneric(\"new.", name, "\")})\n",
        "setMethod(f = \"new.", name, "\", signature = c(", paste0(rep("\"missing\"", lengthField), collapse = ", "),
            "), definition = function (", paste0(field, collapse = ", "), ") {new(\"", name, "\")})\n",
        paste0("setMethod(f = \"new.", name, "\", signature = c("),
        paste0(rep("\"ANY\"", lengthField), collapse = ", "), "), definition = function (", paste0(field, collapse = ", "), ") {\n",
        paste0("  if (missing(", field, ")) {", field, " <- ", type, "()} else {}\n", collapse = ""),
        "  return(new(\"", name, "\"", paste0(", ", field, " = ", field, collapse = ""), "))\n})\n\n\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @param file PARAM_DESCRIPTION, Default: paste0(name, "-Class.R")
#' @param append PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname is.S4
#' @export 
is.S4 <- function (name, file = paste0(name, "-Class.R"), append = TRUE) {
    code <- c("### Is ###\n",
        "setGeneric(name = \"is.", name, "\", def = function (object) {standardGeneric(\"is.", name, "\")})\n",
        "setMethod(f = \"is.", name, "\", signature = \"ANY\", definition = function (object) {\n",
        "  if (length(object)>1) {\n",
        "    return(sapply(object, is.", name, "))\n",
        "  } else {\n",
        "    if (class(object) == \"", name, "\") {\n",
        "      return(TRUE)\n",
        "    } else {\n",
        "      return(FALSE)\n",
        "    }\n",
        "  }\n",
        "})\n\n\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @param file PARAM_DESCRIPTION, Default: paste0(name, "-Class.R")
#' @param append PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname show.S4
#' @export 
show.S4 <- function (name, file = paste0(name, "-Class.R"), append = TRUE) {
    code <- c("### Show ###\n",
        "setMethod(f = \"show\", signature = \"", name, "\", definition = function (object){\n",
        # "  cat(\"  ~~~ Class:\", class(object), \"~~~\\n\")\n",
        "  showSlot <- function (slot) {\n",
        "    sNames <- gsub(\"^[^@]*@(.*)\", \"\\\\1\", slot)\n",
        "    eSlot <- eval(parse(text = slot))\n",
        "    tmp <- switch(EXPR = class(eSlot),\n",
        "      \"matrix\" = {\n",
        "        cat(paste0(\" ~ \", sNames, \" : [\", nrow(eSlot), \"x\", ncol(eSlot), \"]\", collapse = \"\"))\n",
        "        if (all(dim(eSlot)==0)) {\n",
        "          cat(\"NA\")\n",
        "        } else {\n",
        "          cat(\"\\n",
        "\")\n",
        "          nrowShow <- seq(min(5, nrow(eSlot)))\n",
        "          ncolShow <- seq(min(5, ncol(eSlot)))\n",
        "          shortObject <- eSlot[nrowShow, ncolShow]\n",
        "          if (is.null(rownames(shortObject))) {\n",
        "            rownames(shortObject) <- seq(nrow(shortObject))\n",
        "          } else {}\n",
        "          if (is.null(colnames(shortObject))) {\n",
        "            colnames(shortObject) <- seq(ncol(shortObject))\n",
        "          } else {}\n",
        "          resFormat <- format(cbind(c(\"\", rownames(shortObject)), rbind(colnames(shortObject), format(shortObject, digits = 4))), justify = \"centre\")\n",
        "          if (nrow(shortObject)!=nrow(eSlot)) {\n",
        "            resFormat <- rbind(resFormat, c(\".\", sapply(seq(colnames(shortObject)), function (iCol) {paste0(rep(\".\", nchar(resFormat[1, 1])), collapse = \"\")})))\n",
        "          } else {}\n",
        "          if (ncol(shortObject)!=ncol(eSlot)) {\n",
        "            resFormat <- cbind(resFormat, c(\".\", rep(paste0(rep(\".\", nchar(resFormat[1, 1])), collapse = \"\"), nrow(resFormat)-1)))\n",
        "          } else {}\n",
        "          cat(paste0(\"     \", apply(format(resFormat, justify = \"centre\"), 1, paste, collapse = \" \"), \"\\n",
        "\", collapse = \"\"))\n",
        "        }\n",
        "        cat(\"\\n\")\n",
        "      },\n",
        "      \"data.frame\" = {\n",
        "        cat(paste0(\" ~ \", sNames, \" : [\", nrow(eSlot), \"x\", ncol(eSlot), \"]\", collapse = \"\"))\n",
        "        if (all(dim(eSlot)==0)) {\n",
        "          cat(\" NA\")\n",
        "        } else {\n",
        "          cat(\"\\n",
        "\")\n",
        "          nrowShow <- seq(min(5, nrow(eSlot)))\n",
        "          ncolShow <- seq(min(5, ncol(eSlot)))\n",
        "          shortObject <- eSlot[nrowShow, ncolShow]\n",
        "          if (is.null(rownames(shortObject))) {\n",
        "            rownames(shortObject) <- seq(nrow(shortObject))\n",
        "          } else {}\n",
        "          if (is.null(colnames(shortObject))) {\n",
        "            colnames(shortObject) <- seq(ncol(shortObject))\n",
        "          } else {}\n",
        "          resFormat <- format(cbind(c(\"\", rownames(shortObject)), rbind(colnames(shortObject), format(shortObject, digits = 4))), justify = \"centre\")\n",
        "          if (nrow(shortObject)!=nrow(eSlot)) {\n",
        "            resFormat <- rbind(resFormat, c(\".\", sapply(seq(colnames(shortObject)), function (iCol) {paste0(rep(\".\", nchar(resFormat[1, 1])), collapse = \"\")})))\n",
        "          } else {}\n",
        "          if (ncol(shortObject)!=ncol(eSlot)) {\n",
        "            resFormat <- cbind(resFormat, c(\".\", rep(paste0(rep(\".\", nchar(resFormat[1, 1])), collapse = \"\"), nrow(resFormat)-1)))\n",
        "          } else {}\n",
        "          cat(paste0(\"     \", apply(format(resFormat, justify = \"centre\"), 1, paste, collapse = \" \"), \"\\n",
        "\", collapse = \"\"))\n",
        "        }\n",
        "        cat(\"\\n\")\n",
        "      },\n",
        "      \"numeric\" = {\n",
        "        cat(paste0(\" ~ \", sNames, \" : \", collapse = \"\"))\n",
        "        if (length(eSlot) == 0) {\n",
        "          cat(\"NA\")\n",
        "        } else {\n",
        "          if (length(eSlot)>1) {\n",
        "            cat(paste0(\"[\", length(eSlot), \"] \", paste0(format(head(eSlot), digits = 4), collapse = \" \")))\n",
        "          } else {\n",
        "            cat(format(eSlot, digits = 4))\n",
        "          }\n",
        "        }\n",
        "        cat(\"\\n",
        "\")\n",
        "      },\n",
        "      \"character\" = {\n",
        "        cat(paste0(\" ~ \", sNames, \" : \", collapse = \"\"))\n",
        "        if (length(eSlot) == 0) {\n",
        "          cat(\"NA\")\n",
        "        } else {\n",
        "          if (length(eSlot)>1) {\n",
        "            cat(\"[\", length(eSlot), \"] \\\"\", paste0(head(eSlot), collapse = \"\\\" \\\"\"), \"\\\"\", sep = \"\")\n",
        "          } else {\n",
        "            cat(paste0(\"\\\"\", eSlot, \"\\\"\"))\n",
        "          }\n",
        "        }\n",
        "        cat(\"\\n",
        "\")\n",
        "      },\n",
        "      {\n",
        "        cat(paste0(\" ~ \", sNames, \" : \", collapse = \"\"))\n",
        "        if (length(eSlot) == 0) {\n",
        "          cat(\"NA\")\n",
        "        } else {\n",
        "          if (length(eSlot)>1) {\n",
        "            cat(paste0(\"[\", length(eSlot), \"] \", paste0(head(eSlot), collapse = \" \")))\n",
        "          } else {\n",
        "            cat(eSlot)\n",
        "          }\n",
        "        }\n",
        "        cat(\"\\n",
        "\")\n",
        "      }\n",
        "    )\n",
        "    return(invisible())\n",
        "  }\n",
        "  showObject <- function (object) {\n",
        "    cat(\"  ~~~ Class:\", class(object), \"~~~\\n",
        "\")\n",
        "    sNames <- paste0(\"object@\", slotNames(object))\n",
        "    trash <- sapply(sNames, showSlot)\n",
        "    return(invisible())\n",
        "  }\n",
        "  showObject(object)\n",
        "  return(invisible(object))\n",
        "})\n\n\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @param field PARAM_DESCRIPTION
#' @param type PARAM_DESCRIPTION
#' @param file PARAM_DESCRIPTION, Default: paste0(name, "-Class.R")
#' @param append PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get.S4
#' @export 
get.S4 <- function (name, field, type, file = paste0(name, "-Class.R"), append = TRUE) {
    lengthField <- length(field)
    code <- c("### Getteur ###\n",
        "setMethod(f = \"[\", signature = \"", name, "\", definition = function (x, i, j, drop){\n  switch(EXPR = i, \n",
        sapply(seq(lengthField), function (i) {
            if (type[i]=="list") {
                paste0("    \"", field[i], "\" = {\n      if (missing(j)) {\n        return(x@", field[i], ")\n      } else {\n",
                    "        if (j>length(x@", field[i], ")) {\n          stop(\"[", name, ":get] indice out of limits\")\n        } else {\n",
                    "          return(x@", field[i], "[[j]])\n        }\n      }\n    }, \n")
            } else {
                if (type[i]=="matrix" | type[i]=="data.frame") {
                    paste0("    \"", field[i], "\" = {return(x@", field[i], ")}, \n")
                } else {
                    paste0("    \"", field[i], "\" = {\n      if (missing(j)) {\n        return(x@", field[i], ")\n      } else {\n",
                        "        if (j>length(x@", field[i], ")) {\n          stop(\"[", name, ":get] indice out of limits\")\n",
                        "        } else {\n          return(x@", field[i], "[j])\n        }\n      }\n    }, \n")
                }
            }
        }),
        "    stop(\"[", name, ":get] \", i, \" is not a \\\"", name, "\\\" slot\")\n  )\n})\n\n\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @param field PARAM_DESCRIPTION
#' @param type PARAM_DESCRIPTION
#' @param file PARAM_DESCRIPTION, Default: paste0(name, "-Class.R")
#' @param append PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname set.S4
#' @export 
set.S4 <- function (name, field, type, file = paste0(name, "-Class.R"), append = TRUE) {
    lengthField <- length(field)
    code <- c("### Setteur ###\n",
        "setMethod(f = \"[<-\", signature = \"", name, "\", definition = function (x, i, j, value){\n    switch(EXPR = i, \n",
        sapply(seq(lengthField), function (i) {
            if (type[i]=="list") {
                paste0("    \"", field[i], "\" = {\n      if (missing(j)) {\n        x@", field[i], " <- value\n      } else {\n",
                    "        if (j>length(x@", field[i], ")) {\n          stop(\"[", name, ":set] indice out of limits\")\n",
                    "        } else {\n          x@", field[i], "[[j]] <- value\n        }\n      }\n    }, \n")
            } else {
                if (type[i]=="matrix" | type[i]=="data.frame") {
                    paste0("    \"", field[i], "\" = {x@", field[i], " <- value}, \n")
                } else {
                    paste0("    \"", field[i], "\" = {\n      if (missing(j)) {\n        x@", field[i], " <- value\n      } else {\n",
                    "        if (j>length(x@", field[i], ")) {\n          stop(\"[", name, ":set] indice out of limits\")\n",
                    "        } else {\n          x@", field[i], "[j] <- value\n        }\n      }\n    }, \n")
                }
            }
        }),
        "    stop(\"[", name, ":set] \", i, \" is not a \\\"", name, "\\\" slot\")\n",
        "  )\n  validObject(x)\n  return(invisible(x))\n})\n\n\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @param file PARAM_DESCRIPTION, Default: paste0(name, "-Class.R")
#' @param append PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname summary.S4
#' @export 
summary.S4 <- function (name, file = paste0(name, "-Class.R"), append = TRUE) {
    code <- c("### Summary ###\n",
        "setMethod(f = \"summary\", signature = \"", name , "\", definition = function (object){\n",
        "  if (missing(object)){\n",
        "    stop(\"[", name, ":summary] \\\"object\\\" is missing\", call. = FALSE)\n",
        "    return(invisible())\n",
        "  } else {}\n",
        "  warning(\"[", name, ":summary] No summary method defined for \\\"", name, "\\\" object!\", call. = FALSE)\n",
        "  return(invisible(object))\n",
        "})\n")
    cat(code, sep = "", file = file, append = append)
    return(invisible(code))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param clName PARAM_DESCRIPTION
#' @param filename PARAM_DESCRIPTION, Default: NULL
#' @param where PARAM_DESCRIPTION, Default: topenv(parent.frame())
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[utils]{topicName}}
#' @rdname helpClass.S4
#' @export 
#' @importFrom utils topicName
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
    .title <- paste0("\n\  itle{Class \\code{\"", clName, "\"}}")
    .desc <- paste0("\n\\description{", "\n%%  ~~ A concise (1-5 lines) description of what the class is. ~~\n}")
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
        .usage <- paste0(.usage, "{\nObjects can be created by calls of the form \\code{", callString, "}.\n%%  ~~ describe objects here ~~ \n}")
    }

    .slots <- if (nslots > 0) {
        slotclasses <- slotClassWithSource(clName)
        slotnames <- names(slotclasses)
        .slots.head <- c("\n\\section{Slots}{", "  \\describe{")
        .slots.body <- paste0("    \\item{\\code{", slotnames, "}}", "{[", slotclasses, "]: Object of class \\code{", slotclasses, "} }")
        .slots.tail <- c("  }", "}")
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
        .meths.body <- "  \\describe{"
        for (i in 1L:nmeths) {
            .sig <- sigsList(methnms[i], where = whereClass)
            for (j in seq_along(.sig)) {
                if (!all(is.na(match(.sig[[j]], clName)))) {
                    methn.i <- escape(methnms[i])
                    .meths.body <- c(.meths.body, paste0("    \\item{", methn.i, "}{\\code{signature", pastePar(.sig[[j]]), "}: ... }"))
                    cur <- paste(.sig[[j]], collapse = ",")
                    .methAliases <- paste0(.methAliases, "\\alias{", methn.i, ",", cur, "-method}\n")
                } else {}
            }
        }
        .meths.body <- c(.meths.body, "  }")
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
        references = "\n\\references{\n%%  ~~put references to the literature/web site here~~\n}",
        author = "\n\\author{\n%%  ~~who you are~~\n}",
        note = "\n\\note{\n%%  ~~further notes~~\n}\n\n%%  ~~Make other sections like Warning with \\section{Warning }{....} ~~",
        seealso = "\n\\seealso{\n%%  ~~objects to See Also as \\code{\\link{~~fun~~}}, ~~~%%  ~~or \\code{\\linkS4class{CLASSNAME}} for links to other classes ~~~\n}",
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

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param f PARAM_DESCRIPTION
#' @param filename PARAM_DESCRIPTION, Default: NULL
#' @param where PARAM_DESCRIPTION, Default: topenv(parent.frame())
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[utils]{topicName}}
#' @rdname helpMethod.S4
#' @export 
#' @importFrom utils topicName
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
    text <- paste0("    \\item{", labels, "}{ ~~describe this method here~~ }")
    text <- c("\n\\section{Methods}{\n  \\describe{", text, "  }\n}")
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
        title = sprintf("\n\n\  itle{Methods for Function \\code{%s} %s}", f, packageString),
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

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @param field PARAM_DESCRIPTION
#' @param type PARAM_DESCRIPTION
#' @param file PARAM_DESCRIPTION, Default: paste0(name, "-class.R")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname createS4
#' @export 
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
