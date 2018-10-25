#---------------------------------------------------------------------------------
# Name - S4generator.R
# Desc - Build a S4 class object with several methods
# Author - Mickael Canouil
# Source code - https://github.com/mcanouil/DEV/S4_generator.R
#---------------------------------------------------------------------------------


header_S4 <- function(name, file = paste0(name, "-Class.R"), append = TRUE) {
  nb <- paste0(rep("#", floor((70 - nchar(paste0(" Class ", name, " "))) / 2)), collapse = "")
  title <- paste(nb, "Class", name, nb)
  if (nchar(title) < 70) {
    code <- c(rep("#", 70), "\n", title, "#\n", rep("#", 30), " Creation ", rep("#", 30), "\n", rep("#", 70), "\n\n\n")
  } else {
    code <- c(rep("#", 70), "\n", title, "\n", rep("#", 30), " Creation ", rep("#", 30), "\n", rep("#", 70), "\n\n\n")
  }
  cat(code, sep = "", file = file, append = append)
  return(invisible(code))
}

set_class_S4 <- function(name, field, type, file = paste0(name, "-Class.R"), append = TRUE) {
  length_field <- length(field)
  code <- c(
    "### Class definition ###\n",
    "set.class(\n  Class = \"", name, "\", \n  representation = representation(\n",
    paste0(paste0(sapply(seq(length_field), function(i) {
      paste0("    ", field[i], " = \"", type[i], "\"")
    }), collapse = ", \n"), "\n"),
    "  ), \n  prototype = prototype(\n",
    paste0(paste0(sapply(seq(length_field), function(i) {
      if (type[i] == "matrix") {
        paste0("    ", field[i], " = ", "matrix(, nrow = 0, ncol = 0)")
      } else {
        paste0("    ", field[i], " = ", type[i], "()")
      }
    }), collapse = ", \n"), "\n"),
    "  )# , \n  # validity = function (object) {\n    # cat(\"**** validity ", name, " <empty> ****\\n\")\n    # return(TRUE)\n  # }\n)\n\n\n"
  )
  cat(code, sep = "", file = file, append = append)
  return(invisible(code))
}

new_S4 <- function(name, field, type, file = paste0(name, "-Class.R"), append = TRUE) {
  length_field <- length(field)
  code <- c(
    "### Constructor ###\n",
    "setGeneric(name = \"new_", name, "\", def = function (", paste0(field, collapse = ", "), ") {standardGeneric(\"new_", name, "\")})\n",
    "setMethod(f = \"new_", name, "\", signature = c(", paste0(rep("\"missing\"", length_field), collapse = ", "),
    "), definition = function (", paste0(field, collapse = ", "), ") {new(\"", name, "\")})\n",
    paste0("setMethod(f = \"new_", name, "\", signature = c("),
    paste0(rep("\"ANY\"", length_field), collapse = ", "), "), definition = function (", paste0(field, collapse = ", "), ") {\n",
    paste0("  if (missing(", field, ")) {", field, " <- ", type, "()}\n", collapse = ""),
    "  return(new(\"", name, "\"", paste0(", ", field, " = ", field, collapse = ""), "))\n})\n\n\n"
  )
  cat(code, sep = "", file = file, append = append)
  return(invisible(code))
}

is_S4 <- function(name, file = paste0(name, "-Class.R"), append = TRUE) {
  code <- c(
    "### Is ###\n",
    "setGeneric(name = \"is_", name, "\", def = function (object) {standardGeneric(\"is_", name, "\")})\n",
    "setMethod(f = \"is_", name, "\", signature = \"ANY\", definition = function (object) {\n",
    "  if (length(object)>1) {\n",
    "    return(sapply(object, is_", name, "))\n",
    "  } else {\n",
    "    if (class(object) == \"", name, "\") {\n",
    "      return(TRUE)\n",
    "    } else {\n",
    "      return(FALSE)\n",
    "    }\n",
    "  }\n",
    "})\n\n\n"
  )
  cat(code, sep = "", file = file, append = append)
  return(invisible(code))
}

show_S4 <- function(name, file = paste0(name, "-Class.R"), append = TRUE) {
  code <- c(
    "### Show ###\n",
    "setMethod(f = \"show\", signature = \"", name, "\", definition = function (object){\n",
    # "  cat(\"  ~~~ Class:\", class(object), \"~~~\\n\")\n",
    "  show_slot <- function (slot) {\n",
    "    string_name <- gsub(\"^[^@]*@(.*)\", \"\\\\1\", slot)\n",
    "    eval_slot <- eval(parse(text = slot))\n",
    "    tmp <- switch(EXPR = class(eval_slot),\n",
    "      \"matrix\" = {\n",
    "        cat(paste0(\" ~ \", string_name, \" : [\", nrow(eval_slot), \"x\", ncol(eval_slot), \"]\", collapse = \"\"))\n",
    "        if (all(dim(eval_slot)==0)) {\n",
    "          cat(\"NA\")\n",
    "        } else {\n",
    "          cat(\"\\n",
    "\")\n",
    "          show_nrow <- seq(min(5, nrow(eval_slot)))\n",
    "          show_ncol <- seq(min(5, ncol(eval_slot)))\n",
    "          short_object <- eval_slot[show_nrow, show_ncol]\n",
    "          if (is.null(rownames(short_object))) {\n",
    "            rownames(short_object) <- seq(nrow(short_object))\n",
    "          }\n",
    "          if (is.null(colnames(short_object))) {\n",
    "            colnames(short_object) <- seq(ncol(short_object))\n",
    "          }\n",
    "          format_show_output <- format(cbind(c(\"\", rownames(short_object)), rbind(colnames(short_object), format(short_object, digits = 4))), justify = \"centre\")\n",
    "          if (nrow(short_object)!=nrow(eval_slot)) {\n",
    "            format_show_output <- rbind(format_show_output, c(\".\", sapply(seq(colnames(short_object)), function (iCol) {paste0(rep(\".\", nchar(format_show_output[1, 1])), collapse = \"\")})))\n",
    "          }\n",
    "          if (ncol(short_object)!=ncol(eval_slot)) {\n",
    "            format_show_output <- cbind(format_show_output, c(\".\", rep(paste0(rep(\".\", nchar(format_show_output[1, 1])), collapse = \"\"), nrow(format_show_output)-1)))\n",
    "          }\n",
    "          cat(paste0(\"     \", apply(format(format_show_output, justify = \"centre\"), 1, paste, collapse = \" \"), \"\\n",
    "\", collapse = \"\"))\n",
    "        }\n",
    "        cat(\"\\n\")\n",
    "      },\n",
    "      \"data.frame\" = {\n",
    "        cat(paste0(\" ~ \", string_name, \" : [\", nrow(eval_slot), \"x\", ncol(eval_slot), \"]\", collapse = \"\"))\n",
    "        if (all(dim(eval_slot)==0)) {\n",
    "          cat(\" NA\")\n",
    "        } else {\n",
    "          cat(\"\\n",
    "\")\n",
    "          show_nrow <- seq(min(5, nrow(eval_slot)))\n",
    "          show_ncol <- seq(min(5, ncol(eval_slot)))\n",
    "          short_object <- eval_slot[show_nrow, show_ncol]\n",
    "          if (is.null(rownames(short_object))) {\n",
    "            rownames(short_object) <- seq(nrow(short_object))\n",
    "          }\n",
    "          if (is.null(colnames(short_object))) {\n",
    "            colnames(short_object) <- seq(ncol(short_object))\n",
    "          }\n",
    "          format_show_output <- format(cbind(c(\"\", rownames(short_object)), rbind(colnames(short_object), format(short_object, digits = 4))), justify = \"centre\")\n",
    "          if (nrow(short_object)!=nrow(eval_slot)) {\n",
    "            format_show_output <- rbind(format_show_output, c(\".\", sapply(seq(colnames(short_object)), function (iCol) {paste0(rep(\".\", nchar(format_show_output[1, 1])), collapse = \"\")})))\n",
    "          }\n",
    "          if (ncol(short_object)!=ncol(eval_slot)) {\n",
    "            format_show_output <- cbind(format_show_output, c(\".\", rep(paste0(rep(\".\", nchar(format_show_output[1, 1])), collapse = \"\"), nrow(format_show_output)-1)))\n",
    "          }\n",
    "          cat(paste0(\"     \", apply(format(format_show_output, justify = \"centre\"), 1, paste, collapse = \" \"), \"\\n",
    "\", collapse = \"\"))\n",
    "        }\n",
    "        cat(\"\\n\")\n",
    "      },\n",
    "      \"numeric\" = {\n",
    "        cat(paste0(\" ~ \", string_name, \" : \", collapse = \"\"))\n",
    "        if (length(eval_slot) == 0) {\n",
    "          cat(\"NA\")\n",
    "        } else {\n",
    "          if (length(eval_slot)>1) {\n",
    "            cat(paste0(\"[\", length(eval_slot), \"] \", paste0(format(head(eval_slot), digits = 4), collapse = \" \")))\n",
    "          } else {\n",
    "            cat(format(eval_slot, digits = 4))\n",
    "          }\n",
    "        }\n",
    "        cat(\"\\n",
    "\")\n",
    "      },\n",
    "      \"character\" = {\n",
    "        cat(paste0(\" ~ \", string_name, \" : \", collapse = \"\"))\n",
    "        if (length(eval_slot) == 0) {\n",
    "          cat(\"NA\")\n",
    "        } else {\n",
    "          if (length(eval_slot)>1) {\n",
    "            cat(\"[\", length(eval_slot), \"] \\\"\", paste0(head(eval_slot), collapse = \"\\\" \\\"\"), \"\\\"\", sep = \"\")\n",
    "          } else {\n",
    "            cat(paste0(\"\\\"\", eval_slot, \"\\\"\"))\n",
    "          }\n",
    "        }\n",
    "        cat(\"\\n",
    "\")\n",
    "      },\n",
    "      {\n",
    "        cat(paste0(\" ~ \", string_name, \" : \", collapse = \"\"))\n",
    "        if (length(eval_slot) == 0) {\n",
    "          cat(\"NA\")\n",
    "        } else {\n",
    "          if (length(eval_slot)>1) {\n",
    "            cat(paste0(\"[\", length(eval_slot), \"] \", paste0(head(eval_slot), collapse = \" \")))\n",
    "          } else {\n",
    "            cat(eval_slot)\n",
    "          }\n",
    "        }\n",
    "        cat(\"\\n",
    "\")\n",
    "      }\n",
    "    )\n",
    "    return(invisible())\n",
    "  }\n",
    "  show_object <- function (object) {\n",
    "    cat(\"  ~~~ Class:\", class(object), \"~~~\\n",
    "\")\n",
    "    string_name <- paste0(\"object@\", slotNames(object))\n",
    "    trash <- sapply(string_name, show_slot)\n",
    "    return(invisible())\n",
    "  }\n",
    "  show_object(object)\n",
    "  return(invisible(object))\n",
    "})\n\n\n"
  )
  cat(code, sep = "", file = file, append = append)
  return(invisible(code))
}

get_S4 <- function(name, field, type, file = paste0(name, "-Class.R"), append = TRUE) {
  length_field <- length(field)
  code <- c(
    "### Getteur ###\n",
    "setMethod(f = \"[\", signature = \"", name, "\", definition = function (x, i, j, drop){\n  switch(EXPR = i, \n",
    sapply(seq(length_field), function(i) {
      if (type[i] == "list") {
        paste0(
          "    \"", field[i], "\" = {\n      if (missing(j)) {\n        return(x@", field[i], ")\n      } else {\n",
          "        if (j>length(x@", field[i], ")) {\n          stop(\"[", name, ":get] indice out of limits\")\n        } else {\n",
          "          return(x@", field[i], "[[j]])\n        }\n      }\n    }, \n"
        )
      } else {
        if (type[i] == "matrix" | type[i] == "data.frame") {
          paste0("    \"", field[i], "\" = {return(x@", field[i], ")}, \n")
        } else {
          paste0(
            "    \"", field[i], "\" = {\n      if (missing(j)) {\n        return(x@", field[i], ")\n      } else {\n",
            "        if (j>length(x@", field[i], ")) {\n          stop(\"[", name, ":get] indice out of limits\")\n",
            "        } else {\n          return(x@", field[i], "[j])\n        }\n      }\n    }, \n"
          )
        }
      }
    }),
    "    stop(\"[", name, ":get] \", i, \" is not a \\\"", name, "\\\" slot\")\n  )\n})\n\n\n"
  )
  cat(code, sep = "", file = file, append = append)
  return(invisible(code))
}

set_S4 <- function(name, field, type, file = paste0(name, "-Class.R"), append = TRUE) {
  length_field <- length(field)
  code <- c(
    "### Setteur ###\n",
    "setMethod(f = \"[<-\", signature = \"", name, "\", definition = function (x, i, j, value){\n    switch(EXPR = i, \n",
    sapply(seq(length_field), function(i) {
      if (type[i] == "list") {
        paste0(
          "    \"", field[i], "\" = {\n      if (missing(j)) {\n        x@", field[i], " <- value\n      } else {\n",
          "        if (j>length(x@", field[i], ")) {\n          stop(\"[", name, ":set] indice out of limits\")\n",
          "        } else {\n          x@", field[i], "[[j]] <- value\n        }\n      }\n    }, \n"
        )
      } else {
        if (type[i] == "matrix" | type[i] == "data.frame") {
          paste0("    \"", field[i], "\" = {x@", field[i], " <- value}, \n")
        } else {
          paste0(
            "    \"", field[i], "\" = {\n      if (missing(j)) {\n        x@", field[i], " <- value\n      } else {\n",
            "        if (j>length(x@", field[i], ")) {\n          stop(\"[", name, ":set] indice out of limits\")\n",
            "        } else {\n          x@", field[i], "[j] <- value\n        }\n      }\n    }, \n"
          )
        }
      }
    }),
    "    stop(\"[", name, ":set] \", i, \" is not a \\\"", name, "\\\" slot\")\n",
    "  )\n  validObject(x)\n  return(invisible(x))\n})\n\n\n"
  )
  cat(code, sep = "", file = file, append = append)
  return(invisible(code))
}

summary_S4 <- function(name, file = paste0(name, "-Class.R"), append = TRUE) {
  code <- c(
    "### Summary ###\n",
    "setMethod(f = \"summary\", signature = \"", name, "\", definition = function (object){\n",
    "  if (missing(object)){\n",
    "    stop(\"[", name, ":summary] \\\"object\\\" is missing\", call. = FALSE)\n",
    "    return(invisible())\n",
    "  }\n",
    "  warning(\"[", name, ":summary] No summary method defined for \\\"", name, "\\\" object!\", call. = FALSE)\n",
    "  return(invisible(object))\n",
    "})\n"
  )
  cat(code, sep = "", file = file, append = append)
  return(invisible(code))
}

help_class_S4 <- function(class_name, file_name = NULL, where = topenv(parent.frame())) {
  type <- "class"
  keywords <- c("classes", "class", class_name)
  generator_name <- class_name
  .makeCallString <- function(def, name = substitute(def), args = formalArgs(def)) {
    if (is.character(def)) {
      if (missing(name)) {
        name <- def
      }
      def <- getFunction(def)
    }
    if (is(def, "function")) {
      paste0(name, "(", paste(args, collapse = ", "), ")")
    } else {
      ""
    }
  }
  .fileDesc <- function(file) {
    if (is.character(file)) {
      if (nzchar(file)) {
        paste(" to the file", sQuote(file))
      } else {
        " to the standard output connection"
      }
    } else {
      if (inherits(file, "connection")) {
        paste(" to the connection", sQuote(summary(file)$description))
      } else {
        ""
      }
    }
  }
  classInSig <- function(g, where, cl) {
    cl %in% unique(unlist(findMethods(g, where)@signatures))
  }
  genWithClass <- function(cl, where) {
    allgen <- getGenerics(where = where)
    ok <- as.logical(unlist(lapply(allgen, classInSig, cl = cl, where = where)))
    allgen[ok]
  }
  sigsList <- function(g, where) {
    methods <- findMethods(g, where)
    value <- methods@signatures
    args <- methods@arguments
    if (length(value)) {
      length(args) <- length(value[[1]])
      value <- lapply(value, function(x) {
        names(x) <- args
        x
      })
    }
    value
  }
  slotClassWithSource <- function(class_name) {
    clDef <- getClassDef(class_name)
    extds <- names(clDef@contains)
    allslots <- getSlots(clDef)
    for (j in rev(seq_along(extds))) {
      i <- extds[[j]]
      slotsi <- getSlots(getClass(i))
      if (length(slotsi)) {
        allslots[names(slotsi)] <- paste0("\"", as.character(slotsi), "\", from class \"", i, "\"")
      }
    }
    slotsi <- getSlots(clDef)
    if (length(slotsi)) {
      allslots[names(slotsi)] <- paste0("\"", as.character(slotsi), "\"")
    }
    allslots
  }
  cleanPrompt <- function(object, name) {
    value <- prompt(object, name = name, filename = NA)
    for (i in seq_along(value)) {
      item <- value[[i]]
      bad <- grepl("^ *%", item)
      if (any(bad)) {
        value[[i]] <- item[!bad]
      }
    }
    value
  }
  pastePar <- function(x) {
    xn <- names(x)
    x <- as.character(x)
    xn <- if (length(xn) == length(x)) {
      paste(xn, "= ")
    } else {
      ""
    }
    paste0("(", paste0(xn, "\"", x, "\"", collapse = ", "), ")")
  }
  escape <- function(txt) {
    gsub("%", "\\\\%", txt)
  }

  if (is.null(file_name)) {
    file_name <- paste0(utils:::topicName(type, class_name), ".Rd")
  }

  if (!missing(where) && !is.na(match(class_name, getClasses(where)))) {
    whereClass <- where
  } else {
    whereClass <- find(classMetaName(class_name))
    if (length(whereClass) == 0L) {
      stop(gettextf("no definition of class %s found", dQuote(class_name)), domain = NA)
    } else {
      if (length(whereClass) > 1L) {
        if (identical(where, topenv(parent.frame()))) {
          whereClass <- whereClass[[1L]]
          warning(gettextf("multiple definitions of %s found; using the one on %s", dQuote(class_name), whereClass), domain = NA)
        } else {
          if (exists(classMetaName(class_name), where, inherits = FALSE)) {
            whereClass <- where
          } else {
            stop(sprintf(
              ngettext(
                length(whereClass),
                "no definition of class %s in the specified position, %s, definition on : %s",
                "no definition of class %s in the specified position, %s, definitions on : %s"
              ),
              dQuote(class_name), where, paste(whereClass, collapse = ", ")
            ), domain = NA)
          }
        }
      }
    }
  }

  fullName <- utils:::topicName("class", class_name)
  clDef <- getClass(class_name, where = whereClass)
  .name <- paste0("\\name{", fullName, "}")
  .type <- paste0("\\docType{", type, "}")
  .alias <- paste0("\\alias{", fullName, "}")
  .title <- paste0("\n\  itle{Class \\code{\"", class_name, "\"}}")
  .desc <- paste0("\n\\description{", "\n%%  ~~ A concise (1-5 lines) description of what the class is. ~~\n}")
  slotclasses <- getSlots(clDef)
  slotnames <- names(slotclasses)
  slotclasses <- as.character(slotclasses)
  nslots <- length(slotclasses)
  class_nameQ <- paste0("\"", class_name, "\"")
  .usage <- "\n\\section{Objects from the Class}"
  virtualClass <- isVirtualClass(class_name)

  if (virtualClass) {
    .usage <- paste0(.usage, "{\nA virtual Class: No objects may be created from it.\n}")
    generator <- NULL
  } else {
    if (exists(generator_name, where, inherits = FALSE)) {
      generator <- get(generator_name, where, inherits = FALSE)
    } else {
      generator <- NULL
    }
    if (is(generator, "classGeneratorFunction")) {
      promptGenerator <- cleanPrompt(generator, generator_name)
      callString <- .makeCallString(generator, generator_name)
      .alias <- c(.alias, promptGenerator$aliases)
    } else {
      initMethod <- unRematchDefinition(selectMethod("initialize", class_name))
      argNames <- formalArgs(initMethod)
      argNames[[1L]] <- class_nameQ
      callString <- .makeCallString(initMethod, "new", argNames)
    }
    .usage <- paste0(.usage, "{\nObjects can be created by calls of the form \\code{", callString, "}.\n%%  ~~ describe objects here ~~ \n}")
  }

  .slots <- if (nslots > 0) {
    slotclasses <- slotClassWithSource(class_name)
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
  nmeths <- length(methnms <- genWithClass(class_name, where = whereClass))
  .meths.head <- "\n\\section{Methods}{"
  .methAliases <- ""

  if (nmeths > 0) {
    .meths.body <- "  \\describe{"
    for (i in 1L:nmeths) {
      .sig <- sigsList(methnms[i], where = whereClass)
      for (j in seq_along(.sig)) {
        if (!all(is.na(match(.sig[[j]], class_name)))) {
          methn.i <- escape(methnms[i])
          .meths.body <- c(.meths.body, paste0("    \\item{", methn.i, "}{\\code{signature", pastePar(.sig[[j]]), "}: ... }"))
          cur <- paste(.sig[[j]], collapse = ",")
          .methAliases <- paste0(.methAliases, "\\alias{", methn.i, ",", cur, "-method}\n")
        }
      }
    }
    .meths.body <- c(.meths.body, "  }")
  } else {
    .meths.head <- "\n\\section{Methods}{"
    .meths.body <- paste("No methods defined with class", class_nameQ, "in the signature.")
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
    examples = paste0("\n\\examples{\nshowClass(", class_nameQ, ")\n}"),
    keywords = .keywords
  )

  if (is(clDef, "refClassRepresentation")) {
    Rdtxt <- refClassPrompt(clDef, Rdtxt, nmeths, nslots, .meths.head)
  } else {
    if (is(generator, "classGeneratorFunction")) {
      what <- c("usage", "arguments")
      Rdtxt[what] <- promptGenerator[what]
    }
  }

  if (is.na(file_name)) {
    return(Rdtxt)
  }

  cat(unlist(Rdtxt), file = file_name, sep = "\n")
  # message("A shell of class documentation has been written", .fileDesc(file_name), ".\n")
  return(invisible(paste0(unlist(Rdtxt), collapse = "")))
}

help_method_S4 <- function(f, file_name = NULL, where = topenv(parent.frame())) {
  .requirePackage <- function(package, mustFind = TRUE) {
    value <- package

    if (nzchar(package)) {
      if (package %in% loadedNamespaces()) {
        value <- getNamespace(package)
      } else {
        if (identical(package, ".GlobalEnv")) {
          return(.GlobalEnv)
        }
        if (identical(package, "methods")) {
          return(topenv(parent.frame()))
        }
        if (exists(package, envir = .PackageEnvironments, inherits = FALSE)) {
          return(get(package, envir = .PackageEnvironments))
        }
      }
    }

    if (is.environment(value)) {
      return(value)
    }

    topEnv <- options()$topLevelEnvironment

    if (is.null(topEnv)) {
      topEnv <- .GlobalEnv
    }

    if (exists(".packageName", topEnv, inherits = TRUE) && .identC(package, get(".packageName", topEnv))) {
      return(topEnv)
    }

    if (!(nzchar(package) && require(package, character.only = TRUE))) {
      if (mustFind) {
        stop(gettextf("unable to find required package %s", sQuote(package)), domain = NA)
      } else {
        return(NULL)
      }
    }

    value <- .asEnvironmentPackage(package)
    assign(package, value, envir = .PackageEnvironments)
    return(value)
  }

  .genEnv <- function(f, default = .requirePackage("methods"), package = "") {
    if (!nzchar(package)) {
      package <- packageSlot(f)
    }
    if (is.null(package)) {
      value <- default
      def <- .getGeneric(f, value)
      if (is.null(def)) {
        value <- .GlobalEnv
        def <- .getGeneric(f, value)
        if (is.null(def)) {
          value <- .requirePackage("methods")
          if (!identical(default, value)) {
            def <- .getGeneric(f, value)
          }
        }
      }
      if (is.null(def)) {
        baseenv()
      } else {
        value
      }
    } else {
      .requirePackage(package)
    }
  }

  escape <- function(txt) {
    gsub("%", "\\\\%", txt)
  }
  packageString <- ""
  fdef <- getGeneric(f)

  if (!isGeneric(f, fdef = fdef)) {
    stop(gettextf("no generic function found corresponding to %s", sQuote(f)), domain = NA)
  }

  methods <- findMethods(fdef)
  where <- .genEnv(fdef, where)

  if (!identical(where, .GlobalEnv)) {
    packageString <- sprintf("in Package \\pkg{%s}", getPackageName(where))
  }

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

  if (identical(file_name, FALSE)) {
    return(c(aliasText, text))
  }

  if (is.null(file_name) || identical(file_name, TRUE)) {
    file_name <- paste0(fullName, ".Rd")
  }

  Rdtxt <- list(
    name = paste0("\\name{", fullName, "}"),
    type = "\\docType{methods}",
    aliases = aliasText,
    title = sprintf("\n\n\  itle{Methods for Function \\code{%s} %s}", f, packageString),
    description = paste0("\n\\description{\n%%  ~~ Methods for function", " \\code{", f, "} ", sub("^in Package", "in package", packageString), " ~~\n}"),
    `section{Methods}` = text,
    keywords = c("\n\\keyword{methods}", "\\keyword{method}", "\\keyword{new}", paste0("\\keyword{", f, "}"))
  )

  if (is.na(file_name)) {
    return(Rdtxt)
  }

  cat(unlist(Rdtxt), file = file_name, sep = "\n")
  # message("A shell of methods documentation has been written", .fileDesc(file_name), ".\n")
  return(invisible(paste0(unlist(Rdtxt), collapse = "")))
}

create_S4 <- function(name, field, type, file = paste0(name, "-class.R")) {
  .fileDesc <- function(file) {
    if (is.character(file)) {
      if (nzchar(file)) {
        paste(" to the file", sQuote(file))
      } else {
        " to the standard output connection"
      }
    } else {
      if (inherits(file, "connection")) {
        paste(" to the connection", sQuote(summary(file)$description))
      } else {
        ""
      }
    }
  }

  # dir.create(name)
  # directory_name <- paste0("./", name, "/")
  directory_name <- ""

  header_part <- header_S4(name, file = paste0(directory_name, file), append = FALSE)
  set_class_part <- set_class_S4(name, field, type, file = paste0(directory_name, file), append = TRUE)
  new_part <- new_S4(name, field, type, file = paste0(directory_name, file), append = TRUE)
  is_part <- is_S4(name, file = paste0(directory_name, file), append = TRUE)
  show_part <- show_S4(name, file = paste0(directory_name, file), append = TRUE)
  get_part <- get_S4(name, field, type, file = paste0(directory_name, file), append = TRUE)
  set_part <- set_S4(name, field, type, file = paste0(directory_name, file), append = TRUE)
  summary_part <- summary_S4(name, file = paste0(directory_name, file), append = TRUE)
  code <- paste0(
    c(header_part, set_class_part, new_part, is_part, show_part, get_part, set_part, summary_part), 
    collapse = ""
  )
  temp_envir <- new.env()
  eval(parse(text = code), envir = temp_envir)
  message(paste("A shell of class script has been written", .fileDesc(file = paste0(directory_name, file)), "."))

  # helpClass_part <- helpClass_S4(class_name = name, file_name = paste0(directory_name, name, "-class.Rd"), where = temp_envir)
  # message("A shell of class documentation has been written", .fileDesc(paste0(file = paste0(directory_name, file), "d")), ".")
  # helpMethod_part <- sapply(c(paste0("new_", name), paste0("is.", name)), function (f) {
  # helpMethod_part <- helpMethod_S4(f = f, file_name = paste0(directory_name, f, "-methods.Rd"), where = temp_envir)
  # message("A shell of methods documentation has been written", .fileDesc(paste0(directory_name, f, "-methods.Rd")), ".")
  # return(invisible(helpMethod_part))
  # })
  return(invisible(code))
}

### Run Test
# SLOT <- c("numeric", "list", "matrix", "data.frame", "integer", "character", "array")
# create_S4(name = "Generator", field = paste0("F", SLOT), type = SLOT)
