# @examples
# ggqqplot_old()
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pvalue PARAM_DESCRIPTION
#' @param lambdaNames PARAM_DESCRIPTION, Default: NULL
#' @param pt.size PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ggqqplot
#' @export
ggqqplot_old <- function (pvalue, lambdaNames = NULL, pt.size = 1) {
  if (!(is.matrix(pvalue) | is.data.frame(pvalue))) {
    pvalue <- matrix(pvalue)
  }
  
  if (is.null(lambdaNames)) {
    if ((any(colnames(pvalue) == "") | is.null(colnames(pvalue)))) {
      lambdaNames <- paste0("lambda", seq_len(ncol(pvalue)))
    } else {
      lambdaNames <- colnames(pvalue)
    }
  }
  
  res <- do.call("rbind", lapply(seq_len(ncol(pvalue)), function(i) {
    pv <- pvalue[, i]
    X2 <- qnorm(pv/2)^2
    gc <- median(X2, na.rm = TRUE)/qchisq(0.5, df = 1)
    obspval <- sort(pv)
    logobspval <- -(log10(obspval))
    exppval <- seq_along(obspval)
    logexppval <- -(log10((exppval - 0.5)/length(exppval)))
    labnames <- paste0(lambdaNames[i], "=", round(gc, 4))
    tmp <- data.frame(logexppval, logobspval, i, labnames)
    whichInfinite <- apply(tmp, 1, function (iRow) { return(any(is.infinite(as.numeric(iRow[-c(3, 4)])))) })
    return(tmp[!whichInfinite, ])
  }))
  res[, "i"] <- factor(res[, "i"], levels = unique(res[, "i"]))
  res[, "labnames"] <- factor(res[, "labnames"], levels = unique(res[, "labnames"]))
  
  
  axisLim <- range(
    pretty_breaks()(
      range(
        unlist(
          lapply(
            seq_len(ncol(pvalue)),
            function (i) {
              range(res[res[, "i"] == i, c(1, 2)])
            }
          ), 
          use.names = FALSE
        )
      )
    )
  )
  
  p <- ggplot(data = res) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(
      aes_string(x = "logexppval", y = "logobspval", colour = "labnames", shape = "labnames"), 
      size = pt.size
    ) +
    scale_colour_viridis(name = NULL, discrete = TRUE) +
    scale_shape_discrete(name = NULL, solid = FALSE) +
    labs(
      x = bquote(Expected -log[10](P[value])), 
      y = bquote(Observed - log[10](P[value])), 
      title = "Q-Q plot"
    ) + 
    xlim(axisLim) + 
    ylim(axisLim)
  return(p)
}


# @examples
# ggqqplot()
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pvalue PARAM_DESCRIPTION
#' @param col_names PARAM_DESCRIPTION, Default: NULL
#' @param point_size PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ggqqplot
#' @export
ggqqplot <- function (data, col_names = colnames(data), point_size = 1) {
  pval_trans <- function () {
    neglog10_breaks <- function (n = 5) {
      function (x) {
        rng <- -log(range(x, na.rm = TRUE), base = 10)
        min <- ceiling(rng[2])
        max <- floor(rng[1])
        if (max == min) {
          return(10^-min)
        } else {
          by <- floor((max - min)/n) + 1
          return(10^-seq(min, max, by = by))
        }
      }
    }
    trans_new(
      name = "pval", 
      transform = function (x) {-log(x, 10)}, 
      inverse = function (x) {10^-x}, 
      breaks = neglog10_breaks(), 
      domain = c(1e-100, Inf),
      format = function(x) {
        parse(
          text = scientific_format()(x) %>% 
            gsub("1e+00", "1", ., fixed = TRUE) %>% 
            gsub("e", " %*% 10^", .)
        )
      }
    )
  }
  
  if (is.null(ncol(data))) {
    data <- data.frame(X1 = data)
  }

  if (is.null(col_names)) {
    colnames(data) <- col_names
  }

  data %>% 
    as.data.frame() %>% 
    tidyr::gather(key = "group", value = "obspval") %>% 
    group_by(group) %>% 
    arrange(obspval) %>% 
    mutate(
      exppval = (seq_len(n()) - 0.5)/length(seq_len(n())),
      gc = median(qnorm(obspval/2)^2, na.rm = TRUE)/qchisq(0.5, df = 1),
      logobspval = -log10(obspval),
      logexppval = -log10(exppval)
    ) %>% 
    ungroup() %>% 
    mutate(
      labels = paste0("lambda[", group, "]==", round(gc, digits = 3)),
      labels = factor(labels, levels = unique(labels))
    ) %>% 
    ggplot() +
      geom_abline(intercept = 0, slope = 1) +
      geom_point(aes_string(x = "exppval", y = "obspval", colour = "labels", shape = "labels"), size = point_size) +
      scale_x_continuous(trans = pval_trans()) +
      scale_y_continuous(trans = pval_trans()) +
      scale_colour_viridis(name = NULL, discrete = TRUE, labels = parse_format()) +
      scale_shape_discrete(name = NULL, solid = FALSE, labels = parse_format()) +
      labs(
        x = "Expected P-value", 
        y = "Observed P-value", 
        title = "Q-Q plot"
      )
  }


# dta <- structure(list(P.Value = c(0.218407692890091, 0.130525279012434, 
# 0.683343144388117, 0.246780750782676, 0.0177984284258655, 0.00794396333344532, 
# 0.424582819202053, 0.0158087313777856, 0.950931737670156, 0.366425735714326, 
# 0.321704440283552, 0.392047323824355, 0.870146441330612, 0.238352275368997, 
# 0.242401075761186, 0.922258685427959, 0.0130760453443766, 0.288683426491957, 
# 0.135958675698331, 0.993024896922599, 0.0501851941122901, 0.618475566265418, 
# 0.695401840376323, 0.63527857415512, 0.406330891517864, 0.106066059822693, 
# 0.0640583894818549, 0.40807652830766, 0.0297068779722965, 0.161874136387928, 
# 0.350316519125907, 0.0920999691981586, 0.775353125614848, 0.018081443521426, 
# 0.350545552199862, 0.905715118428708, 0.0276001318036593, 0.641727326043442, 
# 0.034619298388489, 0.708919638081687, 0.0416846202039843, 0.20959576981185, 
# 0.167777686363245, 0.0134796130052804, 0.0414003151060316, 0.745560007129648, 
# 0.893934202384428, 0.0659484387214272, 0.225538455432154, 0.0103639785535232, 
# 0.580065523731895, 0.507330598020703, 0.54833146886504, 0.222800314850634, 
# 0.17162111032015, 0.442222282993156, 0.0183125429869003, 0.149999771676483, 
# 0.472893907495399, 0.943873439644904, 0.147561326685318, 0.528836137547885, 
# 0.176051861234689, 0.136431401107172, 0.0138732185844228, 0.643630981660745, 
# 0.893946031120356, 0.258621720595788, 0.928696157446891, 0.843620169697404, 
# 0.67414852836773, 0.99740533709626, 0.189100959709667, 0.00251124524041367, 
# 0.096415552603373, 0.671341027719868, 0.0933038866495186, 0.556706546125107, 
# 0.0379422098016142, 0.329242707453886, 0.00197603685238656, 0.445570451148947, 
# 0.184349084734725, 0.0531966884653242, 0.0902511024515039, 0.893175854880846, 
# 0.0886803234284303, 0.472123146496314, 0.213084881120064, 0.79572829420555, 
# 0.101453788995734, 0.177333204739506, 0.00907031897270732, 0.00929272537554209, 
# 0.0928463832064664, 0.0171196536169356, 0.12336340673638, 0.766126287705065, 
# 0.0335536645143222, 0.741112384982528), adj.P.Val = c(0.478174813333279, 
# 0.379168515224653, 0.835712421632489, 0.506195426391641, 0.194317892395495, 
# 0.164798352577968, 0.65966978924227, 0.188941486288489, 0.97837557633262, 
# 0.613218872210222, 0.575046504009115, 0.634153945246229, 0.939280222248596, 
# 0.497982868317602, 0.501967856374283, 0.96493410803311, 0.180687989711587, 
# 0.54562942101058, 0.385888004569138, 0.99702229503438, 0.261641288840669, 
# 0.795743342987562, 0.84294154530878, 0.806369100306266, 0.645382430191038, 
# 0.347410028153252, 0.285411946058119, 0.646735402444672, 0.222355072825229, 
# 0.416715196930641, 0.599760250069894, 0.328101914057783, 0.888927341218152, 
# 0.19505853735881, 0.599932881555821, 0.956961507687173, 0.217839369392412, 
# 0.810300082372563, 0.232457610342254, 0.851048164723106, 0.246189046910296, 
# 0.469210929104593, 0.423424769711279, 0.18191557273121, 0.245665483797174, 
# 0.872207767113563, 0.951070541494584, 0.288569067093628, 0.485449731127622, 
# 0.172528594845106, 0.770771365723838, 0.720871213779243, 0.749383007633671, 
# 0.482717117784595, 0.427762355777471, 0.673242231322025, 0.195616506518972, 
# 0.402780718387987, 0.696200273289968, 0.975099114874269, 0.399791644863781, 
# 0.736043550915602, 0.432758243104638, 0.386443361492906, 0.183253471906897, 
# 0.811513241199141, 0.951077166109449, 0.517684456502085, 0.967988507277119, 
# 0.925649676364915, 0.830202509467836, 0.998967936831103, 0.447237985955388, 
# 0.141133583403223, 0.334104893229416, 0.828515341324999, 0.329736570773039, 
# 0.755104869146845, 0.239001355342663, 0.581625066838609, 0.137114710914828, 
# 0.675697329232817, 0.442093934898134, 0.266981669503252, 0.325554202653125, 
# 0.950716209429489, 0.323295968616416, 0.695587097775052, 0.472776097723021, 
# 0.900224709701643, 0.341113518367685, 0.434142878001448, 0.168620324817731, 
# 0.169530221518836, 0.32914895678025, 0.192540846711352, 0.369981675410129, 
# 0.883757071864581, 0.230327033896926, 0.869679377611833)), .Names = c("P.Value", 
# "adj.P.Val"), row.names = c(NA, -100L), class = "data.frame")
# cowplot::plot_grid(
#   ggqqplot2(dta),
#   ggqqplot(dta)
# )
