#' Get probabilities of stock membership for humpback whales
#'
#' \code{get_mnstockprops} provides probabilities of membership in each of four
#' humpback whale stocks, and associated uncertainties, given minimum and maximum
#' latitude(s) along the U.S. West Coast for individual whales. It can
#' provide probabilities for individual whales or averaged across whales.
#'
#' @param x Data frame containing four columns and one row per whale:
#' minimum and maximum latitudes of occurrence (two numeric columns), season of
#' occurrence (character, \code{"summer"} (Jun-Oct) or \code{"all"}) (all months),
#' and a proration value from serious injury determination (number between zero
#' and one; set to all ones if not relevant). Note that the function rounds all
#' latitudes to 0.1 degrees precision.
#' @param props.ind Logical value indicating whether to return probabilities for
#' for individual whales or for the total number of whales (must be \code{FALSE}
#' to get appropriate uncertainties for the total). Defaults to \code{FALSE}.
#'
#' @details
#' \code{get_mnstockprops} provides expected probabilities of stock membership
#' of humpback whales off the U.S. West Coast (as delineated under the U.S. MMPA),
#' given a range of potential latitudes. Probabilities can be returned at either
#' the individual level (for each set of minimum and maximum latitudes provided)
#' or as an overall average that can be applied to the total number of whales.
#' Uncertainty is derived from Monte Carlo simulations, so uncertainty for mean
#' probabilities across whales cannot be calculated post hoc from statistics for
#' individuals.
#'
#' Estimated proportions are provided for four stocks (with abbreviations used in
#' the returned data frame):
#'   * Central America / Southern Mexico - California-Oregon-Washington (CASM)
#'   * Mainland Mexico - California-Oregon-Washington (MX-COW)
#'   * Hawai'i (HI)
#'   * Mexico-North Pacific (MX-NP)
#'
#' The stock membership probabilities returned are based on stock proportions by
#' latitude, estimated for discrete latitude bins following the methods reported
#' in Curtis et al. (2025). Probabilities across multiple bins are calculated
#' as weighted averages using relative densities along the U.S. West Coast from
#' Becker et al. (2020) and in the Salish Sea from Wright et al. (2021). The
#' proportions and uncertainties applied here currently match those reported
#' in Curtis et al. (2025).
#'
#' Minimum and maximum latitudes accepted by \code{get_mnstockprops} are 30.5 and
#' 49, which map to the southernmost and northernmost extents of the U.S. West
#' Coast EEZ, including the Salish Sea. To limit proportions to the outer coast,
#' set maximum latitude less than or equal to 48.5. The Salish Sea can be
#' included by setting the maximum latitude to 49, and can be subset by setting
#' both minimum and maximum latitudes to 49 (note that proportions and relative
#' densities for the Salish Sea do include waters south of 48.5 too!). Latitude
#' limits between 48.5 and 49 will be rejected. Additional useful reference
#' latitudes: CA/OR border = 42, OR/WA border = 46.3.
#'
#' Use of the following object names in your workspace while using this package
#' will disable its functionality and could lead to erroneous answers and unexpected
#' warnings or error messages: \code{lxi}, \code{lxi.mc}, and \code{mn.n.line}.
#'
#'
#' @references
#'
#' Becker, E.A., Forney, K.A., Miller, D.L., Fiedler, P.C., Barlow, J., Moore,
#' J.E.. 2020. Habitat-based density estimates for cetaceans in the California
#' Current Ecosystem based on 1991-2018 survey data. NOAA Technical Memorandum
#' NMFS-SWFSC-638.
#'
#' Curtis, K. A., Calambokidis, J., and others (2025) Harnessing the power of 
#' photo-ID data for apportionment to migratory whale herds: U.S. West Coast 
#' humpback whale stock proportions by latitude for the period 2019-2024. NOAA 
#' Technical Memorandum NMFS-SWFSC-XXX.
#'
#' Wright, B. M., Nichol, L. M., Doniol-Valcroze, T. 2021. Spatial density models
#' of cetaceans in the Canadian Pacific estimated from 2018 ship-based surveys.
#' DFO Can. Sci. Advis. Sec. Res. Doc. 2021/049. viii + 46 p. (Erratum: March 2022)
#'
#' @returns A data frame with the following columns:
#'   \item{id}{Included only if \code{props.ind = TRUE}: Sequential integers
#'             corresponding to each input row (i.e., each whale).}
#'   \item{minlat}{Included only if \code{props.ind = TRUE}.}
#'   \item{maxlat}{Included only if \code{props.ind = TRUE}.}
#'   \item{ssn}{Included only if \code{props.ind = TRUE}.}
#'   \item{sipv}{Included only if \code{props.ind = TRUE}.}
#'   \item{stock}{Stock (using abbreviations provided in Details).}
#'   \item{prop}{Mean proportion.}
#'   \item{sd}{Standard deviation of proportion from simulation.}
#'   \item{q95l}{2.5th percentile of proportion from simulation.}
#'   \item{q80l}{10th percentile of proportion from simulation.}
#'   \item{q80u}{90th percentile of proportion from simulation.}
#'   \item{q95u}{97.5th percentile of proportion from simulation.}
#'
#' @examples
#' whale.locs <- data.frame(minlat = c(34.5, 46), maxlat = c(40, 48.5),
#'                          ssn = c("all", "all"), sipv = c(1, 1))
#' get_mnstockprops(whale.locs)
#' get_mnstockprops(whale.locs, props.ind = TRUE)
#'
#' @export
get_mnstockprops <- function(x, props.ind=FALSE) {

  # check data inputs
  if (!is.logical(props.ind))
    stop("props.ind must be of type logical.")
  if (!is.data.frame(x))
    stop("x must be a data frame.")
  if (dim(x)[2] != 4)
    stop("x must have four columns.")
  if (dim(x)[1]==0)
    stop("x must have one or more rows.")
  if (any(is.na(x)))
    stop("No missing values allowed in x.")
  if (!is.numeric(unlist(x[,c(1:2,4)])) | !is.character(unlist(x[,3])))
    stop(paste0("Columns for x must be as follows: minimum and maximum possible ",
         "latitudes of occurrence, season, and proration values. See ",
         "?get_mnstockprops for more information."))
  lats <- unlist(x[,1:2])
  if (!all((lats>=30.5) & (lats<=49) & (lats<=48.5 | lats==49)))
    stop(paste0("Latitude limits must be between 30.5 and 49, and can't include ",
         "48.5 < lat < 49."))
  if (!all(x[,1] <= x[,2]))
    stop(paste0("Minimum latitude (column 1) must be less or equal to maximum ",
         "latitude (column 2)."))
  if (!all(x[,3] %in% c("summer", "all")))
    stop("Season must be 'all' or 'summer'.")
  if (!all(x[,4] >= 0 & x[,4] <= 1))
    stop("Proration values must be a number between zero and one.")

  # edit data frame
  names(x) <- c("minlat", "maxlat", "ssn", "sipv")
  x$id <- 1:nrow(x)
  x$minlat <- round(x$minlat, 1)
  x$maxlat <- round(x$maxlat, 1)

  # calculate stock proportions per sim per whale
  ## (could gain efficiency if group individuals by shared minlat and maxlat first)
  ni <- nrow(x)
  nstock <- length(unique(lxi$stock))
  nsim <- max(lxi.mc$sim)
  propdist.each <- data.frame(id=rep(x$id, each=nstock*nsim),
                              minlat=rep(x$minlat, each=nstock*nsim),
                              maxlat=rep(x$maxlat, each=nstock*nsim),
                              ssn=rep(x$ssn, each=nstock*nsim),
                              sipv=rep(x$sipv, each=nstock*nsim),
                              stock=rep(NA, ni*nstock*nsim),
                              sim=rep(NA, ni*nstock*nsim),
                              prop=rep(NA, ni*nstock*nsim))
  propx.each <- data.frame(id=rep(x$id, each=nstock),
                           minlat=rep(x$minlat, each=nstock),
                           maxlat=rep(x$maxlat, each=nstock),
                           ssn=rep(x$ssn, each=nstock),
                           sipv=rep(x$sipv, each=nstock),
                           stock=rep(NA, ni*nstock),
                           prop=rep(NA, ni*nstock))
  for (i in 1:ni) {
    disti <- (i*nstock*nsim-nstock*nsim+1):(i*nstock*nsim)
    xi <- (i*nstock-nstock+1):(i*nstock)
    propdist.each[disti, 6:ncol(propdist.each)] <-
      get_propsperwhale(x[i,c("minlat","maxlat","ssn")], stats="dist")
    propx.each[xi, 6:ncol(propx.each)] <-
      get_propsperwhale(x[i,c("minlat","maxlat","ssn")], stats="point")
  }

  # summarize props by stock across sims by whale or across whales
  if(props.ind) {
    # summarize props by whale and stock across sims, merge with point estim, and return
    propstats.each <- stats::aggregate(prop ~ id + stock, data=propdist.each,
                                FUN = function(x) c(stats::sd(x), stats::quantile(x, c(0.025,0.1,0.9,0.975))))
    propstats.each <- cbind(propstats.each[,1:2], propstats.each$prop)
    names(propstats.each) <- c("id","stock","sd","q95l","q80l","q80u","q95u")
    propstats.each <- merge(propx.each, propstats.each)
    # round to appropriate precision for smaller proportions
    propstats.each[,c("prop","sd","q95l","q80l","q80u","q95u")] <-
      round(propstats.each[,c("prop","sd","q95l","q80l","q80u","q95u")],4)
    # return result
    return(propstats.each[,c(names(propx.each),"sd","q95l","q80l","q80u","q95u")])
  } else {
    # objective is to return four-row df with stockwise stats for all whales combined,
    # (accounting for sipv)
    ## calculate weighted (by sipv) average of props across whales for point estim
    propx.each$prop.wtd <- propx.each$sipv * propx.each$prop
    propx.sum <- stats::aggregate(cbind(sipv, prop.wtd) ~ stock, data=propx.each, FUN=sum)
    propx.sum$prop <- propx.sum$prop.wtd/propx.sum$sipv
    ## calculate weighted (by sipv) average of props across whales within sim
    propdist.each$prop.wtd <- propdist.each$sipv * propdist.each$prop
    propdist.sum <- stats::aggregate(cbind(sipv, prop.wtd) ~ stock + sim,
                                     data=propdist.each, FUN=sum)
    propdist.sum$propsum.std <- propdist.sum$prop.wtd/propdist.sum$sipv
    ## then get stats across sims
    propstats.sum <- stats::aggregate(propsum.std ~ stock, data=propdist.sum,
                               FUN = function(x) c(stats::sd(x), stats::quantile(x, c(0.025,0.1,0.9,0.975))))
    propstats.sum <- cbind(propstats.sum[,1,drop=FALSE], propstats.sum$propsum.std)
    names(propstats.sum) <- c("stock","sd","q95l","q80l","q80u","q95u")
    propstats.sum <- merge(propx.sum[,c(1,4)], propstats.sum)
    # round to appropriate precision for smaller proportions
    propstats.sum[,c("prop","sd","q95l","q80l","q80u","q95u")] <-
      round(propstats.sum[,c("prop","sd","q95l","q80l","q80u","q95u")],4)
    # return result
    return(propstats.sum)
  }
}

# Hidden function used by get_mnstockprops
# df contains the fields minlat, maxlat, and ssn
# stats can be "dist" or "point" to return distribution from MCMC or point estimate
get_propsperwhale <- function(df, stats="dist"){
  dentemp <- mn.n.line[mn.n.line$mlat >= df$minlat & mn.n.line$mlat <= df$maxlat,]
  densum <- stats::aggregate(n.mn~latbin, data=dentemp, FUN=sum)
  densum$ssn <- df$ssn
  # merge with lxi or lxi.mc
  if (stats=="point") {
    den.lxi <- merge(densum, lxi)
    # get weighted average and restandardize across bins
    den.lxi$lxi.wtd <- den.lxi$n.mn * den.lxi$lxi.c.std
    stock.lxi <- stats::aggregate(cbind(n.mn,lxi.wtd) ~ stock, data=den.lxi, FUN=sum)
    stock.lxi$lxi.std <- stock.lxi$lxi.wtd/stock.lxi$n.mn
    return(stock.lxi[,c("stock","lxi.std")])
  } else {
    # # add 0.118 CV to n.mn (if >1 bin) RESERVE FOR APPROPRIATE ASSESSMENT OF DENSITY UNCERTAINTY
    # if (nrow(densum)>1) {
    #   nsim <- max(lxi.mc$sim)
    #   densum <- data.frame(latbin=rep(densum$latbin, each=nsim),
    #                        n.mn=pmax(0, rnorm(nrow(densum)*nsim, mean=rep(densum$n.mn, each=nsim), sd=0.118*rep(densum$n.mn, each=nsim))),
    #                        sim=rep(1:nsim, nrow(densum)),
    #                        ssn=rep(densum$ssn,each=nsim))
    # }
    den.lxi.mc <- merge(densum, lxi.mc)
    # get weighted average and restandardize across bins within sims
    den.lxi.mc$lxi.wtd <- den.lxi.mc$n.mn * den.lxi.mc$lxi.c.std
    stock.lxi <- stats::aggregate(cbind(n.mn,lxi.wtd) ~ stock + sim, data=den.lxi.mc, FUN=sum)
    stock.lxi$lxi.std <- stock.lxi$lxi.wtd/stock.lxi$n.mn
    return(stock.lxi[,c("stock","sim","lxi.std")])
  }
}
