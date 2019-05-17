plot.fevd.mle <- function (x, type = c("primary", "probprob", "qq", "qq2", "Zplot", 
    "hist", "density", "rl", "trace"), rperiods = c(2, 5, 10, 
    20, 50, 80, 100, 120, 200, 250, 300, 500, 800), a = 0, hist.args = NULL, 
    density.args = NULL, period = "year", prange = NULL, d = NULL, 
    ...) 
{
    type <- match.arg(type)
    model <- x$type
    pars <- x$results$par
    args <- list(...)
    ytmp <- datagrabber(x)
    if (x$data.name[2] != "") 
        data <- ytmp[, -1]
    else data <- NULL
    y <- c(ytmp[, 1])
    const.thresh <- check.constant(x$par.models$threshold)
    const.loc <- check.constant(x$par.models$location)
    const.scale <- check.constant(x$par.models$scale)
    const.shape <- check.constant(x$par.models$shape)
    if (is.element(model, c("PP", "GP", "Exponential", "Beta", 
        "Pareto")) && !const.thresh && all(c(const.loc, const.scale, 
        const.shape)) && type == "rl") 
        stop("plot.fevd: invalid type argument for POT models with varying thresholds but constant parameters (are you sure about this model choice?).")
    tform <- !is.fixedfevd(x)
    if (tform) {
        ytrans <- trans(x)
        if (model == "PP" && is.element(type, c("primary", "hist", 
            "density"))) 
            ytransPP <- trans(x, return.all = TRUE)
    }
    if (!tform) {
        if (!is.element(model, c("GP", "Beta", "Pareto", "Exponential"))) 
            loc <- pars["location"]
        else loc <- NULL
        if (is.element("scale", names(pars))) 
            scale <- pars["scale"]
        else scale <- exp(pars["log.scale"])
        if (!is.element(model, c("Gumbel", "Exponential"))) 
            shape <- pars["shape"]
        else shape <- 0
    }
    else if (is.element(type, c("primary", "rl"))) 
        if (missing(rperiods)) 
            rperiods <- c(2, 20, 100)
    npy <- x$npy
    if (is.element(model, c("PP", "GP", "Beta", "Pareto", "Exponential"))) {
        u <- x$threshold
        eid <- y > u
        lam <- mean(eid)
    }
    else u <- lam <- NULL
    if (is.element(type, c("primary", "probprob", "qq", "rl"))) {
        if (is.element(model, c("GP", "PP", "Beta", "Pareto", 
            "Exponential"))) {
            if (!tform) 
                n <- sum(y > x$threshold)
            else n <- sum(!is.na(ytrans) & !is.nan(ytrans))
        }
        else n <- x$n
        
        # ## xxx
        # print(paste("x$n: ", x$n))
        
        ## xxx here is where return periods are calculated
        xp <- ppoints(n = n, a = a)
        # print(paste("n: ", n))
        # print(paste("a: ", a))
        # print("xp:")
        # print(xp)
    }
    if (type == "primary") {
        op <- par()
        if (model != "PP") 
            par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
        else if (!tform) 
            par(mfrow = c(3, 2), oma = c(0, 0, 2, 0))
        else par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
    }
    if (type == "probprob") {
        if (is.null(args$main)) 
            m1 <- deparse(x$call)
        if (!tform) {
            if (is.element(model, c("PP", "GP", "Beta", "Pareto", 
                "Exponential"))) {
                yp <- pevd(sort(y[eid]), loc = loc, scale = scale, 
                  shape = shape, threshold = u, npy = npy, type = model)
            }
            else yp <- pevd(sort(y), loc = loc, scale = scale, 
                shape = shape, threshold = u, npy = npy, type = model)
            if (is.null(args$main)) 
                plot(xp, yp, main = m1, xlab = "Empirical Probabilities", 
                  ylab = "Model Probabilities", ...)
            else plot(xp, yp, xlab = "Empirical Probabilities", 
                ylab = "Model Probabilities", ...)
            abline(0, 1)
        }
        else {
            if (is.element(model, c("GEV", "Gumbel", "Weibull", 
                "Frechet"))) 
                yp <- exp(-exp(-sort(ytrans)))
            else if (model == "PP") 
                yp <- sort(ytrans)
            else yp <- 1 - exp(-sort(ytrans))
            if (is.null(args$main)) {
                plot(xp, yp, main = m1, xlab = "Residual Empirical Probabilities", 
                  ylab = "Residual Model Probabilities", ...)
            }
            else plot(xp, yp, xlab = "Residual Empirical Probabilities", 
                ylab = "Residual Model Probabilities", ...)
            abline(0, 1)
        }
        out <- data.frame(empirical = xp, model = yp)
    }
    if (is.element(type, c("primary", "qq"))) {
        if (!tform) {
            if (is.element(model, c("Weibull", "Frechet"))) 
                mod2 <- "GEV"
            else if (is.element(model, c("Exponential", "Beta", 
                "Pareto"))) 
                mod2 <- "GP"
            else mod2 <- model
            if (!is.element(model, c("PP", "GP", "Beta", "Pareto", 
                "Exponential"))) 
                yq <- qevd(xp, loc = loc, scale = scale, shape = shape, 
                  type = mod2)
            else yq <- qevd(xp, threshold = u, loc = loc, scale = scale, 
                shape = shape, type = mod2)
            if (is.null(args$main)) {
                if (type == "primary") 
                  m2 <- ""
                else m2 <- deparse(x$call)
                if (is.element(model, c("GEV", "Weibull", "Frechet", 
                  "Gumbel"))) {
                  plot(yq, sort(y), xlab = "Model Quantiles", 
                    ylab = "Empirical Quantiles", main = m2)
                  if (type == "qq") 
                    out <- data.frame(empirical = sort(y), model = yq)
                }
                else {
                  plot(yq, sort(y[eid]), xlab = "Model Quantiles", 
                    ylab = "Empirical Quantiles", main = m2, 
                    ...)
                  if (type == "qq") 
                    out <- data.frame(empirical = sort(y[eid]), 
                      model = yq)
                }
            }
            else {
                if (is.element(model, c("GEV", "Weibull", "Frechet", 
                  "Gumbel"))) {
                  plot(yq, sort(y), xlab = "Model Quantiles", 
                    ylab = "Empirical Quantiles", ...)
                  if (type == "qq") 
                    out <- data.frame(empirical = sort(y), model = yq)
                }
                else {
                  plot(yq, sort(y[eid]), xlab = "Model Quantiles", 
                    ylab = "Empirical Quantiles", ...)
                  if (type == "qq") 
                    out <- data.frame(empirical = sort(y[eid]), 
                      model = yq)
                }
            }
        }
        else {
            if (is.null(args$main)) {
                if (type == "primary") 
                  m2 <- ""
                else m2 <- deparse(x$call)
                if (is.element(model, c("GEV", "Weibull", "Frechet"))) 
                  m2 <- paste(m2, "(Gumbel Scale)", sep = "\n")
                else if (is.element(model, c("PP", "GP", "Beta", 
                  "Pareto"))) 
                  m2 <- paste(m2, "Exponential Scale", sep = "\n")
                if (is.element(model, c("GEV", "Weibull", "Gumbel", 
                  "Frechet"))) {
                  plot(-log(-log(sort(xp))), sort(ytrans), main = m2, 
                    xlab = "(Standardized) Model Quantiles", 
                    ylab = "Empirical Residual Quantiles", ...)
                  if (type == "qq") 
                    out <- data.frame(empirical = sort(ytrans), 
                      model = -log(-log(sort(xp))))
                }
                else if (is.element(model, c("GP", "Beta", "Exponential", 
                  "Pareto"))) {
                  plot(-log(1 - xp), sort(ytrans), main = m2, 
                    xlab = "(Standardized) Residual Quantiles", 
                    ylab = "Empirical Residual Quantiles", ...)
                  if (type == "qq") 
                    out <- data.frame(empirical = sort(ytrans), 
                      model = -log(1 - xp))
                }
                else if (model == "PP") {
                  plot(-log(1 - xp), sort(-log(ytrans)), main = m2, 
                    xlab = "(Standardized) Residual Quantiles", 
                    ylab = "Empirical Residual Quantiles", ...)
                  if (type == "qq") 
                    out <- data.frame(empirical = sort(-log(ytrans)), 
                      model = -log(1 - xp))
                }
            }
            else {
                if (is.element(model, c("GEV", "Weibull", "Gumbel", 
                  "Frechet"))) {
                  plot(-log(-log(sort(xp))), sort(ytrans), xlab = "Model", 
                    ylab = "Empirical", ...)
                  if (type == "qq") 
                    out <- data.frame(empirical = sort(ytrans), 
                      model = -log(-log(sort(xp))))
                }
                else if (is.element(model, c("GP", "Beta", "Exponential", 
                  "Pareto"))) {
                  plot(-log(1 - xp), sort(ytrans), xlab = "Model", 
                    ylab = "Empirical", ...)
                  if (type == "qq") 
                    out <- data.frame(empirical = sort(ytrans), 
                      model = -log(1 - xp))
                }
                else if (model == "PP") {
                  plot(-log(1 - xp), sort(-log(ytrans)), xlab = "Model", 
                    ylab = "Empirical", ...)
                  if (type == "qq") 
                    out <- data.frame(empirical = sort(-log(ytrans)), 
                      model = -log(1 - xp))
                }
            }
        }
        abline(0, 1)
    }
    if (is.element(type, c("primary", "qq2"))) {
        if (is.fixedfevd(x) && !is.null(x$blocks)) {
            z <- rextRemes(x, round(x$blocks$nBlocks * x$npy))
        }
        else {
            if (!is.element(model, c("PP", "GP", "Beta", "Exponential", 
                "Pareto"))) 
                z <- rextRemes(x)
            else z <- rextRemes(x, n = sum(eid, na.rm = TRUE))
        }
        if (!is.element(model, c("PP", "GP", "Beta", "Exponential", 
            "Pareto"))) {
            yQQ <- y
            if (is.null(args$xlab)) 
                xl <- paste(x$data.name[1], " Empirical Quantiles", 
                  sep = "")
            else xl <- args$xlab
        }
        else {
            yQQ <- y[eid]
            if (is.null(args$xlab)) {
                if (length(x$threshold) == 1) 
                  xl <- paste(x$data.name[1], "( > ", x$threshold, 
                    ") Empirical Quantiles", sep = "")
                else xl <- paste(x$data.name[1], "( > threshold) Empirical Quantiles", 
                  sep = "")
            }
            else xl <- args$xlab
        }
        if (!is.null(args$main)) {
            if (type == "primary") 
                mQQ <- ""
            else mQQ <- deparse(x$call)
            if (type != "qq2") 
                qqplot(yQQ, z, main = mQQ, xlab = xl, ylab = "Quantiles from Model Simulated Data")
            else out <- qqplot(yQQ, z, main = mQQ, xlab = xl, 
                ylab = "Quantiles from Model Simulated Data")
        }
        else {
            if (type != "qq2") 
                qqplot(yQQ, z, xlab = xl, ylab = "Quantiles from Model Simulated Data")
            else out <- qqplot(yQQ, z, xlab = xl, ylab = "Quantiles from Model Simulated Data")
        }
    }
    if (!(type == "primary" && model == "PP" && tform)) {
        if (is.element(type, c("primary", "density")) && is.null(x$blocks)) {
            if (!tform) {
                if (!is.element(model, c("PP", "GP", "Beta", 
                  "Exponential", "Pareto"))) 
                  yd <- y
                else if (model != "PP") 
                  yd <- y[eid] - x$threshold
                else {
                  if (x$span%%1 != 0 || x$npy%%1 != 0) 
                    warning("plot.fevd.mle: span or npy not integers; determination of max in each block may be substantially in error when there are many blocks.")
                  blocks <- rep(1:round(x$span), each = round(x$npy))
                  n2 <- length(blocks)
                  if (n2 < x$n) 
                    blocks <- c(blocks, rep(blocks[n2], x$n - 
                      n2))
                  else if (n2 > x$n) 
                    blocks <- blocks[1:x$n]
                  yd <- c(aggregate(y, by = list(blocks), max)$x)
                }
                if (is.null(density.args)) 
                  yd <- density(yd)
                else yd <- do.call("density", c(list(yd), density.args))
                if (is.element(type, c("primary", "density"))) 
                  xd <- seq(min(yd$x, na.rm = TRUE), max(yd$x, 
                    na.rm = TRUE), , 100)
                else xd <- seq(min(yh, na.rm = TRUE), max(yh, 
                  na.rm = TRUE), , 100)
                if (is.element(model, c("PP", "Gumbel", "Weibull", 
                  "Frechet"))) 
                  mod2 <- "GEV"
                else if (is.element(model, c("Pareto", "Frechet", 
                  "Beta", "Exponential"))) 
                  mod2 <- "GP"
                else mod2 <- model
                if (tform) {
                  mu <- 0
                  sig <- 1
                  xi <- 0
                  u2 <- 0
                }
                else {
                  mu <- loc
                  sig <- scale
                  xi <- shape
                  u2 <- u[1]
                }
                yd2 <- devd(xd, loc = mu, scale = sig, shape = xi, 
                  threshold = u2, type = mod2)
                if (is.null(args$ylim)) {
                  yld <- range(c(yd$y, yd2), finite = TRUE)
                  yld[1] <- min(yld[1], 0)
                }
                if (is.null(args$main)) {
                  if (type == "primary") 
                    m3 <- ""
                  else m3 <- deparse(x$call)
                  if (is.null(args$ylim)) 
                    plot(yd, main = m3, ylim = yld, ...)
                  else plot(yd, main = m3, ...)
                }
                else {
                  if (is.null(args$ylim)) 
                    plot(yd, ylim = yld, ...)
                  else plot(yd, ...)
                }
            }
            else {
                if (type == "density" && is.element(model, c("PP", 
                  "GP", "Exponential", "Pareto", "Beta"))) 
                  stop("plot.fevd: invalid type argument for this model.")
                if (!is.element(model, c("PP", "GP", "Exponential", 
                  "Pareto", "Beta"))) {
                  if (is.null(density.args)) 
                    yd <- density(ytrans)
                  else yd <- do.call("density", c(list(ytrans), 
                    density.args))
                  if (is.null(args$ylim)) {
                    yld <- range(yd$y, finite = TRUE)
                    yld[1] <- min(yld[1], 0)
                  }
                  if (is.null(args$main)) {
                    if (type == "primary") 
                      m3 <- "Transformed Data"
                    else m3 <- paste(deparse(x$call), "Transformed Data", 
                      sep = "\n")
                    if (is.null(args$ylim)) 
                      plot(yd, main = m3, ylim = yld, ...)
                    else plot(yd, main = m3, ...)
                  }
                  else {
                    if (is.null(args$ylim)) 
                      plot(yd, ylim = yld, ...)
                    else plot(yd, ...)
                  }
                }
            }
            if (type == "density") 
                out <- yd
        }
    }
    if (type == "hist" && is.null(x$blocks)) {
        if (!tform) {
            if (is.null(args$main)) {
                if (model != "PP") 
                  m4 <- paste(deparse(x$call), "Histogram", sep = "\n")
                else m4 <- paste(deparse(x$call), "\n", paste("Histogram (", 
                  x$period.basis, " maxima)", sep = ""))
            }
            if (is.element(model, c("GP", "Beta", "Exponential", 
                "Pareto"))) 
                yh <- y[eid] - x$threshold
            else if (model != "PP") 
                yh <- y
            else {
                blocks <- rep(1:x$span, each = x$npy)
                n2 <- length(blocks)
                if (n2 < x$n) 
                  blocks <- c(blocks, rep(blocks[n2], x$n - n2))
                else if (n2 > x$n) 
                  blocks <- blocks[1:x$n]
                yh <- c(aggregate(y, by = list(blocks), max)$x)
            }
        }
        else {
            if (is.element(model, c("PP", "GP", "Exponential", 
                "Beta", "Pareto"))) 
                stop("plot.fevd: invalid type argument for this model.")
            if (is.null(args$main)) 
                m4 <- paste(deparse(x$call), "Histogram of Transformed Data", 
                  sep = "\n")
        }
        if (is.null(hist.args)) {
            if (!is.null(args$ylim)) {
                if (is.null(args$main)) {
                  if (is.null(args$col)) {
                    out <- hist(yh, col = "darkblue", freq = FALSE, 
                      breaks = "FD", xlab = x$data.name[1], main = m4, 
                      ...)
                  }
                  else out <- hist(yh, freq = FALSE, breaks = "FD", 
                    main = m4, xlab = x$data.name[1], ...)
                }
                else {
                  if (is.null(args$col)) 
                    out <- hist(yh, col = "darkblue", freq = FALSE, 
                      breaks = "FD", xlab = x$data.name[1], ...)
                  else out <- hist(yh, freq = FALSE, breaks = "FD", 
                    xlab = x$data.name[1], ...)
                }
            }
            else {
                if (is.null(args$main)) {
                  if (is.null(args$col)) 
                    out <- hist(yh, col = "darkblue", freq = FALSE, 
                      breaks = "FD", xlab = x$data.name[1], main = m4, 
                      ylim = c(0, 1.5), ...)
                  else out <- hist(yh, freq = FALSE, breaks = "FD", 
                    main = m4, xlab = x$data.name[1], ylim = c(0, 
                      1.5), ...)
                }
                else {
                  if (is.null(args$col)) 
                    out <- hist(yh, col = "darkblue", freq = FALSE, 
                      breaks = "FD", xlab = x$data.name[1], ylim = c(0, 
                        1.5), ...)
                  else out <- hist(yh, freq = FALSE, breaks = "FD", 
                    xlab = x$data.name[1], ylim = c(0, 1.5), 
                    ...)
                }
            }
        }
        else out <- do.call("hist", c(list(yh), hist.args))
    }
    if (!(tform && is.element(model, c("PP", "GP", "Beta", "Exponential", 
        "Pareto")))) {
        if (is.element(type, c("primary", "density", "hist")) && 
            is.null(x$blocks)) {
            if (is.element(type, c("primary", "density"))) 
                xd <- seq(min(yd$x, na.rm = TRUE), max(yd$x, 
                  na.rm = TRUE), , 100)
            else xd <- seq(min(yh, na.rm = TRUE), max(yh, na.rm = TRUE), 
                , 100)
            if (is.element(model, c("PP", "Gumbel", "Weibull", 
                "Frechet"))) 
                mod2 <- "GEV"
            else if (is.element(model, c("Pareto", "Frechet", 
                "Beta", "Exponential"))) 
                mod2 <- "GP"
            else mod2 <- model
            if (tform) {
                mu <- 0
                sig <- 1
                xi <- 0
                u2 <- 0
            }
            else {
                mu <- loc
                sig <- scale
                xi <- shape
                u2 <- u[1]
            }
            yd2 <- devd(xd, loc = mu, scale = sig, shape = xi, 
                threshold = u2, type = mod2)
            lines(xd, yd2, lty = 2, col = "blue", lwd = 1.5)
            if (model != "PP") {
                if (type == "hist" || !is.null(density.args)) 
                  legend("topright", legend = "Modeled Density", 
                    col = "blue", lty = 2, lwd = 1.5, bty = "n")
                else if (is.null(density.args)) 
                  legend("topright", legend = c("Empirical", 
                    "Modeled"), col = c("black", "blue"), lty = c(1, 
                    2), lwd = c(1, 1.5), bty = "n")
            }
            else {
                if (type == "hist" || !is.null(density.args)) 
                  legend("topright", legend = "Modeled Density", 
                    col = "blue", lty = 2, lwd = 1.5, bty = "n")
                else if (is.null(density.args)) 
                  legend("topright", legend = c(paste("Empirical (", 
                    x$period.basis, " maxima)", sep = ""), "Modeled"), 
                    col = c("black", "blue"), lty = c(1, 2), 
                    lwd = c(1, 1.5), bty = "n")
            }
        }
    }
    if (is.element(type, c("primary", "Zplot")) && model == "PP") {
        if (type == "primary") {
            eeplot(x = x, type = "Zplot", main = "Z plot", d = d, 
                ...)
        }
        else out <- eeplot(x = x, type = type, d = d, ...)
    }
    if (type == "Zplot" && model != "PP") 
        stop("plot.fevd.mle: invalid type argument for this model.")
    if (is.element(type, c("primary", "rl")) && is.null(x$blocks)) {
        if (!(is.element(model, c("PP", "GP", "Exponential", 
            "Beta", "Pareto")) && !const.thresh && all(c(const.loc, 
            const.scale, const.shape)))) {
            if (is.null(args$main)) {
                if (type == "primary") 
                  m5 <- ""
                else m5 <- deparse(x$call)
            }
            if (model == "PP") {
                mod2 <- "GEV"
                if (is.null(args$main)) {
                  if (type == "rl") 
                    m5 <- paste(m5, "Return Levels based on approx. equivalent GEV", 
                      sep = "\n")
                  else m5 <- paste("Return Levels based on approx.", 
                    "equivalent GEV", sep = "\n")
                }
                blocks <- rep(1:x$span, each = x$npy)
                n2 <- length(blocks)
                if (n2 < x$n) 
                  blocks <- c(blocks, rep(blocks[n2], x$n - n2))
                else if (n2 > x$n) 
                  blocks <- blocks[1:x$n]
                yEmp <- c(aggregate(y, by = list(blocks), max)$x)
            }
            else mod2 <- model
            if (is.null(x$units)) 
                ylb <- "Return Level"
            else ylb <- paste("Return Level (", x$units, ")", 
                sep = "")
            if (!tform) {
                ## this probably calculates the return periods
                yrl <- rlevd(rperiods, loc = loc, scale = scale, 
                  shape = shape, threshold = u, type = mod2, 
                  npy = npy, rate = lam)
                bds <- ci(x, return.period = rperiods)
                if (is.null(args$ylim)) 
                  yl <- range(c(bds), finite = TRUE)
                if (is.element(model, c("PP", "GEV", "Gumbel", 
                  "Weibull", "Frechet"))) 
                  xrl <- -1/(log(1 - 1/rperiods))
                else xrl <- rperiods
                xlb <- paste("Return Period (", x$period.basis, 
                  "s)", sep = "")
                if (is.null(args$main)) {
                  if (!is.null(args$ylim)) 
                    plot(xrl, yrl, type = "l", log = "x", xlab = xlb, 
                      ylab = ylb, main = m5, ...)
                  else plot(xrl, yrl, type = "l", log = "x", 
                    ylim = yl, xlab = xlb, ylab = ylb, main = m5, 
                    ...)
                }
                else {
                  if (!is.null(args$ylim)) 
                    plot(xrl, yrl, type = "l", log = "x", xlab = xlb, 
                      ylab = ylb, ...)
                  else plot(xrl, yrl, type = "l", log = "x", 
                    ylim = yl, xlab = xlb, ylab = ylb, ...)
                }
                lines(xrl, bds[, 1], col = "gray", lty = 2, lwd = 2)
                lines(xrl, bds[, 3], col = "gray", lty = 2, lwd = 2)
                if (is.element(model, c("GEV", "Gumbel", "Weibull", 
                  "Frechet"))) {
                  ## XXX this is it: xp are the return periods
                  points(-1/log(xp), sort(y), pch=16)
                  if (type == "rl") 
                    out <- list(model = bds, empirical = data.frame(transformed.period = -1/log(xp), 
                      sorted.level = sort(y)))
                }
                else if (is.element(model, c("GP", "Beta", "Pareto", 
                  "Exponential"))) {
                  n2 <- x$n
                  if (is.null(a)) 
                    xp2 <- ppoints(n2)
                  else xp2 <- ppoints(n2, a = a)
                  sdat <- sort(y)
                  points(-1/log(xp2)[sdat > u]/npy, sdat[sdat > 
                    u])
                  if (type == "rl") {
                    out <- list(model = bds, empirical = data.frame(transformed.period = -1/log(xp2)[sdat > 
                      u]/npy, sorted.level = sdat[sdat > u]))
                  }
                }
                else if (model == "PP") {
                  if (is.null(a)) 
                    xp2 <- ppoints(length(yEmp))
                  else xp2 <- ppoints(length(yEmp), a = a)
                  points(-1/log(xp2), sort(yEmp))
                  if (type == "rl") 
                    out <- list(model = bds, empirical = data.frame(transformed.period = -1/log(xp2), 
                      sorted.level = sort(yEmp)))
                }
            }
            else {
                np <- length(rperiods)
                effrl <- matrix(NA, length(y), np)
                for (i in 1:np) effrl[, i] <- erlevd(x = x, period = rperiods[i])
                if (is.null(args$ylim)) {
                  yl <- range(c(c(y), c(effrl)), finite = TRUE)
                  yl[2] <- yl[2] + sign(yl[2]) * log2(abs(yl[2]))
                  if (is.null(args$main)) {
                    plot(y, type = "l", xlab = "index", ylab = ylb, 
                      main = m5, ylim = yl, ...)
                  }
                  else {
                    plot(y, type = "l", xlab = "index", ylab = ylb, 
                      ...)
                  }
                  if (type == "rl") 
                    out <- list(series = y, level = effrl)
                }
                else {
                  if (is.null(args$main)) {
                    plot(y, type = "l", xlab = "index", ylab = ylb, 
                      main = m5, ...)
                  }
                  else {
                    plot(y, type = "l", xlab = "index", ylab = ylb, 
                      ...)
                  }
                  if (type == "rl") 
                    out <- list(series = y[eid], level = effrl)
                }
                for (i in 1:np) lines(effrl[, i], lty = i, col = i + 
                  1)
                if (is.element(model, c("PP", "GP", "Beta", "Exponential", 
                  "Pareto"))) {
                  if (length(x$threshold) == 1) 
                    abline(h = x$threshold, col = "darkorange", 
                      lwd = 2)
                  else lines(x$threshold, col = "darkorange", 
                    lwd = 2)
                }
                if (!is.element(model, c("GEV", "Gumbel", "Weibull", 
                  "Frechet"))) {
                  legend("topleft", legend = c(paste(rperiods, 
                    "-", period, " level", sep = ""), "threshold"), 
                    lty = c(1:np, 1), col = c(2:(np + 1), "darkorange"), 
                    bg = "white")
                }
                else legend("topleft", legend = paste(rperiods, 
                  "-", period, " level", sep = ""), lty = 1:np, 
                  col = 2:(np + 1), bg = "white")
            }
        }
    }
    if (type == "trace") {
        op <- par()
        ntheta <- length(pars)
        if (is.null(prange)) {
            theta.hat <- pars
            if (check.constant(x$par.models$scale)) {
                phiU <- FALSE
                if (x$par.models$log.scale) 
                  theta.hat["log.scale"] <- exp(theta.hat["log.scale"])
                names(theta.hat)[names(theta.hat) == "log.scale"] <- "scale"
            }
            else phiU <- x$par.models$log.scale
            prange <- matrix(NA, 2, ntheta)
            tmp <- summary(x, silent = TRUE)
            tmp <- rbind(tmp$par, tmp$se.theta)
            if (is.matrix(tmp)) 
                for (j in 1:ntheta) prange[, j] <- c(tmp[1, j] - 
                  2 * tmp[2, j], tmp[1, j] + 2 * tmp[2, j])
            else {
                tmp <- c(tmp)
                for (j in 1:ntheta) prange[, j] <- c(tmp[j] - 
                  2 * log2(abs(tmp[j])), tmp[j] + 2 * log2(abs(tmp[j])))
            }
            if (any(tmp.id <- names(tmp) == "scale")) {
                if (!phiU) {
                  prange[1, tmp.id] <- max(prange[1, tmp.id], 
                    1e-08)
                  if (prange[1, tmp.id] > prange[2, tmp.id]) 
                    prange[2, tmp.id] <- 2 * prange[1, tmp.id]
                }
            }
        }
        hold <- list()
        par(mfrow = c(2, ntheta), oma = c(0, 0, 2, 0))
        for (i in 1:ntheta) {
            if (!is.null(data)) 
                hold[[i]] <- grlevdTracer(x = y, p = theta.hat, 
                  which.vary = i, p1.range = c(prange[, i]), 
                  threshold = u, threshold.fun = x$par.models$threshold, 
                  location.fun = x$par.models$location, scale.fun = x$par.models$scale, 
                  shape.fun = x$par.models$shape, data = data, 
                  phi = phiU, blocks = x$blocks, type = model, 
                  npy = x$npy, na.action = x$na.action, par1.name = names(theta.hat)[i], 
                  plot = FALSE)
            else hold[[i]] <- grlevdTracer(x = y, p = theta.hat, 
                which.vary = i, p1.range = c(prange[, i]), threshold = u, 
                threshold.fun = x$par.models$threshold, phi = phiU, 
                blocks = x$blocks, type = model, npy = x$npy, 
                na.action = x$na.action, par1.name = names(theta.hat)[i], 
                plot = FALSE)
            if (i == 1) 
                plot(hold[[i]], type = "likelihood", main = "", 
                  xlab = "")
            else plot(hold[[i]], type = "likelihood", main = "", 
                xlab = "", ylab = "")
            abline(v = theta.hat[i], lty = 2)
        }
        for (i in 1:ntheta) {
            if (i == 1) 
                plot(hold[[i]], type = "gradient", main = "")
            else plot(hold[[i]], type = "gradient", main = "", 
                ylab = "")
        }
        out <- hold
    }
    if (is.element(type, c("primary", "trace"))) {
        mtext(deparse(x$call), line = 0.5, outer = TRUE)
        par(mfrow = op$mfrow, oma = op$oma)
    }
    if (type != "primary") 
        invisible(out)
    else invisible()
}
