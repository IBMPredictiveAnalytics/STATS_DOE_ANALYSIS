# ══════════════════════════════════════════════════════════════════════════════
# STATS DOE ANALYSIS 2.15.1 (23-sep-2026)
#  * Fixes: radio-button groups on the Model and Mixture/Taguchi dialogs were
#    not drawn; PtType column dropped from generated mixture designs; mixture
#    auto-detection too strict for rounded amounts; validation errors shown 3x.
# STATS DOE ANALYSIS 2.15.0 (23-sep-2026)
#  * Term selection (backward/forward/stepwise), Box-Cox, general full
#    factorial with Tukey grouping, canonical analysis, 3-D surfaces,
#    overlaid contours, confirmation-run intervals, robustness simulation,
#    mixture designs (simplex-lattice/centroid generation, Scheffe models,
#    trace and ternary plots, blend optimizer), Taguchi S/N analysis.
#  * Fixes: TERMS without a "response:" prefix was ignored (default model used);
#    mixture/factor variables without LOWS/HIGHS crashed validation.
# STATS DOE ANALYSIS 2.14.0 (23-sep-2026)
#  * New coded-factor analysis engine (default; /FITMODEL ANALYSISMODEL=CODED):
#    -1/+1 coding incl. text factors, center-point curvature term, user model
#    TERMS (per response), alias removal + alias table, adjusted-SS ANOVA with
#    lack-of-fit/pure error, coded coefficients with effects and VIF, model
#    summary incl. predicted R-squared, natural-unit equations, unusual
#    observations, hierarchical backward elimination, fitted-means main-effect/
#    interaction/cube plots, Pareto/normal plots of standardized effects with
#    ALPHA-based reference line, contour and residual plots, HTML report.
#  * Multi-response desirability optimizer with lower/target/upper limits,
#    weights, importance, HOLD, several solutions, CI/PI at the optimum.
#  * Fixes: two-level designs were analysed with a saturated raw-unit model
#    (no error df, no t/p); fixed alpha 0.05 on the Pareto; raw-mean effect
#    plots joined center points; optimizer failed with text factors; VARNAMES
#    box disabled (and all columns used as factors) in analysis-only mode;
#    constraint-file and formula validations fired when not applicable.
# ══════════════════════════════════════════════════════════════════════════════
# ══════════════════════════════════════════════════════════════════════════════
# WINDOWS TEMP DIR FIX
# ══════════════════════════════════════════════════════════════════════════════
# Restricted corporate Windows accounts sometimes have an unwritable default
# temp dir, which makes spssRGraphics.Submit()/png() fail with "cannot access
# Rplot0XX.png". Redirect to a known-writable location before anything else runs.
if (.Platform$OS.type == "windows") {
    .td_candidates <- c(
        "C:/Temp",
        file.path(Sys.getenv("PUBLIC"),      "Temp"),
        file.path(Sys.getenv("USERPROFILE"), "RTemp"))
    for (.td in .td_candidates) {
        if (!dir.exists(.td)) dir.create(.td, recursive=TRUE, showWarnings=FALSE)
        if (dir.exists(.td)) { options(tmpdir = .td); break }
    }
    rm(.td_candidates, .td)
}

coerce_yesno <- function(x) {
    x <- unlist(x)
    if (is.logical(x)) return(x)
    return(tolower(as.character(x)) %in% c("yes","true","1"))
}

# Splits a possibly-multi-valued syntax argument (a vector from an islist=TRUE
# Template, OR a single space/comma separated string typed by a user, e.g. the
# RESPONSEVAR/OPTIMIZATIONGOALS plural-sounding fields) into a clean character
# vector with blanks removed. Used to make any keyword that *sounds* plural in
# its tooltip actually behave as a list everywhere it is read, instead of
# crashing R's `if()` on a length>1 logical comparison. Returns character(0)
# for NULL/empty input so callers can test length()==0 uniformly.
parse_multi_values <- function(x) {
    if (is.null(x)) return(character(0))
    x <- unlist(x)
    if (length(x) == 0) return(character(0))
    x <- unlist(strsplit(as.character(x), "[ ,]+"))
    x <- trimws(x)
    x[nzchar(x)]
}

# ══════════════════════════════════════════════════════════════════════════════
# R PACKAGE AUTO-INSTALL GUARD (shared mirror architecture, matches
# STATS_PROCESS_CAPABILITY and other Nestle SPSS extensions)
# ══════════════════════════════════════════════════════════════════════════════
# Tries public CRAN first, falls back to the org SharePoint mirror (Windows),
# is renv-aware for SPSS 32+, and uses the correct binary type per platform
# (Windows binary, Apple Silicon default, Intel Mac explicit big-sur-x86_64).
# Silent/no-op once packages are present - zero overhead on repeat runs.
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

.SPSS_FALLBACK_ZIP  <- paste0(
    "https://nestle.sharepoint.com/:u:/r/sites/SPSS_Statistics/",
    "Shared%20Documents/Packages/spss_cran_mirror.zip",
    "?csf=1&web=1&e=RHeShh")
.SPSS_LOCAL_REPO  <- "C:/spss_packages"
.SPSS_CRAN_MIRROR <- "https://cloud.r-project.org"

.spss_ensure_packages <- function(pkgs) {
    missing_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
    if (length(missing_pkgs) == 0) return(invisible(TRUE))

    is_intel_mac <- .Platform$OS.type != "windows" && grepl("x86_64", R.version$arch %||% "")
    pkg_type <- if (.Platform$OS.type == "windows") "binary"
                else if (is_intel_mac) "mac.binary.big-sur-x86_64"
                else getOption("pkgType")

    r_ver    <- paste0(R.version$major, ".", strsplit(R.version$minor, "\\.")[[1]][1])
    repo_dir <- file.path(.SPSS_LOCAL_REPO, "bin", "windows", "contrib", r_ver)
    repo_url <- paste0("file:///", .SPSS_LOCAL_REPO)

    pkg_in_repo <- function(pkg) {
        dir.exists(repo_dir) &&
        length(list.files(repo_dir, pattern = paste0("^", pkg, "_"),
                          ignore.case = TRUE)) > 0
    }

    writable_lib <- tryCatch({
        candidate <- NULL
        for (p in .libPaths()) {
            if (dir.exists(p) && file.access(p, 2L) == 0L) { candidate <- p; break }
        }
        if (is.null(candidate)) {
            ulib <- file.path(path.expand("~"), ".R", "library")
            dir.create(ulib, recursive = TRUE, showWarnings = FALSE)
            if (dir.exists(ulib)) { .libPaths(c(ulib, .libPaths())); candidate <- ulib }
        }
        candidate
    }, error = function(e) NULL)

    use_renv <- isNamespaceLoaded("renv") &&
                tryCatch(is.function(renv::install), error = function(e) FALSE)

    # Step 1: try public CRAN (renv-aware)
    cran_ok <- tryCatch({
        tmp_chk <- tempfile(); old_to <- getOption("timeout"); options(timeout = 15)
        on.exit({ unlink(tmp_chk); options(timeout = old_to) }, add = TRUE)
        res <- suppressWarnings(download.file(
            paste0(.SPSS_CRAN_MIRROR, "/src/contrib/PACKAGES.gz"),
            tmp_chk, quiet = TRUE, mode = "wb"))
        res == 0
    }, error = function(e) FALSE, warning = function(w) FALSE)

    if (cran_ok) {
        for (pkg in missing_pkgs) {
            if (use_renv)
                tryCatch(renv::install(pkg, repos = .SPSS_CRAN_MIRROR, prompt = FALSE),
                         error = function(e) NULL)
            if (!requireNamespace(pkg, quietly = TRUE))
                tryCatch(
                    install.packages(pkg, repos = .SPSS_CRAN_MIRROR, lib = writable_lib,
                                     dependencies = c("Depends","Imports","LinkingTo"),
                                     quiet = TRUE, type = pkg_type),
                    error = function(e) NULL)
        }
        missing_pkgs <- missing_pkgs[!sapply(missing_pkgs, requireNamespace, quietly = TRUE)]
        if (length(missing_pkgs) == 0) return(invisible(TRUE))
    }

    # Step 1b: Intel Mac extra fallback - bypass renv, install to SPSS rpackage dir
    if (is_intel_mac && length(missing_pkgs) > 0) {
        spss_rpkg <- Filter(function(p) grepl("rpackage", p, fixed = TRUE) &&
                                        dir.exists(p) && file.access(p, 2L) == 0L, .libPaths())
        if (length(spss_rpkg) > 0)
            for (pkg in missing_pkgs)
                tryCatch(
                    install.packages(pkg, repos = .SPSS_CRAN_MIRROR, lib = spss_rpkg[1],
                                     dependencies = c("Depends","Imports","LinkingTo"),
                                     quiet = TRUE, type = "mac.binary.big-sur-x86_64"),
                    error = function(e) NULL)
        missing_pkgs <- missing_pkgs[!sapply(missing_pkgs, requireNamespace, quietly = TRUE)]
        if (length(missing_pkgs) == 0) return(invisible(TRUE))
    }

    # Step 2: CRAN blocked/incomplete -> SharePoint fallback (Windows only)
    if (.Platform$OS.type == "windows" && length(missing_pkgs) > 0) {
        need_download <- !all(sapply(missing_pkgs, pkg_in_repo))
        if (need_download) {
            zip_dest <- file.path(tempdir(), "spss_cran_mirror.zip")
            if (!dir.exists(.SPSS_LOCAL_REPO))
                dir.create(.SPSS_LOCAL_REPO, recursive = TRUE, showWarnings = FALSE)
            dl_ok <- tryCatch({
                suppressWarnings(
                    download.file(.SPSS_FALLBACK_ZIP, zip_dest, mode = "wb", quiet = TRUE))
                file.size(zip_dest) > 100000 &&
                    !inherits(tryCatch(unzip(zip_dest, list = TRUE), error = function(e) e), "error")
            }, error = function(e) FALSE)
            if (dl_ok) {
                unzip(zip_dest, exdir = .SPSS_LOCAL_REPO, overwrite = TRUE)
                unlink(zip_dest)
            }
        }
        if (dir.exists(repo_dir)) {
            for (pkg in missing_pkgs)
                tryCatch(install.packages(pkg, repos = repo_url, type = "win.binary",
                         dependencies = TRUE, quiet = TRUE), error = function(e) NULL)
            missing_pkgs <- missing_pkgs[!sapply(missing_pkgs, requireNamespace, quietly = TRUE)]
        }
    }

    invisible(length(missing_pkgs) == 0)
}

# Attempt to auto-install every package this extension can use (silent no-op
# if already present). Individual features still degrade gracefully via the
# has_* flags below and their own requireNamespace() checks if any package
# remains unavailable after this (e.g. fully offline, non-Windows, no Xcode).
.spss_ensure_packages(c("AlgDesign","FrF2","rsm","desirability","BsMD",
                         "ggplot2","gridExtra","leaps",
                         "plotly","htmlwidgets","jsonlite"))

# ══════════════════════════════════════════════════════════════════════════════
# GLOBAL PACKAGE AVAILABILITY FLAGS
# ══════════════════════════════════════════════════════════════════════════════
# Define at top level so all functions can access them without parameter passing
has_FrF2         <- suppressMessages(requireNamespace("FrF2", quietly=TRUE))   # silences the DoE.base "S3 method overwritten" load note
has_rsm          <- requireNamespace("rsm",          quietly=TRUE)
has_desirability <- requireNamespace("desirability", quietly=TRUE)
has_BsMD         <- requireNamespace("BsMD",         quietly=TRUE)
has_ggplot2      <- requireNamespace("ggplot2",      quietly=TRUE)
has_gridExtra    <- requireNamespace("gridExtra",    quietly=TRUE)
has_plotly       <- requireNamespace("plotly",       quietly=TRUE)
has_htmlwidgets  <- requireNamespace("htmlwidgets",  quietly=TRUE)
has_jsonlite     <- requireNamespace("jsonlite",     quietly=TRUE)

# ══════════════════════════════════════════════════════════════════════════════
# CROSS-PLATFORM DEFAULT REPORT LOCATION
# ══════════════════════════════════════════════════════════════════════════════
# No GUI control exists (or should exist) for the HTML report path - it must be
# resolved automatically on Windows, macOS, and Linux alike. Strategy: prefer a
# "SPSS_DOE_Reports" subfolder of the user's Documents folder when one exists
# (Windows/macOS typical layout), falling back to the home directory, and
# finally to the R session temp dir if nothing is writable.
get_default_report_dir <- function() {
    home <- tryCatch(path.expand("~"), error=function(e) "")
    if (!nzchar(home) || !dir.exists(home)) {
        home <- tryCatch(Sys.getenv("USERPROFILE", unset=""), error=function(e) "")
    }
    if (!nzchar(home) || !dir.exists(home)) {
        home <- tryCatch(Sys.getenv("HOME", unset=""), error=function(e) "")
    }
    if (!nzchar(home) || !dir.exists(home)) home <- tempdir()

    docs     <- file.path(home, "Documents")
    base_dir <- if (dir.exists(docs)) docs else home
    out_dir  <- file.path(base_dir, "SPSS_DOE_Reports")

    ok <- tryCatch({ dir.create(out_dir, recursive=TRUE, showWarnings=FALSE); dir.exists(out_dir) },
                   error=function(e) FALSE)
    if (!ok) out_dir <- tempdir()
    out_dir
}

# Builds a unique, descriptive .html filename for a report (design or analysis
# phase) inside the resolved default directory, stamped so repeated runs never
# silently overwrite each other.
make_default_html_path <- function(tag, label) {
    out_dir <- get_default_report_dir()
    safe    <- gsub("[^A-Za-z0-9_-]+", "_", as.character(label))
    fname   <- sprintf("DOE_%s_%s_%s.html", tag, safe, format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.path(out_dir, fname)
}

optdesmc <- function(varnames=NULL, frml=NULL, factors="no",
    mixtures="no", lows=-Inf, highs=Inf, centers=NULL,
    nlevels=NULL, roundtos=NULL, ncand=NULL,
    constraintfunc=NULL, mixturesum=1,
    model="linear", constant=TRUE,
    ntrials=NULL, criterion="d", center=FALSE,
    initial="random", repeats=5, designalg="exact",
    # BUGFIX: outputdataset used to have no default, so R's lazy-argument
    # evaluation would throw a raw "argument is missing, with no default"
    # crash the instant any code path referenced it (e.g. augment_to_ccd())
    # if the /SAVE subcommand was omitted from syntax entirely (which now
    # happens whenever Output Dataset is left blank -- see the /SAVE
    # conditional-emission fix in extension.xml). Defaulting to NULL lets
    # every existing null/empty check downstream (augment_to_ccd's own
    # check, and the new explicit check below) fire a clean, translatable
    # warning instead of an uncontrolled R error. Fully backward compatible:
    # when DATASET=... is supplied, outputdataset is populated exactly as
    # before.
    outputdataset=NULL, confounding=TRUE,
    designtype="optimal", generatedesign=TRUE, randomize=FALSE, blocks=NULL, replicates=NULL,
    responsevar=NULL, analyze=FALSE, createplots=FALSE,
    # REVERTED (regression fix): these 7 flags were briefly changed to
    # default TRUE, on the theory that it would only affect hand-typed
    # syntax that omits the keyword. That was wrong. Each checkbox's pasted
    # syntax macro is single-sided ("MAINEFFECTS=YES±" with no "±...=NO"
    # counterpart -- confirmed directly in extension.xml), meaning CDB
    # OMITS the keyword entirely whenever the box is unchecked, not just
    # for hand-typed syntax. Since every freshly opened dialog starts with
    # all chart boxes unchecked (CDB has no default-checked mechanism), the
    # TRUE default meant EVERY GUI-driven run silently rendered all 7
    # charts regardless of what was checked -- interaction/cube/contour
    # plots iterate over every factor pair, so this made ordinary runs take
    # a very long time. Reverted to FALSE, matching original R behavior.
    maineffects=FALSE, interactions=FALSE, cubeplot=FALSE,
    contourplot=FALSE, residualplots=FALSE, paretoplot=FALSE, curvatureplot=FALSE,
    optimizeresponse=FALSE, optimizationgoal="maximize", optimizationgoals=NULL,
    # ADDITIVE FEATURE: numeric target value(s) for OPTIMIZATIONGOAL=TARGET /
    # OPTIMIZATIONGOALS containing "target" entries. Previously there was no
    # way -- neither in the GUI (no input field existed) nor in hand-typed
    # syntax (no keyword existed at all) -- to specify WHAT value "target"
    # should aim for; do_optimization()/do_multi_response_optimization()
    # always silently substituted mean(response) (single) or mean(range(response))
    # (multi), regardless of what the user actually needed to hit. Both left
    # NULL by default, which preserves that exact historical fallback
    # behavior unchanged for anyone not using these new keywords.
    optimizationtarget=NULL, optimizationtargets=NULL, externalplots="no",
    exporthtml=TRUE, htmlpath=NULL,
    effectstable=FALSE, residualtests=FALSE, viftable=FALSE, optdetailtable=FALSE,
    screeningcenterpts=FALSE, screeningcenterptscount=NULL, screeningsummary=FALSE,
    powertable=FALSE, powereffectsizes=NULL, powerruns=NULL, poweralpha=0.05, powertarget=0.8,
    augmenttoccd=FALSE, augmentcenterpts=NULL,
    varselect=FALSE, varselmethod="stepwise", stepdir="both",
    selectiontrace=FALSE, fitprofile=FALSE, parsimonyplot=FALSE, factormap=FALSE,
    # Coded-factor analysis engine (see "CODED-FACTOR ANALYSIS ENGINE")
    analysismodel="coded", terms=NULL, alpha=0.05, backward=FALSE, alpharemove=0.10,
    centerterm=TRUE, conflevel=95, lowlevels=NULL, equation=TRUE, aliastable=TRUE,
    diagtable=TRUE, factorinfo=TRUE, effectsplot=FALSE,
    optlowers=NULL, optuppers=NULL, optweights=NULL, optimportance=NULL, opthold=NULL,
    optsolutions=1,
    selection="none", alphaenter=0.15, boxcox="none", lambda=NULL, categorical=NULL,
    grouping=FALSE, surfaceplot=FALSE, overlay=FALSE, canonical=FALSE,
    robust=FALSE, robustsd=5, confirmruns=NULL,
    mixmodel="quadratic", mixcomps="auto", traceplot=FALSE, sntype=NULL,
    latticedegree=2, augmentmix=FALSE, interpret=TRUE, predict=NULL) {

    setuplocalization("STATS_DOE_ANALYSIS")

    procname         <- gtxt("Design of Experiments - Enhanced")
    warningsprocname <- gtxt("Design of Experiments: Warnings")
    omsid            <- "STATSDOEANALYSIS"
    warns            <- Warn(procname=warningsprocname, omsid=omsid)

    # BUGFIX: early validation stop messages (Output Dataset required/already
    # in use, VARNAMES required, no-analysis-requested, etc.) were landing
    # only in the syntax/log pane instead of a proper Output Viewer warning
    # table. A prior attempt at fixing this primed StartProcedure with a
    # separate, empty Start/EndProcedure cycle using the same procedure
    # name/omsid right before the real one -- that made the Warnings heading
    # appear, but with an EMPTY table (the back-to-back reuse of the same
    # name/omsid appears to confuse SPSS's output routing, so the second,
    # real StartProcedure call's table content doesn't land correctly). That
    # priming cycle has been removed. The defensive cleanup now lives inside
    # Warn()$display() itself (see below), immediately before its own
    # StartProcedure() call, so there is exactly one Start/EndProcedure cycle
    # per warning display, with no separate throwaway call to collide with.
    tryCatch(spsspkg.EndProcedure(), error=function(e) NULL)

    # Normalise CDB auto-generated enum IDs -> canonical values
    # (CDB assigns item_NNN_a/b/c instead of the named values in extension.xml)
    if (!is.null(varselmethod)) {
        varselmethod <- switch(tolower(as.character(varselmethod)),
            "item_273_a" = "stepwise",
            "item_273_b" = "bestsubsets",
            varselmethod)
    }

    # ROBUSTNESS: VARSELMETHOD defaults to "stepwise" whenever it isn't
    # explicitly supplied (from the R function signature default), and
    # nothing above changes that default if the CDB dialog's radio-button
    # value doesn't come through as expected. Rather than requiring
    # VARSELMETHOD to be set correctly for the checked charts to work, infer
    # the method from which chart checkboxes are actually requested: if
    # VARSELMETHOD is still at its default "stepwise" but a Best-Subsets-only
    # chart (FITPROFILE/PARSIMONYPLOT/FACTORMAP) was requested and the
    # Stepwise-only chart (SELECTIONTRACE) was not, the user's intent is
    # clearly Best Subsets, so switch automatically. This makes the checked
    # charts work correctly regardless of whether VARSELMETHOD itself binds
    # correctly. Silent by design (no Warnings-table entry) -- this is normal,
    # expected behavior from the user's point of view, not something to flag.
    # Purely additive: if VARSELMETHOD was already set to something other
    # than the default "stepwise" (e.g. typed explicitly as BESTSUBSETS, or
    # normalized above from item_273_b), this condition is false and nothing
    # changes.
    if (isTRUE(varselect) &&
        identical(tolower(trimws(as.character(varselmethod))), "stepwise") &&
        !isTRUE(selectiontrace) &&
        (isTRUE(fitprofile) || isTRUE(parsimonyplot) || isTRUE(factormap))) {
        varselmethod <- "bestsubsets"
    }

    if (!is.null(stepdir)) {
        stepdir <- switch(tolower(as.character(stepdir)),
            "item_278_a" = "both",
            "item_278_b" = "forward",
            "item_278_d" = "backward",
            stepdir)
    }
    if (!is.null(model)) {
        model <- switch(tolower(as.character(model)),
            "item_313_a" = "linear",
            "item_313_b" = "quad",
            model)
    }
    if (!is.null(criterion)) {
        criterion <- switch(tolower(as.character(criterion)),
            "item_382_a" = "d",
            "item_382_b" = "a",
            criterion)
    }
    if (!is.null(initial)) {
        initial <- switch(tolower(as.character(initial)),
            "item_562_a" = "random",
            "item_562_b" = "nullification",
            initial)
    }
    if (!is.null(designalg)) {
        designalg <- switch(tolower(as.character(designalg)),
            "item_93_a" = "approx",
            "item_93_b" = "exact",
            designalg)
    }

    # ── package loading ──────────────────────────────────────────────────────
    tryCatch(suppressMessages(suppressWarnings(library(AlgDesign))), error=function(e)
        warns$warn(gtxtf("Required package '%s' could not be loaded.", "AlgDesign"), dostop=TRUE))

    # Load optional packages (has_* flags already defined at top level)
    if (has_FrF2)         suppressMessages(suppressWarnings(library(FrF2)))
    if (has_rsm)          suppressMessages(suppressWarnings(library(rsm)))
    if (has_desirability) suppressMessages(suppressWarnings(library(desirability)))
    if (has_BsMD)         suppressMessages(suppressWarnings(library(BsMD)))
    
    # ── bool fixups ──────────────────────────────────────────────────────────
    to_bool <- function(x) {
        if (is.logical(x)) return(isTRUE(x))
        isTRUE(tolower(as.character(x)) %in% c("yes","true","1"))
    }
    randomize       <- to_bool(randomize)
    analyze         <- to_bool(analyze)
    createplots     <- to_bool(createplots)
    maineffects     <- to_bool(maineffects)
    interactions    <- to_bool(interactions)
    cubeplot        <- to_bool(cubeplot)
    contourplot     <- to_bool(contourplot)
    residualplots   <- to_bool(residualplots)
    paretoplot      <- to_bool(paretoplot)
    curvatureplot   <- to_bool(curvatureplot)
    optimizeresponse<- to_bool(optimizeresponse)
    externalplots   <- to_bool(externalplots)
    exporthtml      <- to_bool(exporthtml)
    effectstable    <- to_bool(effectstable)
    residualtests   <- to_bool(residualtests)
    viftable        <- to_bool(viftable)
    optdetailtable  <- to_bool(optdetailtable)
    screeningcenterpts   <- to_bool(screeningcenterpts)
    screeningsummary     <- to_bool(screeningsummary)
    powertable           <- to_bool(powertable)
    backward             <- to_bool(backward)
    centerterm           <- if (is.null(centerterm)) TRUE else to_bool(centerterm)
    equation             <- to_bool(equation)
    aliastable           <- to_bool(aliastable)
    diagtable            <- to_bool(diagtable)
    factorinfo           <- to_bool(factorinfo)
    effectsplot          <- to_bool(effectsplot)
    grouping             <- to_bool(grouping)
    surfaceplot          <- to_bool(surfaceplot)
    overlay              <- to_bool(overlay)
    canonical            <- to_bool(canonical)
    robust               <- to_bool(robust)
    traceplot            <- to_bool(traceplot)
    augmentmix           <- to_bool(augmentmix)
    designtype           <- tolower(as.character(unlist(designtype)[1]))
    designtype           <- switch(designtype, designtype_slattice="simplexlattice", designtype_scentroid="simplexcentroid", designtype)
    analysismodel        <- tolower(trimws(as.character(unlist(analysismodel)[1])))
    analysismodel        <- switch(analysismodel, "item_am_a"="coded", "item_am_b"="legacy", analysismodel)
    if (!analysismodel %in% c("coded","legacy")) {
        warns$warn(gtxtf("ANALYSISMODEL=%s is not recognized; CODED used.", analysismodel), dostop=FALSE)
        analysismodel <- "coded"
    }
    if (!is.null(screeningcenterptscount)) screeningcenterptscount <- suppressWarnings(as.integer(unlist(screeningcenterptscount)[1]))
    if (!is.null(screeningcenterptscount) && (is.na(screeningcenterptscount) || screeningcenterptscount < 1)) screeningcenterptscount <- NULL
    if (!is.null(htmlpath)) htmlpath <- unlist(htmlpath)[1]
    if (!is.null(htmlpath) && !nzchar(htmlpath)) htmlpath <- NULL

    # ── Multi-response parsing (additive) ─────────────────────────────────────
    # RESPONSEVAR's tooltip has always invited multiple space-separated names,
    # but every downstream read of `responsevar` compared it as a single
    # string -- a length>1 vector would hard-crash R's `if()`. Parsed once,
    # here, into a clean vector; `responsevar` itself is left untouched so
    # every existing single-response code path below (which only ever reads
    # the original `responsevar` variable) behaves exactly as before.
    responsevars_all    <- parse_multi_values(responsevar)
    optimizationgoals_all <- parse_multi_values(optimizationgoals)
    if (length(optimizationgoals_all) == 0 && length(responsevars_all) > 0)
        optimizationgoals_all <- rep(optimizationgoal, length(responsevars_all))
    if (length(responsevars_all) > 0 && length(optimizationgoals_all) < length(responsevars_all))
        optimizationgoals_all <- rep(optimizationgoals_all, length.out=length(responsevars_all))
    multi_response       <- length(responsevars_all) > 1
    primary_responsevar  <- if (length(responsevars_all) >= 1) responsevars_all[1] else responsevar

    # ── Numeric target value(s), parallel to optimizationgoal(s) above ───────
    # single-response: OPTIMIZATIONTARGET (one float). multi-response:
    # OPTIMIZATIONTARGETS (one float per response, same order as
    # RESPONSEVAR/OPTIMIZATIONGOALS). Unspecified entries stay NA, which
    # do_optimization()/do_multi_response_optimization() both treat as "fall
    # back to the historical mean(response)/mean(range) behavior" -- so a run
    # that never mentions these keywords is completely unaffected.
    optimizationtarget_num  <- suppressWarnings(as.numeric(unlist(optimizationtarget)[1]))
    optimizationtargets_all <- suppressWarnings(as.numeric(parse_multi_values(optimizationtargets)))
    if (length(optimizationtargets_all) == 0 && length(responsevars_all) > 0)
        optimizationtargets_all <- rep(optimizationtarget_num, length(responsevars_all))
    if (length(responsevars_all) > 0 && length(optimizationtargets_all) < length(responsevars_all))
        optimizationtargets_all <- rep(optimizationtargets_all, length.out=length(responsevars_all))

    # ── Power & Sample Size calculator (additive, OFF by default) ────────────
    # A pure planning tool: independent of GENERATEDESIGN, gated only on its
    # own POWERTABLE keyword so it never changes any existing default output.
    if (powertable) {
        tryCatch({
            pes <- suppressWarnings(as.numeric(parse_multi_values(powereffectsizes)))
            pes <- pes[!is.na(pes) & pes > 0]
            prs <- suppressWarnings(as.integer(parse_multi_values(powerruns)))
            prs <- prs[!is.na(prs) & prs >= 3]
            pa  <- suppressWarnings(as.numeric(unlist(poweralpha)[1]))
            if (is.na(pa) || pa <= 0 || pa >= 1) pa <- 0.05
            ptg <- suppressWarnings(as.numeric(unlist(powertarget)[1]))
            if (is.na(ptg) || ptg <= 0 || ptg >= 1) ptg <- 0.8
            do_power_analysis(pes, prs, pa, ptg, warns)
        }, error=function(e) {
            tryCatch(spsspkg.EndProcedure(), error=function(x) NULL)
            warns$warn(gtxtf("Power and sample size calculation error: %s", e$message), dostop=FALSE)
        })
    }

    # Load ggplot2 for external plots if requested (after conversion to bool)
    if (externalplots && has_ggplot2) {
        suppressMessages(suppressWarnings(library(ggplot2)))
        if (has_gridExtra) suppressMessages(suppressWarnings(library(gridExtra)))
    }

    if (is.list(factors))  factors  <- fixtype(factors,  warns)
    if (is.list(mixtures)) mixtures <- fixtype(mixtures, warns)
    
    # Simplex (mixture) designs: every VARNAMES entry is a mixture component
    if (designtype %in% c("simplexlattice","simplexcentroid") && !is.null(varnames) &&
        !any(coerce_yesno(mixtures)))
        mixtures <- as.list(rep("yes", length(unlist(varnames))))

    # CRITICAL FIX: Properly coerce yes/no strings to logical
    factors  <- coerce_yesno(factors)
    mixtures <- coerce_yesno(mixtures)

    hasformula <- !is.null(frml)

    alldatasets <- spssdata.GetDataSetList()
    if ("*" %in% alldatasets)
        warns$warn(gtxt("The active dataset must have a name in order to use this procedure"), dostop=TRUE)

    # ── Sequential DOE: augment an existing factorial to a CCD (additive,
    # OFF by default) ─────────────────────────────────────────────────────────
    # Standalone third mode, dispatched and fully handled here, BEFORE any of
    # the existing GENERATEDESIGN/read_existing logic below even runs. Nothing
    # in this branch is reachable unless AUGMENTTOCCD=YES is typed explicitly,
    # so every existing GENERATEDESIGN=YES/NO syntax and code path is
    # completely untouched and unaffected by this feature's existence.
    augmenttoccd <- to_bool(augmenttoccd)
    if (augmenttoccd) {
        tryCatch(
            augment_to_ccd(varnames, responsevar, outputdataset, augmentcenterpts, alldatasets, warns),
            error=function(e) {
                tryCatch(spsspkg.EndProcedure(), error=function(x) NULL)
                warns$warn(gtxtf("Augment-to-CCD error: %s", e$message), dostop=FALSE)
            })
        warns$display(inproc=FALSE)
        tryCatch(rm(list=ls()), warning=function(e) NULL)
        return(invisible(NULL))
    }

    # ── Determine mode: Design generation or Analysis-only ────
    # Auto-detect mode based on parameters:
    # - If VARNAMES provided → Design generation mode
    # - If only RESPONSEVAR provided → Analysis-only mode
    # - generatedesign parameter can override (defaults to TRUE)
    
    # Auto-detect if not explicitly set
    if (is.null(generatedesign) || !is.logical(generatedesign)) {
        # If varnames provided, assume design generation
        # If only responsevar provided, assume analysis
        if (!is.null(varnames) && length(varnames) > 0) {
            generatedesign <- TRUE
        } else if (!is.null(responsevar)) {
            generatedesign <- FALSE
        } else {
            generatedesign <- TRUE  # Default to design generation
        }
    }
    
    read_existing <- !generatedesign
    factor_source <- "given"

    # BUGFIX: Output Dataset (SAVE DATASET=...) is mandatory whenever a new
    # design is being generated (GENERATEDESIGN=YES), but optional when
    # reading from the active dataset (GENERATEDESIGN=NO) for analysis-only
    # runs. This is a defense-in-depth check for anyone driving the
    # procedure via raw syntax (bypassing the dialog's own OK-button
    # validation): give one clean, translatable stop message instead of
    # letting outputdataset reach tolower()/dataset_exists below as NULL.
    if (!read_existing && (is.null(outputdataset) || !nzchar(outputdataset))) {
        warns$warn(gtxt("Output Dataset is required when Generate Design is selected (specify DATASET on the SAVE subcommand)"), dostop=TRUE)
    }

    # BUGFIX: GENERATEDESIGN=NO (read the active dataset for analysis) is a
    # complete no-op unless the user also asks for some analysis output --
    # there is no other work this procedure does in that mode. Previously this
    # silently read the active dataset and produced no output at all with no
    # explanation. Give one clear, translatable message instead. Not a hard
    # stop (dostop=FALSE): the active-dataset read itself is harmless and some
    # callers may legitimately just want the existence/variable check.
    if (read_existing && !(isTRUE(analyze) && length(responsevars_all) > 0)) {
        warns$warn(gtxt("Generate Design is off, so this run only reads the active dataset -- no output was requested. To see results, check Analyze (under the Analysis dialog) and specify a Response Variable."), dostop=FALSE)
    }

    # Check if output dataset already exists (only matters when generating new design)
    if (!read_existing) {
        dataset_exists <- tolower(outputdataset) %in% tolower(alldatasets)
        if (dataset_exists)
            warns$warn(gtxt("The output dataset name is already in use"), dostop=TRUE)
    }

    # ── validation (skip if reading from active dataset for analysis) ────────
    if (!read_existing) {
        # Full validation for design generation
        spec      <- validate(varnames, frml, factors, nlevels, lows, highs, centers, roundtos, mixtures, warns,
                               designtype=designtype, model=model)
        variables <- spec[,1]
        factors   <- as.logical(spec[,7])
        vlevels   <- spec[,5]

        # Safe check - mixtures is now a logical vector
        if (any(mixtures, na.rm=TRUE)) constant <- FALSE
        
        # ── constraint function (only needed for design generation) ─────────────
        dfilter <- NULL
        if (!is.null(constraintfunc)) {
            funcenv <- new.env()
            tryCatch(sys.source(constraintfunc, envir=funcenv),
                error=function(e) warns$warn(e$msg, dostop=TRUE))
            if (!exists("dfilter", envir=funcenv))
                warns$warn(gtxt("The constraint function file does not define a function named dfilter"), dostop=TRUE)
            else
                dfilter <- get("dfilter", envir=funcenv)
        }

        criterion  <- toupper(criterion)
        factorlist <- buildfactorlist(variables, factors, warns)
        frml_built <- genfrml(variables, frml, model, constant, factorlist)
        designtype <- tolower(designtype)
    } else {
        # Minimal setup for analysis - just use varnames from command
        # The design was already validated when created
        variables <- varnames
        spec <- NULL
        vlevels <- NULL
        dfilter <- NULL
        factorlist <- NULL
        frml_built <- NULL
        designtype <- tolower(designtype)
    }

    # ════════════════════════════════════════════════════════════════════════
    # DESIGN GENERATION OR READING FROM EXISTING DATASET
    # ════════════════════════════════════════════════════════════════════════
    
    if (read_existing) {
        # Read from active dataset for analysis (conventional)
        tryCatch({
            # Read all data from the active dataset
            existing_data <- spssdata.GetDataFromSPSS()
            
            # Verify the response variable(s) exist. Uses the parsed
            # responsevars_all vector (length 1 for the original/default
            # single-response usage, so behavior is unchanged there) instead
            # of comparing the raw possibly-multi-valued `responsevar` string
            # directly, which would error inside if() for length>1 input.
            missing_rv <- setdiff(responsevars_all, names(existing_data))
            if (length(missing_rv) > 0) {
                warns$warn(gtxtf("Response variable(s) not found in active dataset: %s",
                                paste(missing_rv, collapse=", ")), dostop=TRUE)
            }

            # Extract variable names from dataset (exclude meta columns and response)
            meta_cols <- c("Reps", "Proportion", "StdOrder", "RunOrder", "Block", "CenterPt", "PtType")
            all_cols <- names(existing_data)
            factor_cols <- setdiff(all_cols, c(meta_cols, responsevars_all))
            
            # If VARNAMES is not given: use the design information saved in
            # the dataset (DOE_Factors attribute) or, failing that, detect the
            # factors (columns with a few distinct settings).
            if (is.null(varnames) || length(unlist(varnames)) == 0) {
                fr <- .doe_resolve_factors(existing_data, responsevars_all)
                variables <- fr$factors; factor_source <- fr$source
                if (!length(variables))
                    warns$warn(gtxt("VARNAMES was not given and no factor columns could be found (a factor has 2 to 7 distinct settings). Name the factors with VARNAMES."), dostop=TRUE)
            } else {
                variables <- varnames; factor_source <- "given"
            }
            
            # Create a minimal res structure for analysis
            res <- list(design=existing_data, D=NA, A=NA, Ge=NA, Dea=NA)
        }, error=function(e) {
            warns$warn(gtxtf("Could not read from active dataset: %s", e$message), dostop=TRUE)
        })
        if (!identical(factor_source, "given") && isTRUE(analyze)) .doe_show_factors(existing_data, variables, factor_source)
    } else {
        # Generate new design
        res <- switch(designtype,
        "factorial"      = {
            if (has_FrF2)
                generate_factorial_FrF2(spec, variables, factors, ntrials, blocks, replicates, warns)
            else
                generate_factorial_base(spec, variables, factors, replicates, warns)
        },
        "plackettburman" = {
            if (has_FrF2)
                generate_pb_FrF2(spec, variables, ntrials, warns)
            else
                generate_pb_base(spec, variables, ntrials, warns)
        },
        "boxbehnken"     = generate_box_behnken(spec, variables, replicates, warns),
        "ccd"            = generate_ccd(spec, variables, replicates, warns),
        "rsm"            = generate_ccd(spec, variables, replicates, warns),
        "taguchi"        = generate_taguchi(spec, variables, factors, warns),
        "simplexlattice" = generate_simplex(spec, variables, "lattice", latticedegree, augmentmix, mixturesum, warns),
        "simplexcentroid"= generate_simplex(spec, variables, "centroid", latticedegree, augmentmix, mixturesum, warns),
        "lhs"            = generate_lhs(spec, variables, ntrials, warns),
        "dsd"            = generate_dsd(spec, variables, warns),
        "fullfactorial"  = generate_full_factorial(spec, variables, warns),
        generate_optimal(spec, variables, frml_built, ntrials, designalg,
                         mixturesum, criterion, initial, repeats, ncand, dfilter, warns)
    )

        # ── Drop extraneous columns from third-party design-generation
        # algorithms ─────────────────────────────────────────────────────────
        # AlgDesign::optMonteCarlo (used for DESIGNTYPE=OPTIMAL) can return
        # columns beyond the requested factors. Most are genuine diagnostic/
        # internal columns and get dropped below. "Proportion" is the one
        # legitimate exception: per AlgDesign's docs, when DESIGNALG=APPROX
        # (approximate=TRUE), optMonteCarlo returns an approximate-theory
        # design where "Proportion" is the relative weight/frequency at which
        # each design point should be run -- real, meaningful DOE output, not
        # an artifact. It's kept here (alongside "Reps") but still excluded
        # from var_names/meta_cols everywhere else in this file so it's never
        # treated as a phantom factor in the model fit or in Main Effects/
        # Interaction/Cube/Pareto charts.
        # BUGFIX: "Block" is a second legitimate exception, for the same
        # reason -- generate_factorial_FrF2() can now attach a genuine,
        # FrF2-native block assignment column (see its own BLOCKS handling).
        # Without listing it here, this cleanup step ran first and deleted
        # that real Block column as if it were a stray diagnostic artifact,
        # silently discarding FrF2's confounding-aware blocking before the
        # later, less-precise round-robin fallback re-added a different
        # Block column in its place.
        expected_cols <- c(as.character(variables), "Reps", "Proportion", "Block", "PtType")
        extra_cols    <- setdiff(names(res$design), expected_cols)
        if (length(extra_cols) > 0) {
            warns$warn(gtxtf(
                "Removed column(s) not among the named factors from the generated design (likely an internal diagnostic column from the design algorithm): %s",
                paste(extra_cols, collapse=", ")), dostop=FALSE)
            res$design <- res$design[, setdiff(names(res$design), extra_cols), drop=FALSE]
        }

        # ── Screening center points (additive; classic 2-level screening
        # designs only) ─────────────────────────────────────────────────────────
        # Adding a handful of center runs (all factors at their midpoint) to a
        # 2-level factorial or Plackett-Burman screening design is the standard
        # way to obtain an independent check for curvature without expanding
        # to a full response-surface design (Montgomery, "Design and Analysis
        # of Experiments", Ch. 6 & 8). Off by default (screeningcenterpts=FALSE)
        # so existing designs are completely unaffected; DSD is excluded here
        # because its standard construction already includes one center run.
        if (isTRUE(screeningcenterpts) && !(designtype %in% c("factorial","plackettburman"))) {
            # Same no-op warning pattern used for GENERATEDESIGN=No (informs
            # the user a keyword had no effect instead of silently ignoring
            # it). The "Screening Options" dialog button isn't restricted to
            # Factorial/Plackett-Burman the way the Optimal-only "Options"
            # button is, because this CDB's ControlCondition only supports a
            # single equals-comparison (confirmed: no OR/in operator is used
            # anywhere in this dialog's XML), so "designtype is Factorial OR
            # Plackett-Burman" can't be expressed as one Enabled condition
            # the way "designtype is Optimal" can. This warning is the
            # runtime substitute for that missing dialog-level greying-out.
            warns$warn(gtxtf(
                "SCREENINGCENTERPTS only applies to Factorial and Plackett-Burman designs; it was ignored for DESIGNTYPE=%s.",
                toupper(designtype)), dostop=FALSE)
        }
        if (screeningcenterpts && designtype %in% c("factorial","plackettburman")) {
            ncp <- if (!is.null(screeningcenterptscount)) screeningcenterptscount else 3
            ctr_vals <- lapply(seq_along(variables), function(i) {
                v   <- as.character(variables[i])
                idx <- which(as.character(spec$var) == v)
                mid <- if (length(idx) == 1) as.numeric(spec$centers[idx]) else NA_real_
                rep(mid, ncp)
            })
            names(ctr_vals) <- as.character(variables)
            ctr_df <- as.data.frame(ctr_vals)
            ctr_df <- cbind(Reps=1, ctr_df)
            missing_cols <- setdiff(names(res$design), names(ctr_df))
            for (mc in missing_cols) ctr_df[[mc]] <- NA
            extra_in_ctr <- setdiff(names(ctr_df), names(res$design))
            if (length(extra_in_ctr) > 0) ctr_df <- ctr_df[, setdiff(names(ctr_df), extra_in_ctr), drop=FALSE]
            ctr_df <- ctr_df[, names(res$design), drop=FALSE]
            # Tag rows so a later analysis run can test curvature: 1 = factorial
            # ("cube") point, 0 = appended center point. Purely additive marker
            # column -- it only exists when screeningcenterpts is turned on (off
            # by default), appended last on both frames so column order matches
            # for rbind, and is excluded from every meta_cols list above so it is
            # never mistaken for a real experimental factor downstream.
            res$design$CenterPt <- 1
            ctr_df$CenterPt     <- 0
            res$design <- rbind(res$design, ctr_df)
        }

        # ── Randomize run order ──────────────────────────────────────────────────
        if (randomize) {
            set.seed(NULL)
            run_order            <- sample(nrow(res$design))
            res$design           <- res$design[run_order, , drop=FALSE]
            res$design$StdOrder  <- as.integer(rownames(res$design))
            res$design$RunOrder  <- seq_len(nrow(res$design))
            rownames(res$design) <- NULL
        } else {
            res$design$StdOrder <- seq_len(nrow(res$design))
            res$design$RunOrder <- seq_len(nrow(res$design))
        }
        
        # ── Reorder columns: StdOrder, RunOrder first, then variables ───────────
        meta_cols <- c("StdOrder", "RunOrder")
        other_cols <- setdiff(names(res$design), meta_cols)
        res$design <- res$design[, c(meta_cols, other_cols), drop=FALSE]

        # ── Blocks ───────────────────────────────────────────────────────────────
        # Skip this generic (statistically arbitrary, run-order-based) block
        # assignment if generate_factorial_FrF2() already set a proper,
        # confounding-aware "Block" column from FrF2's own native blocking --
        # this only fires as the fallback for design types that don't have
        # (or couldn't use) native blocking.
        if (!is.null(blocks) && blocks > 1 && !("Block" %in% names(res$design))) {
            nruns                <- nrow(res$design)
            res$design$Block     <- rep(seq_len(blocks), length.out=nruns)[res$design$RunOrder]
        }

        # ── Replicates for non-FrF2 designs ─────────────────────────────────────
        # BUGFIX 1: "plackettburman", "dsd", and "fullfactorial" were missing
        # from this whitelist -- each of those three generator functions
        # (generate_pb_FrF2/generate_pb_base, generate_dsd, generate_full_
        # factorial) hard-codes Reps=1 and has no internal replicate handling
        # of its own, so REPLICATES=N was silently a no-op for them: the
        # keyword parsed fine and no warning fired, but the generated design
        # just never grew past N=1. Added below.
        #
        # BUGFIX 2 (regression from the fix above -- caught during the same
        # audit before it shipped): "boxbehnken", "ccd", and "rsm" (rsm reuses
        # generate_ccd) must NOT be in this whitelist, even though they always
        # have been. generate_box_behnken() and generate_ccd() already consume
        # `replicates` themselves, internally, as the CENTER POINT COUNT (see
        # their own "ncp <- if (!is.null(replicates)) replicates else 3" lines
        # and their own in-code comments -- "Standard Box-Behnken: 3 center
        # points (not replicates*3)" / "Center points: standard is ncp, not
        # ncp*3" -- both comments explicitly document that this value must NOT
        # also be used as a whole-design multiplier). Because this generic
        # block ran a second time for these three design types anyway, setting
        # REPLICATES=N previously added N center points AND THEN duplicated
        # the entire design (cube/star points included) N times over -- e.g.
        # REPLICATES=3 on a Box-Behnken design silently produced 9 center
        # points and tripled every cube point too, contradicting the
        # generator's own documented intent. This is the same class of gap as
        # "factorial", which was (correctly) already excluded here because it
        # too has its own internal, single-meaning replicate handling.
        #
        # Net effect: REPLICATES now means exactly one thing per design type --
        # "repeat the whole design N times" for optimal/lhs/taguchi/
        # plackettburman/dsd/fullfactorial (none of which have any internal
        # notion of replication), and "use N center points" for boxbehnken/
        # ccd/rsm/factorial (each of which already handles replication/center
        # points correctly on its own).
        if (!is.null(replicates) && replicates > 1 &&
            designtype %in% c("optimal","lhs","taguchi",
                               "plackettburman","dsd","fullfactorial",
                               "simplexlattice","simplexcentroid")) {
            res$design           <- do.call(rbind, replicate(replicates, res$design, simplify=FALSE))
            res$design$RunOrder  <- seq_len(nrow(res$design))
            rownames(res$design) <- NULL
        }
    }  # End of else block for design generation

    # ── Display design summary (skip if reading from existing dataset) ───────
    if (!read_existing) {
        non_meta <- setdiff(names(res$design), c("Reps","Proportion","StdOrder","RunOrder","Block","CenterPt","PtType"))
        if (length(non_meta) > 0 && !any(sapply(res$design[, non_meta, drop=FALSE], is.numeric)))
            center <- FALSE

        displayresults(res, spec, hasformula, variables, vlevels, designtype, model, constraintfunc,
                       constant, criterion, center, initial, repeats, designalg, ncand,
                       outputdataset, frml_built, confounding, warns,
                       exporthtml, htmlpath)

        spsspkg.EndProcedure()
    }

    # ── Analysis phase ────────────────────────────────────────────────────────
    do_any_analysis <- analyze || createplots || maineffects || interactions ||
                       cubeplot || contourplot || residualplots || paretoplot ||
                       curvatureplot || optimizeresponse || exporthtml

    if (do_any_analysis && !is.null(responsevar)) {
        tryCatch({
            # If reading from existing dataset, data already includes response variable
            if (read_existing) {
                # Data already loaded with response variable
                resp_data <- res$design
                # Verify response variable(s) exist (see responsevars_all note above)
                missing_rv <- setdiff(responsevars_all, names(resp_data))
                if (length(missing_rv) > 0) {
                    warns$warn(gtxtf("Response variable(s) not found in dataset: %s",
                                     paste(missing_rv, collapse=", ")), dostop=FALSE)
                    resp_data <- NULL
                }
            } else {
                # For new designs, read response variable(s) from active dataset.
                # Looping is a no-op extension of the original single-call behavior
                # when only one response variable was provided.
                resp_data <- res$design
                for (rv in responsevars_all) {
                    resp_data <- get_response_data(rv, resp_data, warns)
                    if (is.null(resp_data)) break
                }
            }

            # Coded-factor engine: used for every design except mixtures and
            # Taguchi arrays, unless ANALYSISMODEL=LEGACY is requested.
            use_coded <- identical(analysismodel, "coded")
            if (!is.null(resp_data) && use_coded) {
                if (is.null(variables) || length(unlist(variables)) == 0)
                    warns$warn(gtxt("VARNAMES must list the design factors to analyze (all other columns are otherwise ambiguous)."), dostop=TRUE)
                # mixture data: flagged components, simplex design types, or
                # (auto) every factor numeric, non-negative and summing to the
                # same total in every run
                mixflags <- rep(unlist(mixtures) %in% TRUE, length.out=length(unlist(variables)))
                is_mix <- any(mixflags) || designtype %in% c("simplexlattice","simplexcentroid")
                if (!is_mix && length(unlist(variables)) >= 2 && is.null(terms)) {
                    vv <- intersect(tolower(unlist(variables)), tolower(names(resp_data)))
                    cols <- names(resp_data)[match(vv, tolower(names(resp_data)))]
                    if (length(cols) >= 2 && all(vapply(resp_data[cols], is.numeric, logical(1)))) {
                        A <- as.matrix(resp_data[, cols, drop=FALSE]); tot <- rowSums(A)
                        if (nrow(A) >= 3 && all(A >= 0, na.rm=TRUE) && all(is.finite(tot)) && mean(tot) > 0 &&
                            sd(tot) / mean(tot) < 1e-4 && length(unique(round(A[, 1], 9))) > 1) {
                            is_mix <- TRUE
                            warns$warn(gtxt("The factors sum to the same total in every run, so they were analyzed as mixture components. To analyze them as ordinary factors instead, specify the model with TERMS (or use ANALYSISMODEL=LEGACY)."), dostop=FALSE)
                        }
                    }
                }
                snv <- tolower(as.character(unlist(sntype))[1])
                is_tag <- designtype == "taguchi" || (!is.null(sntype) && !is.na(snv) && nzchar(snv) && !snv %in% c("none","item_sn_e"))
                if (!is.null(sntype) && !is.na(snv) && snv %in% c("none","item_sn_e")) sntype <- NULL
                fa_opts <- list(
                    mixture=is_mix, mixflags=if (any(mixflags)) mixflags else NULL, mixturesum=mixturesum,
                    mixmodel=mixmodel, mixcomps=mixcomps, traceplot=traceplot,
                    taguchi=is_tag, sntype=sntype, createplots=createplots,
                    selection=selection, alphaenter=alphaenter, boxcox=boxcox, lambda=lambda,
                    categorical=categorical, designmodel=if (!read_existing) model else NULL, grouping=grouping, surfaceplot=surfaceplot,
                    overlay=overlay, canonical=canonical, robust=robust, robustsd=robustsd, confirmruns=confirmruns,
                    tables=analyze, terms=terms, alpha=alpha, backward=backward, alpharemove=alpharemove,
                    centerterm=centerterm, conflevel=conflevel, lowlevels=lowlevels,
                    lows=if (!is.null(spec)) spec[, 2] else lows,
                    highs=if (!is.null(spec)) spec[, 3] else highs,
                    equation=equation, alias=aliastable, diagnostics=diagtable, factorinfo=factorinfo,
                    pareto=paretoplot, effectsplot=effectsplot, maineffects=maineffects,
                    interactions=interactions, cubeplot=cubeplot, contourplot=contourplot,
                    residualplots=residualplots,
                    optimize=optimizeresponse, goals=optimizationgoals_all,
                    lowers=optlowers,
                    targets=if (multi_response) optimizationtargets_all else optimizationtarget_num,
                    uppers=optuppers, weights=optweights, importance=optimportance,
                    holds=opthold, nsolutions=optsolutions,
                    interpret=interpret, predict=predict,
                    html=exporthtml,
                    htmlpath=if (!is.null(htmlpath) && nzchar(htmlpath)) htmlpath
                             else make_default_html_path("Analysis", primary_responsevar))
                fa_run_analysis(resp_data, variables, responsevars_all, designtype, warns, fa_opts)
            } else if (!is.null(resp_data)) {
                fit <- fit_model(resp_data, variables, designtype, model, warns, primary_responsevar)
                if (!is.null(fit)) {
                    if (analyze)
                        tryCatch(display_analysis(fit, resp_data, variables, warns, primary_responsevar,
                                                   effectstable=effectstable, residualtests=residualtests,
                                                   viftable=viftable, screeningsummary=screeningsummary,
                                                   varselect=varselect, varselmethod=varselmethod, stepdir=stepdir),
                                 error=function(e) {
                                     tryCatch(spsspkg.EndProcedure(), error=function(x) NULL)
                                     warns$warn(gtxtf("Analysis display error: %s", e$message), dostop=FALSE)
                                 })

                    # ── Optimizer: TABLES phase ───────────────────────────────────────
                    # Runs the optimizer and shows its TABLES here (right after the
                    # analysis tables above), but defers its CHART (returned as a
                    # closure) until after create_all_plots() below -- see the
                    # "Optimizer: PLOT phase" comment further down. This groups every
                    # table in the output before any chart, instead of the previous
                    # order where the optimizer's own chart landed in between
                    # create_all_plots()'s charts and create_varselect_plots()'s
                    # charts. do_optimization()/do_multi_response_optimization()
                    # otherwise behave exactly as before -- same computation, same
                    # tables, same gating -- only WHEN their chart is drawn changed.
                    opt_plot_fn <- NULL
                    if (optimizeresponse) {
                        if (multi_response) {
                            # NEW: simultaneous multi-response optimization with composite
                            # desirability -- a true sibling of do_optimization(), which
                            # remains untouched and is still used for the (default,
                            # unchanged) single-response case below.
                            opt_plot_fn <- tryCatch({
                                fits_list <- list()
                                for (rv in responsevars_all)
                                    fits_list[[rv]] <- fit_model(resp_data, variables, designtype, model, warns, rv)
                                do_multi_response_optimization(fits_list, resp_data, variables, spec,
                                                optimizationgoals_all, warns, responsevars_all,
                                                optdetailtable=optdetailtable,
                                                contourplot=(contourplot || createplots),
                                                targets=optimizationtargets_all)
                            }, error=function(e) {
                                tryCatch(spsspkg.EndProcedure(), error=function(x) NULL)
                                warns$warn(gtxtf("Multi-response optimizer error: %s", e$message), dostop=FALSE)
                                NULL
                            })
                        } else {
                            opt_plot_fn <- tryCatch(do_optimization(fit, resp_data, variables, spec,
                                                    optimizationgoal, warns, primary_responsevar,
                                                    optdetailtable=optdetailtable,
                                                    target=optimizationtarget_num),
                                     error=function(e) {
                                         tryCatch(spsspkg.EndProcedure(), error=function(x) NULL)
                                         warns$warn(gtxtf("Optimizer error: %s", e$message), dostop=FALSE)
                                         NULL
                                     })
                        }
                    }

                    # BUGFIX: each chart checkbox used to be OR'd against CREATEPLOTS
                    # ("Create Diagnostic Plots"), so checking that one box forced every
                    # chart on regardless of which individual boxes the user had
                    # unchecked -- unchecking a specific chart had no effect whenever
                    # CREATEPLOTS was also on. CREATEPLOTS still contributes to the
                    # top-level "should we attempt any plots at all" gate just below
                    # (harmless — it only ever makes that gate MORE permissive, never
                    # suppresses anything), but no longer overrides an individually
                    # unchecked box. Each checkbox now purely controls its own chart.
                    if (createplots || maineffects || interactions || cubeplot ||
                        contourplot || residualplots || paretoplot || curvatureplot)
                        tryCatch(create_all_plots(fit, resp_data, variables, designtype,
                                         maineffects,
                                         interactions,
                                         cubeplot,
                                         contourplot,
                                         residualplots,
                                         paretoplot, warns, externalplots, primary_responsevar,
                                         curvatureplot),
                                 error=function(e) {
                                     tryCatch(spsspkg.EndProcedure(), error=function(x) NULL)
                                     warns$warn(gtxtf("Plot error: %s", e$message), dostop=FALSE)
                                 })

                    # ── Optimizer: PLOT phase ─────────────────────────────────────────
                    # Draws the optimizer's own chart (Response Optimization curve/bar,
                    # or the multi-response Overlaid Contour Plot) now that every table
                    # from both the analysis phase and the optimizer's own tables phase
                    # above have already been shown -- see "Optimizer: TABLES phase".
                    if (!is.null(opt_plot_fn))
                        tryCatch(opt_plot_fn(),
                                 error=function(e) {
                                     tryCatch(spsspkg.EndProcedure(), error=function(x) NULL)
                                     warns$warn(gtxtf("Optimizer plot error: %s", e$message), dostop=FALSE)
                                 })

                    if (exporthtml)
                        # NOTE: the interactive HTML report's sections now mirror the same
                        # chart checkboxes ("Perform Factorial Analysis" panel) that control
                        # the SPSS Viewer output, so unchecking a chart there also removes
                        # the matching section from the HTML report -- previously every
                        # section was hardcoded to always appear (TRUE, TRUE, TRUE, TRUE,
                        # TRUE, TRUE below), regardless of checkbox state, which is what
                        # made "all charts show in HTML even when unselected" happen.
                        # (HTMLPATH, typed manually in syntax, still overrides the default
                        # save location.) The report is always built from the primary
                        # (first) response variable, exactly as before, even in
                        # multi-response mode.
                        tryCatch(export_html_report(fit, resp_data, variables, designtype,
                                                warns, primary_responsevar,
                                                if (!is.null(htmlpath) && nzchar(htmlpath)) htmlpath
                                                else make_default_html_path("Analysis", primary_responsevar),
                                                maineffects, interactions, contourplot,
                                                residualplots, paretoplot, cubeplot,
                                                if (optimizeresponse && !multi_response) optimizationgoal else NULL,
                                                spec,
                                                do_curvature=curvatureplot,
                                                varselect=isTRUE(varselect),
                                                varselmethod=varselmethod, stepdir=stepdir),
                                 error=function(e) {
                                     tryCatch(spsspkg.EndProcedure(), error=function(x) NULL)
                                     warns$warn(gtxtf("HTML report error: %s", e$message), dostop=FALSE)
                                 })
                # ── Variable Selection Charts ───────────────────────────────────────
                if (isTRUE(varselect) && (isTRUE(selectiontrace) || isTRUE(fitprofile) ||
                                          isTRUE(parsimonyplot)  || isTRUE(factormap))) {
                    tryCatch({
                        vsel_plt <- compute_variable_selection(
                            fit, varselmethod=varselmethod, stepdir=stepdir, warns=warns)
                        create_varselect_plots(vsel_plt,
                            do_selectiontrace=isTRUE(selectiontrace),
                            do_fitprofile=isTRUE(fitprofile),
                            do_parsimonyplot=isTRUE(parsimonyplot),
                            do_factormap=isTRUE(factormap),
                            warns=warns)
                    }, error=function(e) {
                        tryCatch(spsspkg.EndProcedure(), error=function(x) NULL)
                        warns$warn(gtxtf("Variable selection charts error: %s", e$message), dostop=FALSE)
                    })
                }
                }
            }
        }, error=function(e) {
            # Guarantee procedure is closed before gendataset runs
            tryCatch(spsspkg.EndProcedure(), error=function(x) NULL)
            warns$warn(gtxtf("Analysis error: %s", e$message), dostop=FALSE)
        })
    }

    # ── Save dataset ──────────────────────────────────────────────────────────
    # Safety net: ensure no procedure is open before creating dataset
    tryCatch(spsspkg.EndProcedure(), error=function(e) NULL)

    # Analysis of an existing dataset: remember the factor list in the dataset
    # (data file attribute DOE_Factors) when the user named the factors, so the
    # next analysis of the same data can leave VARNAMES blank.
    if (read_existing && isTRUE(analyze) && identical(factor_source, "given") && length(variables))
        .doe_store_factors(as.character(unlist(variables)), warns)
    
    # Save dataset (only for new designs, not when reading existing)
    if (!read_existing) {
        gendataset(res, outputdataset, variables, factorlist, warns, designtype=designtype)
    }
    
    warns$display(inproc=FALSE)
    tryCatch(rm(list=ls()), warning=function(e) NULL)
}

# ════════════════════════════════════════════════════════════════════════════
# DESIGN GENERATION FUNCTIONS
# ════════════════════════════════════════════════════════════════════════════

generate_optimal <- function(spec, variables, frml, ntrials, designalg,
                              mixturesum, criterion, initial, repeats, ncand, dfilter, warns) {
    # AlgDesign's optMonteCarlo() -> optFederov() has a real upstream quirk that
    # only shows up when the WHOLE design has exactly one variable (nrow(spec)==1):
    # optFederov() receives the candidate set as a plain matrix (not a data.frame),
    # and contains the hardcoded line
    #     data<-data.frame(data); if (ncol(data)==1) colnames(data)<-"X1"
    # which forcibly renames the design's only column to "X1" -- discarding
    # whatever the real variable name was -- before evaluating model.matrix(frml,data).
    # Meanwhile optMonteCarlo's OWN internal expand()/centering helper builds its
    # candidate data.frame the normal way and keeps the real variable name. So the
    # the same `frml` object is evaluated against "X1" in one internal call and
    # against the real name in another, and whichever one doesn't match throws
    # "object '<name>' not found". We can't patch AlgDesign, so we sidestep the
    # mismatch: for the single-variable case only, rename the variable to "X1" in
    # both the data and the formula passed to optMonteCarlo (so every internal
    # AlgDesign call site agrees), then rename the result's column back afterward.
    # AlgDesign's optMonteCarlo() hard-stops with "Cannot use RandomStart==FALSE
    # with approximate==TRUE" -- it does not support combining an approximate-
    # theory design (DESIGNALG=APPROX) with a nullification start (INITIAL=
    # NULLIFICATION). Surface this as a clear message using our own keyword
    # names instead of letting AlgDesign's internal-argument-named error
    # through, and catch it before the (slower) design-generation attempt.
    if (designalg == "approx" && initial != "random") {
        warns$warn(gtxt("INITIAL=NULLIFICATION cannot be combined with DESIGNALG=APPROX (AlgDesign does not support a nullification start for approximate-theory designs). Use INITIAL=RANDOM with DESIGNALG=APPROX, or use DESIGNALG=EXACT with INITIAL=NULLIFICATION."), dostop=TRUE)
    }
    single_var_rename <- nrow(spec) == 1
    if (single_var_rename) {
        realname    <- as.character(spec$var[[1]])
        spec$var[1] <- "X1"
        frml        <- gsub(realname, "X1", frml, fixed=TRUE)
    }
    arglist <- list(
        frml        = as.formula(frml),
        data        = spec,
        nTrials     = ntrials,
        approximate = designalg == "approx",
        evaluateI   = FALSE,
        mixtureSum  = mixturesum,
        criterion   = criterion,
        RandomStart = initial == "random",
        nRepeats    = repeats,
        DFrac=1, CFrac=1
    )
    if (is.null(ntrials))  arglist["nTrials"]     <- NULL
    if (!is.null(dfilter)) arglist[["constraints"]] <- dfilter
    if (!is.null(ncand))   arglist["nCand"]       <- ncand
    res <- tryCatch(do.call(optMonteCarlo, arglist),
             error=function(e) {
                 # "system is computationally singular: reciprocal condition
                 # number = ..." comes straight out of AlgDesign's internal
                 # matrix-inversion step for the A/I-optimality criteria (D
                 # doesn't need a full inverse, which is why it's more robust).
                 # It means the candidate set couldn't support a non-singular
                 # fit for this model -- give the user concrete next steps
                 # instead of a bare LAPACK message.
                 if (grepl("exactly singular|Singular design", e$message)) {
                     warns$warn(gtxtf(
                         "Optimal design generation failed because the model cannot be estimated from the candidate points (%s). Usually a squared or higher term has too few LEVELS (use 3 or more for that variable), or TRIALS is smaller than the number of model terms.",
                         e$message), dostop=TRUE)
                 } else if (grepl("computationally singular|reciprocal condition number", e$message)) {
                     warns$warn(gtxtf(
                         "Optimal design generation failed: the candidate set was too close to singular to invert for this CRITERION/model combination (%s). This is most common with CRITERION=A or CRITERION=I on a higher-order (quad/cubic) model with DESIGNALG=APPROX and CENTER=NO. Try CENTER=YES, a larger NUMCAND, more REPEATS, or CRITERION=D (which does not require a full matrix inverse and is more numerically robust).",
                         e$message), dostop=TRUE)
                 } else {
                     warns$warn(e$message, dostop=TRUE)
                 }
             })
    if (single_var_rename && !is.null(res) && !is.null(res$design) && "X1" %in% names(res$design)) {
        names(res$design)[names(res$design)=="X1"] <- realname
    }
    res
}

generate_factorial_FrF2 <- function(spec, variables, factors, ntrials, blocks, replicates, warns) {
    nvars    <- length(variables)
    factors  <- coerce_yesno(factors)
    nfactors <- sum(factors)
    if (nfactors < 2) warns$warn(gtxt("Factorial design requires at least 2 factors"), dostop=TRUE)
    excluded <- as.character(variables)[!factors]
    if (length(excluded) > 0) {
        warns$warn(gtxtf(
            "The following variable(s) were not flagged as Factor and are excluded from the factorial design: %s",
            paste(excluded, collapse=", ")), dostop=FALSE)
    }
    nreps <- if (!is.null(replicates)) replicates else 1
    nblk  <- if (!is.null(blocks)) blocks else 1

    # BUGFIX: BLOCKS used to only reach FrF2's own native blocking in the
    # no-TRIALS branch, and even there its "Blocks" column was never
    # extracted safely (see below) -- confirmed against FrF2's actual
    # source (R/FrF2.R) that FrF2's native block-assignment column is
    # literally named "Blocks" and is PREPENDED as the very first column
    # whenever blocks>1, not appended at the end. The old code only knew
    # how to safely remove a trailing "Reps" column and then blindly kept
    # "the first nfactors columns" as the real factors -- if Blocks ever
    # landed in that window, it would have been silently renamed into a
    # real factor (with its 1/2 block index rescaled into fake low/high
    # values) while the true last factor was dropped entirely. Fixed by:
    # extracting every known metadata column (Reps, Blocks) BY NAME before
    # any positional factor logic runs, verifying the constraints FrF2
    # itself enforces (blocks must be a power of 2; nfactors+blocks-1 must
    # be less than the run count) before ever requesting native blocking so
    # an infeasible combination degrades gracefully with a clear warning
    # instead of an uncaught error, and hard-stopping with a diagnostic (not
    # a silent mis-assignment) if the column count doesn't come out exactly
    # right afterward.
    blocks_ok <- function(nruns_val) {
        nblk == 1 ||
            (nblk > 0 && log2(nblk) == round(log2(nblk)) && (nfactors + nblk - 1) < nruns_val)
    }
    use_native_blocking <- nblk > 1

    design <- if (is.null(ntrials)) {
        nruns_full <- 2^nfactors
        if (use_native_blocking && !blocks_ok(nruns_full)) {
            warns$warn(gtxtf(
                "BLOCKS=%d is not achievable for a %d-run, %d-factor factorial design (the number of blocks must be a power of 2, and factors + blocks - 1 must be less than the run count). Generating without native blocking instead.",
                nblk, nruns_full, nfactors), dostop=FALSE)
            use_native_blocking <- FALSE
        }
        tryCatch(
            if (use_native_blocking)
                FrF2(nruns=nruns_full, nfactors=nfactors, replications=nreps,
                     blocks=nblk, randomize=FALSE)
            else
                FrF2(nruns=nruns_full, nfactors=nfactors, replications=nreps, randomize=FALSE),
            error=function(e) FrF2(nfactors=nfactors, resolution=4,
                                   replications=nreps, randomize=FALSE))
    } else {
        if (use_native_blocking && !blocks_ok(ntrials)) {
            warns$warn(gtxtf(
                "BLOCKS=%d is not achievable for a %d-run, %d-factor factorial design (the number of blocks must be a power of 2, and factors + blocks - 1 must be less than the run count). Generating without native blocking instead.",
                nblk, ntrials, nfactors), dostop=FALSE)
            use_native_blocking <- FALSE
        }
        tryCatch(
            if (use_native_blocking)
                FrF2(nruns=ntrials, nfactors=nfactors, replications=nreps,
                     blocks=nblk, randomize=FALSE)
            else
                FrF2(nruns=ntrials, nfactors=nfactors, replications=nreps, randomize=FALSE),
            error=function(e) {
                warns$warn(gtxtf("FrF2 blocked-design generation failed (%s); generating without native blocking instead.", e$message), dostop=FALSE)
                FrF2(nruns=ntrials, nfactors=nfactors, replications=nreps, randomize=FALSE)
            })
    }

    df         <- as.data.frame(design)
    factor_vars <- as.character(variables)[factors]

    # Extract known FrF2 metadata columns BY NAME (never by position) --
    # safe regardless of where FrF2 places them or whether a given FrF2
    # version adds them at all.
    has_reps_col <- "Reps" %in% names(df)
    if (has_reps_col) {
        reps_col <- df$Reps
        df <- df[, setdiff(names(df), "Reps"), drop=FALSE]
    } else {
        reps_col <- rep(1, nrow(df))
    }

    has_blocks_col <- "Blocks" %in% names(df)
    if (has_blocks_col) {
        block_col <- suppressWarnings(as.integer(as.character(df$Blocks)))
        df <- df[, setdiff(names(df), "Blocks"), drop=FALSE]
    } else {
        block_col <- NULL
    }

    # Defensive check: after removing known metadata columns by name,
    # exactly nfactors columns should remain. If not (e.g. an unexpected
    # extra column from a different FrF2 version than the one this was
    # verified against), stop with a clear diagnostic instead of silently
    # mis-assigning a metadata column as a treatment factor.
    if (ncol(df) != nfactors) {
        warns$warn(gtxtf(
            "Unexpected column layout returned by FrF2 (expected %d factor column(s) after removing Reps/Blocks, found %d: %s). Design generation stopped to avoid producing an incorrect design.",
            nfactors, ncol(df), paste(names(df), collapse=", ")), dostop=TRUE)
    }

    # Process factor columns - convert coded values to actual levels
    for (i in seq_along(factor_vars)) {
        v    <- factor_vars[i]
        idx  <- which(as.character(spec$var) == v)
        low  <- as.numeric(spec$lows[idx])
        high <- as.numeric(spec$highs[idx])
        col  <- df[[i]]

        # FrF2 returns ordered factors with levels "-1" and "1"
        # Convert via levels, NOT as.integer() which gives 1/2 not -1/1
        coded <- as.numeric(as.character(col))  # "-1" -> -1, "1" -> 1
        df[[i]] <- ifelse(coded == -1, low, high)
        names(df)[i] <- v
    }

    # Keep only the factor columns (now with proper names and values) --
    # by this point ncol(df)==nfactors is already guaranteed above, so this
    # is a defensive no-op rather than the sole safeguard it used to be.
    df <- df[, seq_len(nfactors), drop=FALSE]

    # Add Reps column back at the beginning with correct replicate numbers
    df <- cbind(Reps=reps_col, df)
    # Restore FrF2's native Block assignment, if any -- named "Block"
    # (singular) to match this file's existing meta-column convention used
    # everywhere else (e.g. meta_cols <- c("Reps","Proportion","StdOrder",
    # "RunOrder","Block","CenterPt","PtType")).
    if (!is.null(block_col)) df$Block <- block_col
    # NOTE: 'design' (the FrF2-classed object, pre-as.data.frame) is preserved as design_obj
    # so alias/confounding structure can later be derived via FrF2::design.info()/aliases()
    # without needing to regenerate the design. This is additive only -- existing consumers
    # of generate_factorial_FrF2()'s return value only read $design/$D/$A/$Ge/$Dea and are unaffected.
    list(design=df, D=NA, A=NA, Ge=NA, Dea=NA, design_obj=design)
}

generate_factorial_base <- function(spec, variables, factors, replicates, warns) {
    # Ensure factors is properly coerced
    factor_flags <- coerce_yesno(factors)
    factor_idx   <- which(factor_flags)
    if (length(factor_idx) < 2) warns$warn(gtxt("Factorial design requires at least 2 factors"), dostop=TRUE)
    excluded <- as.character(variables)[!factor_flags]
    if (length(excluded) > 0) {
        warns$warn(gtxtf(
            "The following variable(s) were not flagged as Factor and are excluded from the factorial design: %s",
            paste(excluded, collapse=", ")), dostop=FALSE)
    }
    nreps <- if (!is.null(replicates)) replicates else 1
    
    levels_list <- list()
    for (i in factor_idx) {
        v   <- as.character(spec$var[i])
        nlv <- as.integer(spec$nlevels[i])
        levels_list[[v]] <- seq(as.numeric(spec$lows[i]), as.numeric(spec$highs[i]), length.out=nlv)
    }
    # expand.grid returns columns in order
    df <- expand.grid(levels_list)
    
    # CRITICAL FIX: Handle replicates properly
    if (nreps > 1) {
        # Replicate the entire design
        base_design <- df
        df <- do.call(rbind, replicate(nreps, base_design, simplify=FALSE))
        # Add Reps column with proper numbering
        reps_col <- rep(seq_len(nreps), each=nrow(base_design))
        df <- cbind(Reps=reps_col, df)
    } else {
        df <- cbind(Reps=1, df)
    }
    
    list(design=df, D=NA, A=NA, Ge=NA, Dea=NA)
}

# ── General k-level Full Factorial design ───────────────────────────────────
# A dedicated design type (NOT a modification of "factorial" above), because
# the existing FrF2-backed path hard-codes 2 levels per factor
# (FrF2(nruns=2^nfactors,...)) regardless of the LEVELS property -- so a user
# who set LEVELS>2 silently still got a 2-level design whenever the FrF2
# package was installed. This generator instead always honors each factor's
# individual level count (reusing the existing LEVELS/nlevels Property, the
# same convention already shared by lows/highs/centers/roundtos), mapping
# levels onto evenly spaced points between that factor's low/high bounds via
# expand.grid -- the identical, already-proven technique used by
# generate_factorial_base()'s FrF2-unavailable fallback, just applied
# generally and made into its own selectable design type.
generate_full_factorial <- function(spec, variables, warns) {
    nvars <- length(variables)
    if (nvars < 1) warns$warn(gtxt("Full Factorial design requires at least 1 factor"), dostop=TRUE)

    levels_list <- list()
    for (i in seq_len(nvars)) {
        v   <- as.character(spec$var[i])
        nlv <- suppressWarnings(as.integer(spec$nlevels[i]))
        if (is.na(nlv) || nlv < 2) nlv <- 2
        levels_list[[v]] <- seq(as.numeric(spec$lows[i]), as.numeric(spec$highs[i]), length.out=nlv)
    }
    df <- expand.grid(levels_list)
    total_runs <- nrow(df)
    if (total_runs > 5000) {
        warns$warn(gtxtf(
            "The full factorial design requires %d runs (the product of all factor level counts). Consider reducing the number of levels or factors, or use a fractional design (DESIGNTYPE=FACTORIAL) instead.",
            total_runs), dostop=FALSE)
    }
    df <- cbind(Reps=1, df)
    list(design=df, D=NA, A=NA, Ge=NA, Dea=NA)
}

generate_pb_FrF2 <- function(spec, variables, ntrials, warns) {
    nvars <- length(variables)
    # Default run size: smallest multiple of 4 above the factor count, but at
    # least 12 -- the 8-run Plackett-Burman is just a regular 2^(k-p) fraction
    # (Minitab's PB catalog also starts at 12 runs). An explicit NTRIALS is kept.
    if (is.null(ntrials)) ntrials <- max(12, ceiling((nvars+1)/4)*4)
    design <- tryCatch(withCallingHandlers(pb(nruns=ntrials, nfactors=nvars),
                           warning=function(w) {
                               warns$warn(gsub("\\s+", " ", conditionMessage(w)), dostop=FALSE)
                               invokeRestart("muffleWarning") }),
                       error=function(e) warns$warn(e$message, dostop=TRUE))
    df <- as.data.frame(design)
    for (i in seq_along(variables)) {
        v   <- as.character(variables[i])
        low <- as.numeric(spec$lows[i]); high <- as.numeric(spec$highs[i])
        col <- df[[i]]
        # pb() also returns ordered factors with levels "-1" and "1"
        coded <- as.numeric(as.character(col))
        df[[i]] <- ifelse(coded == -1, low, high)
        names(df)[i] <- v
    }
    df <- df[, seq_along(variables), drop=FALSE]
    df <- cbind(Reps=1, df)
    # design_obj preserved (see generate_factorial_FrF2 note) for partial-aliasing analysis
    list(design=df, D=NA, A=NA, Ge=NA, Dea=NA, design_obj=design)
}

generate_pb_base <- function(spec, variables, ntrials, warns) {
    nvars <- length(variables)
    if (!is.null(ntrials) && ntrials != 12) {
        warns$warn(gtxtf(
            "Plackett-Burman fallback (FrF2 package unavailable) only supports a 12-run design; the requested TRIALS=%d was ignored and 12 runs were generated instead. Install the FrF2 package to get other run sizes (8, 16, 20, 24, ...).",
            ntrials), dostop=FALSE)
    }
    ntrials <- 12
    gen <- c(1,1,-1,1,1,1,-1,-1,-1,1,-1)
    mat <- matrix(0, 12, 11)
    for (i in 1:11) mat[i,] <- c(gen[i:11], gen[seq_len(i-1)])
    mat[12,] <- -1
    mat <- mat[, seq_len(min(nvars,11)), drop=FALSE]
    df  <- as.data.frame(mat)
    for (i in seq_along(variables)) {
        v   <- as.character(variables[i])
        low <- as.numeric(spec$lows[i]); high <- as.numeric(spec$highs[i])
        df[[i]] <- ifelse(df[[i]]==-1, low, high)
        names(df)[i] <- v
    }
    df <- cbind(Reps=1, df)
    list(design=df, D=NA, A=NA, Ge=NA, Dea=NA)
}

# ── Definitive Screening Design (DSD) construction ──────────────────────────
# Implements the general Paley conference-matrix construction described in
# Jones & Nachtsheim (2011, "A Class of Three-Level Designs for Definitive
# Screening in the Presence of Second-Order Effects", J. Qual. Technol. 43(1))
# and the underlying conference-matrix theory of Paley (1933). No new package
# dependency is introduced (consistent with this extension's established
# preference for hand-rolled implementations).
#
# For m factors, a DSD is built from an (m2 x m2) conference matrix B (zero
# diagonal, +-1 off-diagonal, B'B = (m2-1)*I) where m2 is the smallest size
# >= m for which such a matrix can be constructed via the Paley method (i.e.
# m2-1 is an odd prime). Runs = one center point + a foldover pair (+B_i,-B_i)
# for every row i of B, restricted to the first m columns (factors) -- total
# runs N = 2*m2 + 1. When an exact conference matrix of order m happens to
# exist (m2 == m) this yields the textbook-orthogonal DSD; otherwise the next
# constructible size is used and the extra columns are simply discarded, the
# same "projection" strategy used by commercial DOE software when no exact
# conference matrix exists at the requested factor count.
.doe_is_prime <- function(n) {
    if (n < 2) return(FALSE)
    if (n %in% c(2,3)) return(TRUE)
    if (n %% 2 == 0) return(FALSE)
    lim <- floor(sqrt(n))
    # BUGFIX: for n = 5 or 7 (the only odd, non-2/3 values with sqrt(n) < 3),
    # lim is 2, so seq(3, lim, by=2) asks to count from 3 up to 2 with a
    # positive step -- an invalid sequence that R rejects with "wrong sign
    # in 'by' argument" instead of silently returning an empty vector. Both
    # 5 and 7 are themselves prime and have no odd divisor candidate to test
    # in that range, so the correct result at this point is simply TRUE.
    # This affected any factor count whose search reached q=5 or q=7 (e.g.
    # DSD designs with 5 or 6 factors, since m2=6 -> q=5), causing a hard
    # crash rather than a design.
    if (lim < 3) return(TRUE)
    for (d in seq(3, lim, by=2)) if (n %% d == 0) return(FALSE)
    TRUE
}

.doe_modpow <- function(base, exp, mod) {
    base <- base %% mod
    result <- 1
    while (exp > 0) {
        if (exp %% 2 == 1) result <- (result * base) %% mod
        exp  <- exp %/% 2
        base <- (base * base) %% mod
    }
    result
}

.doe_legendre_symbol <- function(x, q) {
    x <- x %% q
    if (x == 0) return(0)
    r <- .doe_modpow(x, (q-1)/2, q)
    if (r == 1) 1 else -1
}

# Builds the (q+1) x (q+1) Paley conference matrix for an odd prime q.
.doe_build_paley_conference <- function(q) {
    n <- q + 1
    C <- matrix(0, nrow=n, ncol=n)
    for (i in 0:(q-1)) {
        for (j in 0:(q-1)) {
            if (i != j) C[i+1, j+1] <- .doe_legendre_symbol(i - j, q)
        }
    }
    sign_inf <- if (q %% 4 == 1) 1 else -1   # symmetric (q==1 mod 4) vs skew (q==3 mod 4)
    for (i in 0:(q-1)) {
        C[i+1, n] <- 1
        C[n, i+1] <- sign_inf
    }
    C[n, n] <- 0
    C
}

# Finds the smallest m2 >= m such that q = m2-1 is an odd prime (a conference
# matrix of that order can be built via Paley's method). Searches a generous
# window since odd primes are dense; m must be >= 4 for a usable DSD.
.doe_find_conference_order <- function(m, search_window=80) {
    for (m2 in m:(m + search_window)) {
        q <- m2 - 1
        if (q >= 3 && q %% 2 == 1 && .doe_is_prime(q)) return(m2)
    }
    NA_integer_
}

generate_dsd <- function(spec, variables, warns) {
    m <- length(variables)
    if (m < 4)
        warns$warn(gtxt("Definitive Screening Designs require at least 4 factors."), dostop=TRUE)

    m2 <- .doe_find_conference_order(m)
    if (is.na(m2))
        warns$warn(gtxtf(
            "Could not construct a Definitive Screening Design for %d factors (no suitable conference matrix size was found). Try a different number of factors.",
            m), dostop=TRUE)
    q <- m2 - 1

    Bfull <- .doe_build_paley_conference(q)      # m2 x m2, diag 0, off-diag +-1
    Bcols <- Bfull[, seq_len(m), drop=FALSE]     # use first m columns as factors

    n_runs <- 2*m2 + 1
    coded  <- matrix(0, nrow=n_runs, ncol=m)
    coded[2:(m2+1), ]        <- Bcols
    coded[(m2+2):(2*m2+1), ] <- -Bcols
    # row 1 stays all-zero: the center run

    colnames(coded) <- as.character(variables)
    df <- as.data.frame(coded)

    for (i in seq_along(variables)) {
        v    <- as.character(variables[i])
        idx  <- which(as.character(spec$var) == v)
        low  <- as.numeric(spec$lows[idx])
        high <- as.numeric(spec$highs[idx])
        mid  <- if (length(idx) == 1 && !is.na(spec$centers[idx])) as.numeric(spec$centers[idx]) else (low+high)/2
        col  <- df[[v]]
        df[[v]] <- ifelse(col == -1, low, ifelse(col == 1, high, mid))
    }

    df <- cbind(Reps=1, df)

    if (m2 != m)
        warns$warn(gtxtf(
            "No conference matrix of exactly %d factors exists; a Definitive Screening Design was built from the next constructible size (%d) and projected down to %d factors, using %d runs instead of the theoretical minimum of %d.",
            m, m2, m, n_runs, 2*m+1), dostop=FALSE)

    list(design=df, D=NA, A=NA, Ge=NA, Dea=NA, dsd_conf_order=m2)
}

# ── Alias / confounding structure for factorial & Plackett-Burman designs ───
# Additive feature (does not alter any existing default output). Computes the
# alias/confounding structure of a 2-level factorial (FrF2()) or PB (pb())
# design directly from the design matrix -- this is a property of the design
# itself (which columns/products of columns are collinear), not of any fitted
# response, so it can be derived safely at design-generation time using a
# placeholder response (the actual values used for the placeholder do not
# affect which effects are aliased -- only the X matrix's rank structure does;
# see Box, Hunter & Hunter, "Statistics for Experimenters", Ch. 6, and the
# documented behavior of FrF2::aliases()).
#   - Regular fractional factorials (resolution III/IV/V/full): effects are
#     FULLY aliased in clean chains (e.g. "A = BC"). Recovered via FrF2::aliases().
#   - Plackett-Burman designs (and any non-regular fraction): main effects are
#     PARTIALLY aliased with many two-factor interactions simultaneously
#     (typical correlation magnitude 1/3 for 12-run PB) -- FrF2::aliases()
#     documents that it errors on such fits, so instead we report the
#     numeric main-effect-by-2FI correlation matrix directly, the standard
#     textbook treatment of PB confounding (Montgomery, "Design and Analysis
#     of Experiments", Ch. 8).
compute_alias_structure <- function(res, designtype, warns) {
    dt <- tolower(designtype)
    if (!(dt %in% c("factorial","plackettburman","fullfactorial"))) return(NULL)
    # Full factorial designs have no aliasing by definition: every main effect
    # and interaction is independently estimable regardless of factor count or
    # level count.  Return an informational note immediately without attempting
    # any FrF2 computation (which may not apply to multi-level full factorials).
    if (dt == "fullfactorial") {
        return(list(type="full", resolution="Full", chains=character(0), matrix=NULL,
                    note=gtxt("Full factorial design: all main effects and interactions are independently estimable. No aliasing or confounding exists.")))
    }
    # BUGFIX/enhancement: this used to bail out to NULL (skipping the whole
    # supplementary table) whenever FrF2 wasn't installed, even though only
    # two specific calls below actually need it (FrF2::design.info() for the
    # formal Resolution number, and FrF2::aliases() for clean "A = BC" alias-
    # chain text). The correlation-matrix path a few lines down already
    # exists purely in base R (model.matrix()+cor()) for Plackett-Burman/non-
    # regular fractions, and both the SPSS Viewer pivot table
    # (templateName "DOEALIASPARTIAL") and the HTML report's "Partial
    # Aliasing Matrix" heatmap already know how to display that exact shape.
    #
    # Two things had to change to make that fallback actually reachable when
    # FrF2 is unavailable:
    #   1. FrF2 is skipped, not the whole function (resolution/al stay
    #      NA/NULL below, and everything falls through to the correlation-
    #      matrix path instead of skipping the table entirely).
    #   2. res$design_obj itself is only populated by the FrF2-based
    #      generators (generate_factorial_FrF2/generate_pb_FrF2) -- when
    #      FrF2 is unavailable, design generation already fell back to
    #      generate_factorial_base()/generate_pb_base(), neither of which
    #      sets design_obj at all. So this now also falls back to res$design
    #      (the plain generated design frame) when design_obj is absent.
    #      Correlation is invariant to the affine low/high rescaling used to
    #      build res$design from coded levels, so this gives an equivalent
    #      confounding result to using a coded design_obj directly -- it's
    #      just working from the real factor values instead of a -1/+1
    #      coding, which doesn't change any correlation coefficient.
    if (is.null(res$design_obj) && is.null(res$design)) return(NULL)

    out <- tryCatch({
        using_design_obj <- !is.null(res$design_obj)
        src_obj <- if (using_design_obj) res$design_obj else res$design
        ddf <- as.data.frame(src_obj)
        # Center-point rows (SCREENINGCENTERPTS) carry no aliasing
        # information -- they exist for curvature detection, a separate
        # purpose -- and were never part of design_obj to begin with (it is
        # captured before that post-processing step runs), so they are
        # excluded here too when falling back to res$design, to keep the
        # confounding calculation equivalent either way.
        if (!using_design_obj && "CenterPt" %in% names(ddf))
            ddf <- ddf[ddf$CenterPt != 0, , drop=FALSE]
        # Same meta-column list used elsewhere in this file (e.g. the HTML
        # design-space scatterplot) so any column added by RANDOMIZE,
        # SCREENINGCENTERPTS, or blocking is correctly excluded rather than
        # mistaken for a factor column when using res$design as the source.
        meta_cols <- c("Reps","Proportion","StdOrder","RunOrder","Block","Blocks","CenterPt","PtType")
        fcols <- setdiff(names(ddf), meta_cols)
        if (length(fcols) < 2) return(NULL)
        # BUGFIX: when using_design_obj is TRUE, these columns come straight
        # from FrF2::FrF2()/FrF2::pb(), which return ordered FACTORS coded
        # "-1"/"1" (same coding this file already converts elsewhere, e.g.
        # generate_factorial_FrF2()'s "FrF2 returns ordered factors with
        # levels '-1' and '1'" conversion). model.matrix() names a factor's
        # dummy column after its LEVEL (e.g. "Temp1"), not the bare variable
        # name, so the later mm[, fcols, ...] / mm[, twofi_cols, ...]
        # lookups by variable name would fail with "subscript out of
        # bounds" whenever FrF2 was actually available for design
        # generation. Converting to numeric here (identical -1/+1 values,
        # just as numbers instead of factor labels) keeps every downstream
        # model.matrix() column named exactly after its source variable and
        # does not change the confounding result -- correlation is
        # unaffected by this representation change. Columns that are
        # already numeric (the res$design fallback path, and any future
        # source) are left untouched.
        for (v in fcols) {
            col <- ddf[[v]]
            if (is.factor(col) || is.character(col)) {
                num <- suppressWarnings(as.numeric(as.character(col)))
                if (!anyNA(num)) ddf[[v]] <- num
            }
        }
        ddf$.doe_dummy_y <- seq_len(nrow(ddf))
        frml_txt <- sprintf(".doe_dummy_y ~ (%s)^2", paste(fcols, collapse=" + "))
        fit_dummy <- lm(as.formula(frml_txt), data=ddf[, c(fcols, ".doe_dummy_y"), drop=FALSE])

        resolution <- NA_character_
        al         <- NULL
        if (has_FrF2) {
            # BUGFIX (root cause, confirmed via the actual runtime error
            # message "'design.info' is not an exported object from
            # 'namespace:FrF2'"): design.info() is not FrF2's own function
            # at all -- it belongs to DoE.base, a hard dependency that FrF2
            # Depends on and auto-attaches via library(FrF2), and DoE.base
            # is the package that actually exports it (verified directly
            # against DoE.base's source, R/DesignAccessors.r):
            #   design.info <- function(design){
            #       if (!"design" %in% class(design))
            #           stop("design.info is applicable for class design only.")
            #       else attr(design,"design.info")
            #   }
            # So FrF2::design.info(...) was always going to raise this exact
            # namespace error, unconditionally -- the previous fix's field
            # path (catlg.entry[[1]]$res) was correct all along (also
            # confirmed against FrF2's own print.catlg() display logic), it
            # just never got the chance to run, because the call to obtain
            # "di" itself failed first and the surrounding tryCatch silently
            # swallowed that error into NA_character_. Calling
            # DoE.base::design.info() instead (the actual exported home of
            # this function) is what fixes it.
            # NOTE: catlg.entry is only present for catalog-based designs
            # (FrF2()'s own regular-fraction construction). Plackett-Burman
            # designs are built via pb(), a different, non-catalog generator
            # -- so design.info(design_obj)$catlg.entry is legitimately NULL
            # for them, and NA_character_ ("not applicable") below is the
            # correct result, not a failure: PB designs are non-regular by
            # definition, and Resolution is only a defined concept for
            # regular fractional factorials in the first place. Confirmed
            # via a real diagnostic run (di came back a valid list with
            # catlg.entry genuinely absent, no error) before removing that
            # diagnostic here.
            resolution <- tryCatch({
                di <- DoE.base::design.info(res$design_obj)
                res_num <- if (!is.null(di$catlg.entry) && length(di$catlg.entry) >= 1)
                    di$catlg.entry[[1]]$res else NULL
                if (!is.null(res_num) && !is.na(res_num))
                    as.character(as.roman(as.integer(res_num)))
                else NA_character_
            }, error=function(e) NA_character_)

            al <- tryCatch(FrF2::aliases(fit_dummy), error=function(e) NULL)
        }

        if (!is.null(al) && !is.null(al$aliases) && length(al$aliases) > 0) {
            chains <- al$aliases[sapply(al$aliases, length) > 1]
            chain_txt <- if (length(chains) > 0)
                sapply(chains, function(ch) paste(ch, collapse=" = "))
            else character(0)
            if (is.na(resolution) && length(chains) > 0) {
                # shortest word = smallest symmetric difference between the
                # factor sets of two effects in the same alias chain
                wl <- unlist(lapply(chains, function(ch) {
                    sets <- lapply(gsub("^-", "", ch), function(t) strsplit(t, ":", fixed=TRUE)[[1]])
                    if (length(sets) < 2) return(integer(0))
                    pr <- utils::combn(length(sets), 2)
                    apply(pr, 2, function(ij) length(union(setdiff(sets[[ij[1]]], sets[[ij[2]]]),
                                                           setdiff(sets[[ij[2]]], sets[[ij[1]]]))))
                }))
                wl <- wl[wl >= 1]
                if (length(wl)) resolution <- as.character(as.roman(min(wl)))
            }
            list(type="full", resolution=resolution, chains=chain_txt, matrix=NULL,
                 note=if (length(chain_txt)==0)
                     gtxt("No aliasing detected among the effects in this design (each effect is independently estimable).")
                 else NULL)
        } else {
            # Partial aliasing path (PB designs / non-regular fractions):
            # correlation between each main-effect column and each 2FI column
            # of the model matrix is the standard partial-confounding measure.
            mm <- model.matrix(as.formula(sprintf("~ (%s)^2", paste(fcols, collapse=" + "))),
                                data=ddf[, fcols, drop=FALSE])
            mm <- mm[, colnames(mm) != "(Intercept)", drop=FALSE]
            twofi_cols <- setdiff(colnames(mm), fcols)
            if (length(twofi_cols) == 0) {
                list(type="none", resolution=resolution, chains=character(0), matrix=NULL,
                     note=gtxt("This design has too few runs to estimate two-factor interactions; no partial aliasing to report."))
            } else {
                cm <- suppressWarnings(cor(mm[, fcols, drop=FALSE], mm[, twofi_cols, drop=FALSE]))
                cm[is.na(cm)] <- 0
                # ENHANCEMENT: DESIGNTYPE=FACTORIAL with no NTRIALS (or
                # NTRIALS==2^nfactors) requests the complete, unfractionated
                # grid rather than an actual fraction -- generate_factorial_FrF2()
                # calls FrF2(nruns=2^nfactors, ...) in that case. A full design
                # has zero confounding between every main effect and every
                # two-factor interaction by construction (this is the same
                # property the dedicated DESIGNTYPE=FULLFACTORIAL case above
                # already reports directly), so instead of showing a
                # correlation table that is a wall of ~0.000 values with an
                # unhelpful "not applicable" Resolution, detect that case here
                # (every correlation is negligible) and report it the same
                # clean way. A genuine partial fraction (PB or a real
                # fractional factorial) always has substantial, non-zero
                # correlations here, so this check cannot misfire on a design
                # that actually has something to report.
                if (all(abs(cm) < 1e-8)) {
                    list(type="full", resolution=if (is.na(resolution)) "Full" else resolution, chains=character(0), matrix=NULL,
                         note=gtxt("This design uses the complete (unfractionated) set of factor-level combinations: all main effects and interactions are independently estimable. No aliasing or confounding exists."))
                } else {
                    # Distinguish the genuine PB/non-regular case from the
                    # FrF2-unavailable fallback case in the caption text, so
                    # users see why a regular fractional factorial is showing a
                    # correlation matrix instead of clean "A = BC" alias chains.
                    partial_note <- if (!has_FrF2)
                        gtxt("Shown as a correlation matrix because the optional FrF2 R package is not installed (install it for named alias chains and a formal design resolution instead). Magnitude indicates degree of confounding between main effects and two-factor interactions.")
                    else
                        gtxt("Main effects are partially aliased with two-factor interactions; values are correlation coefficients between effect columns (magnitude indicates degree of confounding, not a strict equivalence).")
                    list(type="partial", resolution=resolution, chains=character(0), matrix=cm,
                         note=partial_note)
                }
            }
        }
    }, error=function(e) { warns$warn(gtxtf("Alias/confounding structure could not be computed: %s", e$message), dostop=FALSE); NULL })
    out
}

generate_box_behnken <- function(spec, variables, replicates, warns) {
    nvars_requested <- length(variables)
    nvars <- min(nvars_requested, 7)
    if (nvars < 3) warns$warn(gtxt("Box-Behnken design requires at least 3 variables"), dostop=TRUE)
    if (nvars_requested > 7) {
        warns$warn(gtxtf(
            "Box-Behnken designs are supported for up to 7 variables; only the first 7 of the %d supplied variables were used.",
            nvars_requested), dostop=FALSE)
    }
    # Standard Box-Behnken: 3 center points (not replicates*3)
    # If replicates specified, use it; otherwise default to 3
    ncp   <- if (!is.null(replicates)) replicates else 3
    pairs <- combn(nvars, 2)
    rows  <- list()
    for (p in seq_len(ncol(pairs))) {
        i <- pairs[1,p]; j <- pairs[2,p]
        for (si in c(-1,1)) for (sj in c(-1,1)) {
            r <- rep(0,nvars); r[i] <- si; r[j] <- sj
            rows[[length(rows)+1]] <- r
        }
    }
    for (k in seq_len(ncp)) rows[[length(rows)+1]] <- rep(0, nvars)
    mat <- do.call(rbind, rows)
    df  <- as.data.frame(mat)
    for (i in seq_len(nvars)) {
        lo <- as.numeric(spec$lows[i]); hi <- as.numeric(spec$highs[i])
        # Coded: -1, 0, +1 -> scale to lo, mid, hi
        mid <- (lo + hi) / 2
        df[[i]] <- ifelse(mat[,i] == -1, lo, ifelse(mat[,i] == 1, hi, mid))
        names(df)[i] <- as.character(spec$var[i])
    }
    df <- cbind(Reps=1, df)
    list(design=df, D=NA, A=NA, Ge=NA, Dea=NA)
}

generate_ccd <- function(spec, variables, replicates, warns) {
    nvars <- length(variables)
    if (nvars < 2) warns$warn(gtxt("CCD requires at least 2 variables"), dostop=TRUE)
    # Standard rotatable CCD: alpha = F^0.25 where F is the number of points
    # in the factorial (cube) portion. With a full 2-level factorial cube of
    # k=nvars factors, F = 2^nvars, so alpha = (2^nvars)^0.25 -- NOT
    # nvars^0.25 (a previous version of this code used nvars^0.25, which is
    # wrong: e.g. for k=2 it gives 1.189 instead of the textbook rotatable
    # value 1.414 = sqrt(2); for k=3 it gives 1.316 instead of 1.682). See
    # Montgomery, "Design and Analysis of Experiments", Ch. 11.
    alpha <- (2^nvars)^0.25
    # Center points: standard is ncp, not ncp*3
    ncp   <- if (!is.null(replicates)) replicates else 3
    
    # Factorial points (coded as ±1)
    fact  <- as.matrix(expand.grid(rep(list(c(-1,1)), nvars)))
    
    # Axial points (coded as ±alpha)
    axial <- do.call(rbind, lapply(seq_len(nvars), function(i) {
        r1 <- rep(0,nvars); r1[i] <-  alpha
        r2 <- rep(0,nvars); r2[i] <- -alpha
        rbind(r1, r2)
    }))
    
    # Center points (coded as 0)
    center <- matrix(0, nrow=ncp, ncol=nvars)
    mat    <- rbind(fact, axial, center)
    df     <- as.data.frame(mat)
    
    # Scale from coded units to actual units.
    # lo/hi define the FACTORIAL cube (the ±1 corners); for a rotatable CCD the
    # axial (star) points must extend BEYOND that cube to ±alpha, so do NOT
    # clamp them back into [lo,hi] -- clamping silently collapses every CCD
    # into a face-centered design and makes the alpha calculation pointless.
    #
    # BUGFIX (was: `mid + (mat[,i] / alpha) * range_half`): dividing the coded
    # value by alpha before rescaling made alpha CANCEL OUT for axial points
    # (coded ±alpha / alpha = ±1, landing exactly on lo/hi) while pulling the
    # factorial cube corners (coded ±1 / alpha) INWARD to only
    # ±(1/alpha)*range_half of the requested range -- i.e. LOWS/HIGHS ended up
    # bounding the axial points instead of the factorial cube, the opposite of
    # both the comment above and the standard rotatable-CCD definition
    # (Montgomery, Ch. 11: the ALREADY-TESTED factorial cube sits at lo/hi;
    # the star points extend beyond it to ±alpha). Multiplying by range_half
    # directly (no division by alpha) fixes this: coded ±1 -> mid±range_half
    # = lo/hi exactly (cube), coded ±alpha -> mid±alpha*range_half (star
    # points genuinely extended beyond lo/hi, as intended).
    for (i in seq_len(nvars)) {
        lo <- as.numeric(spec$lows[i]); hi <- as.numeric(spec$highs[i])
        mid <- (lo + hi) / 2
        range_half <- (hi - lo) / 2
        # Transform: coded value -> actual value. Coded ±1 (cube) lands on
        # lo/hi; coded ±alpha (axial/star) extends alpha*range_half beyond
        # lo/hi; coded 0 (center) lands on mid.
        df[[i]] <- mid + mat[,i] * range_half
        names(df)[i] <- as.character(spec$var[i])
    }
    df <- cbind(Reps=1, df)
    list(design=df, D=NA, A=NA, Ge=NA, Dea=NA)
}

# ── Sequential DOE: augment an existing factorial design to a CCD ───────────
# Reads the already-collected factorial run data straight from the active
# dataset (it is NOT regenerated/discarded -- the original runs and any
# response values already entered are kept exactly as-is) and appends the
# axial (star) points and extra center points needed to turn that factorial
# "cube" into a face-centered Central Composite Design, CCF (Montgomery,
# "Design and Analysis of Experiments", Ch. 11) -- the standard textbook
# follow-up once a factorial screening experiment shows curvature. The axial
# (star) points are placed exactly at the existing cube's low/high bounds,
# NOT extended beyond them -- see the alpha-cancellation note below. This is
# intentional and matches how this feature has been validated: every new run
# stays inside the range the user already tested, which is the right
# behavior when a factor's low/high represents a hard physical or process
# limit that must not be exceeded (e.g. a fixture's mechanical travel
# limits). NOTE: this is NOT the same low/high convention generate_ccd()
# uses for DESIGNTYPE=CCD -- that function's LOWS/HIGHS instead bound the
# outer (rotatable, alpha-extended) star points, with the factorial cube
# pulled inward. A design produced by augmenting an existing factorial and
# one produced directly with DESIGNTYPE=CCD over the same LOWS/HIGHS are
# therefore NOT the same design (their star points sit at different
# coordinates). The low/high range of the cube here is read directly from
# the existing data (min/max of each factor column) rather than re-entered,
# since the already-run factorial corners ARE that range.
augment_to_ccd <- function(varnames, responsevar, outputdataset, augmentcenterpts, alldatasets, warns) {
    if (is.null(outputdataset) || !nzchar(outputdataset))
        warns$warn(gtxt("A dataset name must be given (SAVE DATASET=...) to hold the augmented design"), dostop=TRUE)
    if (tolower(outputdataset) %in% tolower(alldatasets))
        warns$warn(gtxt("The output dataset name is already in use"), dostop=TRUE)

    existing_data <- tryCatch(spssdata.GetDataFromSPSS(),
        error=function(e) warns$warn(gtxtf("Could not read from active dataset: %s", e$message), dostop=TRUE))

    responsevars_all <- parse_multi_values(responsevar)
    meta_cols   <- c("Reps","Proportion","StdOrder","RunOrder","Block","CenterPt","PtType")
    all_cols    <- names(existing_data)
    factor_cols <- setdiff(all_cols, c(meta_cols, responsevars_all))
    variables   <- if (!is.null(varnames) && length(unlist(varnames)) > 0) as.character(unlist(varnames)) else factor_cols
    nvars       <- length(variables)

    if (nvars < 2)
        warns$warn(gtxt("Augmenting to a Central Composite Design requires at least 2 factor variables. If VARNAMES was left blank, make sure RESPONSEVAR identifies the response column so it isn't mistaken for a factor."), dostop=TRUE)

    missing_vars <- setdiff(variables, all_cols)
    if (length(missing_vars) > 0)
        warns$warn(gtxtf("Variable(s) not found in active dataset: %s", paste(missing_vars, collapse=", ")), dostop=TRUE)

    not_numeric <- variables[!sapply(existing_data[variables], is.numeric)]
    if (length(not_numeric) > 0)
        warns$warn(gtxtf("Augmenting to a CCD requires numeric factor columns; not numeric: %s", paste(not_numeric, collapse=", ")), dostop=TRUE)

    lo <- sapply(variables, function(v) suppressWarnings(min(existing_data[[v]], na.rm=TRUE)))
    hi <- sapply(variables, function(v) suppressWarnings(max(existing_data[[v]], na.rm=TRUE)))
    bad_range <- variables[!is.finite(lo) | !is.finite(hi) | lo == hi]
    if (length(bad_range) > 0)
        warns$warn(gtxtf("Variable(s) have no usable range in the existing data (need at least two distinct, non-missing values): %s", paste(bad_range, collapse=", ")), dostop=TRUE)

    # `alpha` is computed with the same rotatable-CCD formula generate_ccd()
    # uses (alpha = (2^k)^0.25), but its value cancels out in the scaling
    # step below: each axial point is coded at +-alpha and then divided by
    # that same alpha before being multiplied back onto the factor's
    # range_half, so the result always lands at exactly +-range_half from
    # the midpoint -- i.e. exactly at the existing lo/hi bounds, regardless
    # of what alpha evaluates to. In other words, this function always
    # produces FACE-CENTERED (alpha=1) axial points, never points extended
    # beyond the tested factorial range, no matter how many factors there
    # are. Left as a named variable computed the "textbook rotatable" way
    # (rather than just hardcoding 1) so the formula below stays visually
    # parallel to generate_ccd()'s, even though the two functions currently
    # place their star points differently -- see the function-level comment
    # above for why this one intentionally stays face-centered.
    alpha <- (2^nvars)^0.25

    axial <- do.call(rbind, lapply(seq_len(nvars), function(i) {
        r1 <- rep(0, nvars); r1[i] <-  alpha
        r2 <- rep(0, nvars); r2[i] <- -alpha
        rbind(r1, r2)
    }))
    axial_df <- as.data.frame(axial)
    names(axial_df) <- variables
    for (i in seq_len(nvars)) {
        v          <- variables[i]
        mid        <- (lo[[v]] + hi[[v]]) / 2
        range_half <- (hi[[v]] - lo[[v]]) / 2
        axial_df[[v]] <- mid + (axial[, i] / alpha) * range_half
    }

    ncp <- suppressWarnings(as.integer(unlist(augmentcenterpts)[1]))
    if (is.null(augmentcenterpts) || is.na(ncp) || ncp < 0) ncp <- 3

    center_df <- as.data.frame(matrix(NA_real_, nrow=max(ncp,1), ncol=nvars))[seq_len(ncp), , drop=FALSE]
    names(center_df) <- variables
    for (v in variables) center_df[[v]] <- (lo[[v]] + hi[[v]]) / 2

    # ── Tag every row with its role (purely descriptive; excluded from
    # analysis everywhere via meta_cols/PtType, exactly like CenterPt) ───────
    old_n <- nrow(existing_data)
    if ("CenterPt" %in% names(existing_data)) {
        existing_data$PtType <- ifelse(existing_data$CenterPt == 0, "Center", "Cube")
    } else {
        existing_data$PtType <- "Cube"
    }
    axial_df$PtType  <- "Axial"
    if (nrow(center_df) > 0) center_df$PtType <- "Center"

    if (!("Block" %in% names(existing_data))) existing_data$Block <- 1
    next_block <- max(suppressWarnings(as.integer(existing_data$Block)), na.rm=TRUE) + 1
    axial_df$Block <- next_block
    if (nrow(center_df) > 0) center_df$Block <- next_block

    # New (axial/center) rows have no response data yet -- left NA, exactly
    # like a freshly generated design, for the user to fill in before
    # re-running with DESIGNTYPE=CCD MODEL=QUAD ANALYZE=YES.
    all_names <- names(existing_data)
    for (nm in setdiff(all_names, names(axial_df)))  axial_df[[nm]]  <- NA
    if (nrow(center_df) > 0) for (nm in setdiff(all_names, names(center_df))) center_df[[nm]] <- NA
    axial_df <- axial_df[, all_names, drop=FALSE]
    if (nrow(center_df) > 0) center_df <- center_df[, all_names, drop=FALSE] else center_df <- center_df[, character(0), drop=FALSE]

    combined <- if (nrow(center_df) > 0) rbind(existing_data, axial_df, center_df) else rbind(existing_data, axial_df)

    if ("StdOrder" %in% names(combined)) combined$StdOrder <- seq_len(nrow(combined))
    if ("RunOrder" %in% names(combined)) combined$RunOrder <- seq_len(nrow(combined))

    tryCatch(spsspkg.EndProcedure(), error=function(e) NULL)
    gendataset(list(design=combined), outputdataset, variables, NULL, warns, designtype="ccd")

    StartProcedure(gtxt("Design of Experiments"), "STATSOPTDESIGN")
    lbls <- c(gtxt("Original (Cube) Runs Kept"), gtxt("Axial (Star) Runs Added"),
              gtxt("Center Runs Added"), gtxt("Total Runs"),
              gtxt("Alpha (Axial Distance)"), gtxt("Output Dataset"))
    # BUGFIX: this used to report the rotatable-formula `alpha` variable
    # (e.g. 1.6818 for 3 factors) here, but the axial points this function
    # actually places are face-centered (exactly at the existing lo/hi
    # bounds -- see the comments above), i.e. a true axial distance of 1.0,
    # not `alpha`. Reporting the unused rotatable value was misleading users
    # about the geometry of the design they were actually getting.
    vals <- c(old_n, nrow(axial_df), nrow(center_df), nrow(combined),
              sprintf("%.4f", 1.0), outputdataset)
    spsspivottable.Display(data.frame(cbind(vals), row.names=lbls),
        title=gtxt("Augment to Central Composite Design — Summary"),
        collabels=c(gtxt("Value")),
        templateName="DOEAUGMENTCCD", outline=gtxt("Augment to CCD"),
        caption=gtxt("The original runs and any response values already entered were kept as-is. Fill in the response column for the new (axial/center) rows in the output dataset, then re-run with DESIGNTYPE=CCD MODEL=QUAD ANALYZE=YES to fit the full quadratic response surface."))
    tryCatch(spsspkg.EndProcedure(), error=function(e) NULL)
    invisible(NULL)
}

generate_taguchi <- function(spec, variables, factors, warns) {
    nvars <- length(variables)

    # Build a genuine 2-level orthogonal (Taguchi-style) array via the
    # Sylvester-Hadamard construction: H(1) = [1]; H(2n) = [[H(n), H(n)],
    # [H(n), -H(n)]]. Every pair of columns of a Hadamard matrix is exactly
    # orthogonal, so dropping the all-+1 first column gives N-1 genuinely
    # orthogonal ±1 columns for N runs (N a power of 2) -- this generalizes
    # the old hardcoded L8-only array (which silently padded extra factors
    # with non-orthogonal random columns) to any factor count: L4(3), L8(7),
    # L16(15), L32(31), etc.
    nruns <- 2
    while ((nruns - 1) < nvars) nruns <- nruns * 2
    if (nruns > 128) {
        warns$warn(gtxtf(
            "Taguchi orthogonal-array generation supports at most %d factors; %d were requested",
            127, nvars), dostop=TRUE)
    }

    H <- matrix(1, 1, 1)
    while (nrow(H) < nruns) H <- rbind(cbind(H, H), cbind(H, -H))
    mat <- H[, 2:nruns, drop=FALSE]          # drop constant column
    mat <- mat[, seq_len(nvars), drop=FALSE] # keep only as many columns as factors

    if ((nruns - 1) > nvars) {
        warns$warn(gtxtf(
            "Taguchi design rounded up to the nearest valid orthogonal array: %d runs (smallest array that stays orthogonal for %d factors).",
            nruns, nvars), dostop=FALSE)
    }

    df <- as.data.frame(mat)
    for (i in seq_len(nvars)) {
        lo <- as.numeric(spec$lows[i]); hi <- as.numeric(spec$highs[i])
        df[[i]] <- ifelse(df[[i]]==-1, lo, hi)
        names(df)[i] <- as.character(spec$var[i])
    }
    df <- cbind(Reps=1, df)
    list(design=df, D=NA, A=NA, Ge=NA, Dea=NA)
}

generate_lhs <- function(spec, variables, ntrials, warns) {
    nvars   <- length(variables)
    if (is.null(ntrials)) ntrials <- max(10, nvars*5)
    mat     <- matrix(0, nrow=ntrials, ncol=nvars)
    for (j in seq_len(nvars)) {
        perm     <- sample(ntrials)
        mat[,j]  <- (perm - runif(ntrials)) / ntrials
    }
    df <- as.data.frame(mat)
    for (i in seq_len(nvars)) {
        lo <- as.numeric(spec$lows[i]); hi <- as.numeric(spec$highs[i])
        df[[i]] <- lo + df[[i]]*(hi-lo)
        names(df)[i] <- as.character(spec$var[i])
    }
    df <- cbind(Reps=1, df)
    list(design=df, D=NA, A=NA, Ge=NA, Dea=NA)
}

# ════════════════════════════════════════════════════════════════════════════
# ANALYSIS
# ════════════════════════════════════════════════════════════════════════════

get_response_data <- function(responsevar, design, warns) {
    tryCatch({
        ds <- spssdata.GetDataFromSPSS(variables=c(responsevar))
        if (nrow(ds) != nrow(design)) {
            warns$warn(gtxt("Response variable row count does not match design rows. Analysis skipped."), dostop=FALSE)
            return(NULL)
        }
        design[[responsevar]] <- ds[[responsevar]]
        return(design)
    }, error=function(e) {
        warns$warn(gtxtf("Could not read response variable '%s': %s", responsevar, e$message), dostop=FALSE)
        return(NULL)
    })
}

fit_model <- function(data, variables, designtype, model, warns, responsevar=NULL) {
    resp      <- if (!is.null(responsevar) && responsevar %in% names(data)) responsevar else tail(names(data), 1)
    meta_cols <- c("Reps","Proportion","StdOrder","RunOrder","Block","CenterPt","PtType")
    var_names <- intersect(as.character(variables),
                           setdiff(names(data), c(meta_cols, resp)))
    if (length(var_names)==0) {
        warns$warn(gtxt("No factor columns found for model fitting"), dostop=FALSE)
        return(NULL)
    }
    is_rsm <- designtype %in% c("ccd","rsm","boxbehnken") || tolower(model) %in% c("quad","cubic")
    if (is_rsm) {
        qt <- paste0("I(",var_names,"^2)", collapse="+")
        it <- if (length(var_names)>1)
            paste(apply(combn(var_names,2),2,paste,collapse=":"), collapse="+") else ""
        lt <- paste(var_names, collapse="+")
        frmstr <- if (nchar(it)>0) paste(resp,"~",lt,"+",it,"+",qt)
                  else paste(resp,"~",lt,"+",qt)
    } else {
        # CRITICAL: For factorial designs, use * to include ALL interactions
        # The * operator expands to: A + B + A:B (main effects + interaction)
        frmstr <- if (length(var_names)>1) paste(resp,"~",paste(var_names,collapse="*"))
                  else paste(resp,"~",var_names[1])

        # ── Curvature check term (additive) ─────────────────────────────────
        # Standard test (Montgomery, "Design and Analysis of Experiments", Ch.
        # 6 & 8) for whether the true response surface is flat or bent over the
        # experimental region: add a single 0/1 center-point indicator as one
        # extra model term. Only fires when a 'CenterPt' marker column exists
        # AND actually varies (both factorial and center rows present) --
        # today that column is only ever created by the optional, off-by-
        # default screening-center-points feature, so every existing design/
        # model that doesn't use it is completely unaffected. Never applies to
        # the is_rsm branch above (CCD/RSM/quadratic models already have their
        # own proper quadratic terms and must not get this extra indicator).
        if ("CenterPt" %in% names(data)) {
            cp_vals <- suppressWarnings(as.numeric(data$CenterPt))
            if (length(unique(cp_vals[!is.na(cp_vals)])) > 1) {
                frmstr <- paste(frmstr, "+ CenterPt")
            }
        }
    }
    
    fit <- tryCatch(lm(as.formula(frmstr), data=data),
             error=function(e) {
                 warns$warn(gtxtf("Model fitting error: %s", e$message), dostop=FALSE)
                 NULL
             })
    # BUGFIX: lm()'s stored call captures `data=data` as an unevaluated
    # symbol pointing at this function's local `data` parameter. Anything
    # that later re-evaluates that call in a different environment --
    # compute_variable_selection()'s step()/update() being the concrete
    # case that was crashing with "'data' must be a data.frame, environment,
    # or list" -- can fail to resolve that symbol, because fit_model()'s own
    # call frame is long gone by the time step() runs. Embedding the actual
    # data.frame object into the call (instead of a symbol reference) makes
    # any later eval(fit$call, ...) self-contained and immune to this
    # classic R scoping trap, regardless of which environment does the
    # re-evaluating.
    if (!is.null(fit)) fit$call$data <- data
    fit
}

# BUGFIX: every predict(fit, newdata=...) builder below (cube plot, contour
# plot, optimizer, sensitivity analysis, multi-response optimizer) builds
# newdata from scratch out of var_names + held-variable means, deliberately
# excluding meta_cols (Reps/Proportion/StdOrder/RunOrder/Block/CenterPt/
# PtType) as "not real factors". That's correct for all of them except
# CenterPt: fit_model() sometimes adds "+ CenterPt" as a genuine curvature
# term (see its own comment), which happens whenever the analyzed data has a
# real, varying CenterPt column -- common when re-analyzing an existing
# dataset (GENERATEDESIGN=NO) that carries one. When that happens, any
# newdata missing a CenterPt column makes predict() fail with "object
# 'CenterPt' not found" while evaluating the model formula. Call this right
# after building any such newdata, before predict(). 0 = a factorial/cube
# corner point (not a center point), the correct default for cube/contour/
# optimizer evaluation, which by construction always evaluates points away
# from the design center.
.doe_add_centerpt <- function(fit, nd) {
    if (!("CenterPt" %in% names(nd)) &&
        "CenterPt" %in% tryCatch(all.vars(formula(fit)), error=function(e) character(0)))
        nd$CenterPt <- 0
    nd
}

# ════════════════════════════════════════════════════════════════════════════
# ANALYSIS OUTPUT
# ════════════════════════════════════════════════════════════════════════════

# ════════════════════════════════════════════════════════════════════════════
# OUTPUT SUBDIALOG HELPERS — Effects/Significance, Residual Tests, VIF
# ════════════════════════════════════════════════════════════════════════════
# These three helpers back the new "Output" GUI subdialog's checkboxes. Each
# is purely additive: gated by its own boolean flag (default off), reads only
# from the already-fitted model, and never alters any existing table/plot.

# Lenth's (1989, Technometrics) Pseudo Standard Error method for judging
# effect significance without needing replicated residual df -- the classic
# remedy for exactly the "saturated model" case this extension already flags
# as having invalid ANOVA p-values (see the warning in display_analysis()).
compute_effects_table <- function(fit, warns) {
    out <- tryCatch({
        coefs <- coef(fit)
        eff   <- coefs[names(coefs) != "(Intercept)"]
        eff   <- eff[!is.na(eff)]
        if (length(eff) < 3) return(NULL)

        abs_eff <- abs(eff)
        s0      <- 1.5 * median(abs_eff)
        trimmed <- abs_eff[abs_eff < 2.5 * s0]
        if (length(trimmed) == 0) trimmed <- abs_eff
        PSE   <- 1.5 * median(trimmed)
        m     <- length(eff)
        # Use m/3 directly (not rounded) -- matches lenth_pse() exactly so the
        # Effects Table, Pareto chart, and Half-Normal plot always report the
        # identical ME/SME for the same fit (qt() accepts fractional df).
        d     <- m / 3
        ME    <- if (PSE > 0) qt(0.975, d) * PSE else 0
        gamma <- (1 + 0.95^(1 / m)) / 2
        SME   <- if (PSE > 0) qt(gamma, d) * PSE else 0

        ord <- order(abs_eff, decreasing=TRUE)
        df <- data.frame(
            Effect = names(eff)[ord],
            Estimate = round(eff[ord], 4),
            `|Effect|` = round(abs_eff[ord], 4),
            `Significant (ME)`  = ifelse(abs_eff[ord] > ME,  gtxt("Yes"), gtxt("No")),
            `Significant (SME)` = ifelse(abs_eff[ord] > SME, gtxt("Yes"), gtxt("No")),
            check.names=FALSE
        )
        list(df=df, PSE=PSE, ME=ME, SME=SME)
    }, error=function(e) { warns$warn(gtxtf("Effects table could not be computed: %s", e$message), dostop=FALSE); NULL })
    out
}

# Residual normality (Shapiro-Wilk, base R) + autocorrelation (Durbin-Watson,
# computed directly as sum(diff(e)^2)/sum(e^2) -- no 'lmtest' dependency).
compute_residual_tests <- function(fit, warns) {
    out <- tryCatch({
        e <- residuals(fit)
        e <- e[is.finite(e)]
        n <- length(e)
        if (n < 3) return(NULL)

        sw <- if (n <= 5000) tryCatch(shapiro.test(e), error=function(x) NULL) else NULL
        dw <- sum(diff(e)^2) / sum(e^2)
        dw_note <- if (dw < 1.5) gtxt("Possible positive autocorrelation")
                   else if (dw > 2.5) gtxt("Possible negative autocorrelation")
                   else gtxt("No strong evidence of autocorrelation")

        labs <- c(gtxt("Shapiro-Wilk W"), gtxt("Shapiro-Wilk p-value"), gtxt("Durbin-Watson Statistic"))
        vals <- c(if (!is.null(sw)) round(sw$statistic, 4) else gtxt("N/A (n out of range)"),
                  if (!is.null(sw)) round(sw$p.value, 4)   else gtxt("N/A"),
                  round(dw, 4))
        df <- data.frame(Value=as.character(vals), row.names=labs)
        list(df=df, sw=sw, dw=dw, dw_note=dw_note)
    }, error=function(e) { warns$warn(gtxtf("Residual tests could not be computed: %s", e$message), dostop=FALSE); NULL })
    out
}

# Hand-rolled Variance Inflation Factor via auxiliary regressions on the
# model-matrix columns -- avoids adding a dependency on 'car'. VIF_j =
# 1/(1-R_j^2) where R_j^2 comes from regressing predictor j on all other
# predictors. Orthogonal designs (e.g. well-balanced factorials) should show
# VIF ~= 1, which is itself a useful confirmation for the user.
compute_vif_table <- function(fit, warns) {
    out <- tryCatch({
        mm <- model.matrix(fit)
        mm <- mm[, colnames(mm) != "(Intercept)", drop=FALSE]
        if (ncol(mm) < 2) return(NULL)

        vifs <- sapply(seq_len(ncol(mm)), function(j) {
            y <- mm[, j]
            X <- mm[, -j, drop=FALSE]
            r2 <- tryCatch(summary(lm(y ~ X))$r.squared, error=function(e) NA)
            if (is.na(r2) || r2 >= 0.999999) return(Inf)
            1 / (1 - r2)
        })
        df <- data.frame(Term=colnames(mm), VIF=ifelse(is.infinite(vifs), gtxt("Inf (perfect collinearity)"), as.character(round(vifs, 3))))
        list(df=df, max_vif=suppressWarnings(max(vifs[is.finite(vifs)])))
    }, error=function(e) { warns$warn(gtxtf("VIF table could not be computed: %s", e$message), dostop=FALSE); NULL })
    out
}

# ── Screening summary / ranking table (additive) ────────────────────────────
# A compact, rank-ordered view of every fitted main effect, sorted by absolute
# magnitude with a running cumulative-contribution percentage -- the tabular
# analogue of the standard Pareto-of-effects chart used to triage which
# factors deserve further investigation in a screening experiment (Box,
# Hunter & Hunter, "Statistics for Experimenters", Ch. 5). Applicable to any
# fitted design, but most useful immediately after a screening design
# (Factorial, Plackett-Burman, or Definitive Screening Design).
compute_screening_ranking <- function(fit, warns) {
    out <- tryCatch({
        coefs <- coef(fit)
        eff   <- coefs[names(coefs) != "(Intercept)"]
        eff   <- eff[!is.na(eff)]
        if (length(eff) == 0) return(NULL)
        abs_eff <- abs(eff)
        total   <- sum(abs_eff)
        ord     <- order(abs_eff, decreasing=TRUE)
        eff_ord <- eff[ord]
        abs_ord <- abs_eff[ord]
        cum_pct <- if (total > 0) cumsum(abs_ord) / total * 100 else rep(NA_real_, length(abs_ord))
        df <- data.frame(
            Rank = seq_along(eff_ord),
            Factor = names(eff_ord),
            Estimate = round(eff_ord, 4),
            `|Effect|` = round(abs_ord, 4),
            `Cumulative %` = round(cum_pct, 1),
            check.names = FALSE
        )
        list(df=df)
    }, error=function(e) { warns$warn(gtxtf("Screening summary/ranking table could not be computed: %s", e$message), dostop=FALSE); NULL })
    out
}

display_analysis <- function(fit, data, variables, warns, responsevar=NULL,
                              effectstable=FALSE, residualtests=FALSE, viftable=FALSE,
                              screeningsummary=FALSE,
                              varselect=FALSE, varselmethod="stepwise", stepdir="both") {
    StartProcedure(gtxt("DOE Analysis"), "STATSDOEANALYSIS")

    resp <- if (!is.null(responsevar) && responsevar %in% names(data)) responsevar else tail(names(data), 1)

    sm   <- summary(fit)
    df_resid <- sm$df[2]
    
    # Enhanced warning for saturated/low df models
    if (df_resid < 1) {
        warns$warn(gtxt(
            "SATURATED MODEL DETECTED: Zero residual degrees of freedom.\n
            Statistical tests (p-values, F-tests) are INVALID.\n
            SOLUTION: Add replicates (REPLICATES=3) or center points (CENTER=YES) to enable valid inference.\n
            Current results show model fit only, not statistical significance."),
            dostop=FALSE)
    } else if (df_resid < 3) {
        warns$warn(gtxtf(
            "LOW STATISTICAL POWER: Only %d residual degree(s) of freedom.\n
            Recommendation: Add replicates to increase df to at least 8 for reliable inference.",
            df_resid),
            dostop=FALSE)
    }
    
    an   <- anova(fit)
    an_df <- as.data.frame(an)
    
    # Round numeric columns to 3 decimal places for cleaner display
    for (col in names(an_df)) {
        if (is.numeric(an_df[[col]])) {
            an_df[[col]] <- round(an_df[[col]], 3)
        }
    }
    an_df[is.na(an_df)] <- ""
    
    # Add note to ANOVA caption if saturated
    anova_caption <- if (df_resid < 1) {
        gtxt("Computed by R lm/aov\nWARNING: F-tests unreliable with 0 residual df")
    } else {
        gtxt("Computed by R lm/aov")
    }
    
    spsspivottable.Display(an_df,
        title=gtxt("Analysis of Variance"),
        caption=anova_caption,
        templateName="DOEANOVA", outline=gtxt("ANOVA"))

    coef_df          <- as.data.frame(sm$coefficients)
    # Round coefficients to 3 decimal places
    for (col in 1:ncol(coef_df)) {
        if (is.numeric(coef_df[[col]])) {
            coef_df[[col]] <- round(coef_df[[col]], 3)
        }
    }
    names(coef_df)   <- c(gtxt("Estimate"), gtxt("Std Error"), gtxt("t value"), gtxt("Pr(>|t|)"))
    spsspivottable.Display(coef_df,
        title=gtxt("Regression Coefficients"),
        templateName="DOECOEF", outline=gtxt("Coefficients"))

    fstat    <- sm$fstatistic

    # ── Predicted R-squared via PRESS (leave-one-out) ─────────────────────────
    # PRESS = sum((e_i / (1 - h_ii))^2), Pred R^2 = 1 - PRESS/SST
    # Standard in standard DOE texts (Montgomery) for assessing predictive ability and
    # detecting overfitting (Pred R^2 much lower than R^2 signals overfit model).
    pred_r2 <- NA
    press   <- NA
    if (resp %in% names(data)) {
        y   <- data[[resp]]
        h   <- tryCatch(hatvalues(fit), error=function(e) NULL)
        if (!is.null(h) && length(h) == length(y) && all(is.finite(y))) {
            if (any(h >= 1 - 1e-8)) {
                # One or more points are perfectly interpolated (saturated model) -
                # leave-one-out residual is undefined (division by ~0).
                press   <- NA
            } else {
                press_resid <- residuals(fit) / (1 - h)
                press       <- sum(press_resid^2)
                sst         <- sum((y - mean(y, na.rm=TRUE))^2)
                if (sst > 0) pred_r2 <- 1 - press/sst
            }
        }
    }

    fit_lbls <- c(gtxt("R-squared"), gtxt("Adj. R-squared"), gtxt("Predicted R-squared"),
                  gtxt("F-statistic"), gtxt("Residual Std Error"), gtxt("DF Residual"), gtxt("PRESS"))
    fit_vals <- c(round(sm$r.squared,4), round(sm$adj.r.squared,4),
                  if (!is.na(pred_r2)) round(pred_r2,4) else gtxt("N/A (saturated model)"),
                  if (!is.null(fstat)) round(fstat[1],3) else NA,
                  round(sm$sigma,4), sm$df[2],
                  if (!is.na(press)) round(press,4) else gtxt("N/A (saturated model)"))
    fit_summary_df <- data.frame(Value=as.character(fit_vals), row.names=fit_lbls)
    fit_caption <- if (!is.na(pred_r2) && !is.na(sm$r.squared) && (sm$r.squared - pred_r2) > 0.2) {
        gtxt("Note: Predicted R-squared is substantially lower than R-squared, which can indicate the model is overfit or contains terms not useful for prediction.")
    } else {
        gtxt("Computed by R lm/aov")
    }
    spsspivottable.Display(fit_summary_df,
        title=gtxt("Model Summary"), caption=fit_caption,
        templateName="DOEFITSUM", outline=gtxt("Model Fit"))

    # ── Lack-of-Fit test (requires replicated design points) ──────────────────
    # Classic decomposition (Montgomery, "Design and Analysis of Experiments"):
    #   SS_PureError = sum of within-replicate-group sums of squares
    #   SS_LOF       = SS_Residual - SS_PureError
    #   F = (SS_LOF/df_LOF) / (SS_PureError/df_PureError)
    # Only meaningful/computable when at least one factor-level combination was
    # measured more than once (replicated points), giving df_PureError > 0.
    meta_cols_lof <- c("Reps","Proportion","StdOrder","RunOrder","Block","CenterPt","PtType")
    var_names_lof <- intersect(as.character(variables),
                               setdiff(names(data), c(meta_cols_lof, resp)))
    if (length(var_names_lof) > 0 && resp %in% names(data)) {
        key <- do.call(paste, c(lapply(data[var_names_lof], function(col) {
            if (is.numeric(col)) round(col, 6) else as.character(col)
        }), sep="\r"))
        grp_sizes <- table(key)
        rep_groups <- names(grp_sizes)[grp_sizes >= 2]
        if (length(rep_groups) > 0) {
            y <- data[[resp]]
            ss_pure <- 0; df_pure <- 0
            for (g in rep_groups) {
                yi <- y[key == g]
                ss_pure <- ss_pure + sum((yi - mean(yi, na.rm=TRUE))^2, na.rm=TRUE)
                df_pure <- df_pure + (length(yi) - 1)
            }
            ss_resid  <- sum(residuals(fit)^2)
            df_resid_total <- df.residual(fit)
            ss_lof <- ss_resid - ss_pure
            df_lof <- df_resid_total - df_pure
            if (df_lof > 0 && df_pure > 0 && ss_lof >= -1e-9) {
                ss_lof   <- max(ss_lof, 0)
                ms_lof   <- ss_lof / df_lof
                ms_pure  <- ss_pure / df_pure
                f_lof    <- if (ms_pure > 0) ms_lof / ms_pure else NA
                p_lof    <- if (!is.null(f_lof) && !is.na(f_lof)) 1 - pf(f_lof, df_lof, df_pure) else NA
                lof_df <- data.frame(
                    Source = c(gtxt("Lack-of-Fit"), gtxt("Pure Error")),
                    DF     = c(df_lof, df_pure),
                    `Sum Sq` = c(round(ss_lof,4), round(ss_pure,4)),
                    `Mean Sq`= c(round(ms_lof,4), round(ms_pure,4)),
                    `F value`= c(round(f_lof,3), NA),
                    `Pr(>F)` = c(round(p_lof,4), NA),
                    check.names=FALSE
                )
                lof_caption <- if (!is.na(p_lof) && p_lof < 0.05)
                    gtxt("Significant lack-of-fit (p < 0.05): the model form may not adequately describe the data.")
                else
                    gtxt("No significant lack-of-fit detected.")
                spsspivottable.Display(lof_df,
                    title=gtxt("Lack-of-Fit Test"), caption=lof_caption,
                    templateName="DOELOF", outline=gtxt("Lack-of-Fit"))
            }
        }
    }

    # ── Output subdialog: Effects & Significance Table (Lenth's PSE method) ──
    if (effectstable) {
        eff_info <- compute_effects_table(fit, warns)
        if (!is.null(eff_info)) {
            spsspivottable.Display(eff_info$df,
                title=gtxt("Effects & Significance (Lenth's Method)"),
                caption=gtxtf("PSE=%.4f, Margin of Error=%.4f, Simultaneous MoE=%.4f (Lenth, 1989, Technometrics). Useful when residual degrees of freedom are too low for standard t/F tests.",
                               eff_info$PSE, eff_info$ME, eff_info$SME),
                templateName="DOEEFFECTS", outline=gtxt("Effects & Significance"))
        }
    }

    # ── Output subdialog: Residual Normality & Autocorrelation Tests ─────────
    if (residualtests) {
        rt_info <- compute_residual_tests(fit, warns)
        if (!is.null(rt_info)) {
            spsspivottable.Display(rt_info$df,
                title=gtxt("Residual Diagnostics: Normality & Autocorrelation"),
                caption=gtxtf("Shapiro-Wilk tests normality of residuals (p<0.05 suggests non-normality). Durbin-Watson tests autocorrelation (~2 = none). %s",
                               rt_info$dw_note),
                templateName="DOERESIDTESTS", outline=gtxt("Residual Tests"))
        }
    }

    # ── Output subdialog: Multicollinearity (VIF) Table ───────────────────────
    if (viftable) {
        vif_info <- compute_vif_table(fit, warns)
        if (!is.null(vif_info)) {
            spsspivottable.Display(vif_info$df,
                title=gtxt("Multicollinearity: Variance Inflation Factors"),
                caption=gtxt("VIF ~= 1 indicates an orthogonal (well-balanced) design. VIF > 5 indicates moderate, VIF > 10 indicates severe multicollinearity among model terms."),
                templateName="DOEVIF", outline=gtxt("Multicollinearity"))
        }
    }

    # ── Screening Designs subdialog: Screening Summary / Ranking Table ───────
    if (screeningsummary) {
        rank_info <- compute_screening_ranking(fit, warns)
        if (!is.null(rank_info)) {
            spsspivottable.Display(rank_info$df,
                title=gtxt("Screening Summary: Effect Ranking"),
                caption=gtxt("Main effects ranked by absolute magnitude with running cumulative contribution -- useful for triaging which factors warrant further study after a screening design."),
                templateName="DOESCREENRANK", outline=gtxt("Screening Summary"))
        }
    }


    # ── Variable Selection (Stepwise or Best Subsets) ─────────────────────────
    if (varselect) {
        vsel <- tryCatch(
            compute_variable_selection(fit, varselmethod=varselmethod, stepdir=stepdir, warns=warns),
            error=function(e) {
                warns$warn(gtxtf("Variable selection error: %s", e$message), dostop=FALSE)
                NULL
            })
        if (!is.null(vsel)) {
            if (vsel$type == "stepwise") {
                cap <- gtxtf(
                    "AIC-based stepwise selection | Direction: %s | Final model: %s",
                    vsel$direction, vsel$final_formula)
                if (nrow(vsel$df) > 0) {
                    spsspivottable.Display(vsel$df,
                        title=gtxt("Variable Selection: Stepwise Steps"),
                        caption=cap,
                        templateName="DOESTEPCAP",
                        outline=gtxt("Variable Selection"))
                } else {
                    spsspivottable.Display(
                        data.frame(Result=gtxt("No variables were added or removed — model unchanged.")),
                        title=gtxt("Variable Selection: Stepwise Steps"),
                        caption=cap,
                        templateName="DOESTEPCAP",
                        outline=gtxt("Variable Selection"))
                }
            } else if (vsel$type == "bestsubsets") {
                cap <- gtxtf(
                    "Best subset by Adj. R²: size=%d, terms: %s",
                    vsel$best_size, paste(vsel$best_vars, collapse=", "))
                spsspivottable.Display(vsel$df,
                    title=gtxt("Variable Selection: Best Subsets"),
                    caption=cap,
                    templateName="DOESBESTCAP",
                    outline=gtxt("Variable Selection"))
            }
        }
    }

    spsspkg.EndProcedure()
}


# ════════════════════════════════════════════════════════════════════════════
# VARIABLE SELECTION
# ════════════════════════════════════════════════════════════════════════════

# compute_variable_selection ─────────────────────────────────────────────────
# conventional variable selection on a fitted lm model.
# Two methods:
#   "stepwise"    — AIC-based step() from base R; no extra package required.
#                   Returns a step-by-step table (Step, Action, Term, AIC)
#                   and the formula of the final selected model.
#   "bestsubsets" — Exhaustive best-subsets via leaps::regsubsets().
#                   Requires the 'leaps' package (graceful warning if absent).
#                   Returns a table indexed by subset size with R², Adj-R², Cp, BIC.
# Both results are displayed as SPSS pivot tables inside display_analysis().
# ────────────────────────────────────────────────────────────────────────────
compute_variable_selection <- function(fit, varselmethod="stepwise", stepdir="both", warns) {
    # DEFENSE IN DEPTH: optdesmc() already normalizes the CDB dialog's raw
    # auto-generated enum IDs (item_273_a/item_273_b) to "stepwise"/
    # "bestsubsets" once, near the top of Run() -- but this function is also
    # called from a few different places (display_analysis(), the plots
    # pathway, and the HTML report), so the same normalization is repeated
    # here, self-contained, in case a raw item_273_a/item_273_b value ever
    # reaches this function directly without having passed through that
    # earlier normalization. Purely additive: if varselmethod is already a
    # normalized/friendly value (or already something else entirely), this
    # has no effect and falls through unchanged.
    varselmethod <- switch(tolower(trimws(as.character(varselmethod))),
        "item_273_a" = "stepwise",
        "item_273_b" = "bestsubsets",
        varselmethod)
    stepdir <- switch(tolower(trimws(as.character(stepdir))),
        "item_278_a" = "both",
        "item_278_b" = "forward",
        "item_278_d" = "backward",
        stepdir)
    method <- tolower(trimws(varselmethod))

    # ── Stepwise ─────────────────────────────────────────────────────────────
    if (method == "stepwise") {
        direction <- tolower(trimws(stepdir))
        if (!direction %in% c("both","forward","backward")) direction <- "both"
        result <- tryCatch({
            # step() trace=0 suppresses per-iteration console output
            step_fit <- if (direction == "forward") {
                # Forward selection has to start from the intercept-only model
                # and add terms up to the full model; starting from the full
                # model (as before) leaves nothing to add, so it stopped at step 1.
                mf <- fit$model
                f0 <- as.formula(paste(deparse(formula(fit)[[2]]), "~ 1"))
                fit0 <- do.call("lm", list(formula=f0, data=mf))
                step(fit0, scope=list(lower=~1, upper=formula(fit)), direction="forward", trace=0)
            } else step(fit, direction=direction, trace=0)
            anova_steps <- step_fit$anova   # data.frame produced by step()
            if (!is.null(anova_steps) && nrow(anova_steps) > 0) {
                df <- data.frame(
                    Step   = seq_len(nrow(anova_steps)),
                    Action = { a <- gsub("^\\s+|\\s+$", "", as.character(anova_steps[["Step"]])); a[!nzchar(a)] <- gtxt("start model"); a },
                    Df     = abs(round(anova_steps[["Df"]], 0)),
                    AIC    = round(anova_steps[["AIC"]], 3),
                    stringsAsFactors = FALSE
                )
            } else {
                df <- data.frame(Step=integer(0), Action=character(0),
                                 Df=integer(0), AIC=numeric(0))
            }
            list(type="stepwise", df=df,
                 final_formula=gsub("\\s+", " ", paste(deparse(formula(step_fit), width.cutoff=500L), collapse=" ")),
                 direction=direction)
        }, error=function(e) {
            warns$warn(gtxtf("Stepwise variable selection could not be computed: %s", e$message),
                       dostop=FALSE)
            NULL
        })
        return(result)
    }

    # ── Best Subsets ─────────────────────────────────────────────────────────
    if (method == "bestsubsets") {
        if (!requireNamespace("leaps", quietly=TRUE)) {
            warns$warn(
                gtxt("Best Subsets selection requires the 'leaps' R package. Install it with: install.packages('leaps')"),
                dostop=FALSE)
            return(NULL)
        }
        result <- tryCatch({
            suppressMessages(library(leaps))
            mf  <- model.frame(fit)
            y   <- model.response(mf)
            Xm  <- model.matrix(fit)
            # Drop intercept column; if nothing left, bail
            keep <- colnames(Xm) != "(Intercept)"
            X   <- Xm[, keep, drop=FALSE]
            if (ncol(X) == 0) {
                warns$warn(gtxt("Best Subsets: no predictors available after removing intercept."),
                           dostop=FALSE)
                return(NULL)
            }
            nvmax <- min(ncol(X), 10)   # cap at 10 for stability
            rs    <- leaps::regsubsets(X, y, nvmax=nvmax, method="exhaustive", really.big=TRUE)
            rs_s  <- summary(rs)
            sizes <- seq_len(nrow(rs_s$which))
            df <- data.frame(
                Size     = sizes,
                "R2 (%)"    = round(rs_s$rsq * 100, 2),
                "AdjR2 (%)" = round(rs_s$adjr2 * 100, 2),
                Cp       = round(rs_s$cp, 3),
                BIC      = round(rs_s$bic, 3),
                check.names=FALSE,
                stringsAsFactors=FALSE
            )
            best_idx  <- which.max(rs_s$adjr2)
            mask      <- rs_s$which[best_idx, ]
            best_vars <- names(mask)[mask & names(mask) != "(Intercept)"]
            list(type="bestsubsets", df=df,
                 best_size=best_idx, best_vars=best_vars,
                 which_matrix=rs_s$which)
        }, error=function(e) {
            warns$warn(gtxtf("Best Subsets selection could not be computed: %s", e$message),
                       dostop=FALSE)
            NULL
        })
        return(result)
    }

    warns$warn(gtxtf("Unknown variable selection method: %s", varselmethod), dostop=FALSE)
    NULL
}

# ════════════════════════════════════════════════════════════════════════════
# PLOTS
# ════════════════════════════════════════════════════════════════════════════

# ── Lenth's (1989) pseudo-standard-error method for unreplicated designs ────
# Provides a valid significance assessment for effect estimates when there are
# no (or very few) residual degrees of freedom for a classical t-test - the
# standard approach used by common DOE practice for unreplicated two-level factorials.
# Reference: Lenth, R.V. (1989), "Quick and Easy Analysis of Unreplicated
# Factorials", Technometrics, 31(4), 469-473.
lenth_pse <- function(effects) {
    effects <- effects[is.finite(effects)]
    m <- length(effects)
    if (m < 3) return(NULL)
    s0      <- 1.5 * median(abs(effects))
    trimmed <- abs(effects)[abs(effects) < 2.5 * s0]
    if (length(trimmed) == 0) trimmed <- abs(effects)
    pse     <- 1.5 * median(trimmed)
    dfl     <- m / 3
    me      <- qt(0.975, dfl) * pse
    gamma   <- (1 + 0.95^(1/m)) / 2
    sme     <- qt(gamma, dfl) * pse
    list(PSE=pse, ME=me, SME=sme, df=dfl)
}

# ── Factor letter-coding for Pareto/effects charts ──────────────────────────
# Classic factorial-design convention (Montgomery, "Design and Analysis of
# Experiments"): factors are referred to by single letters (A, B, C, ...) in
# the order they appear in the design, and interaction terms are written as
# the concatenation of their components' letters (e.g. the interaction of the
# 1st and 2nd factor is "AB"). This keeps long/interaction term labels short
# and legible on a horizontal bar chart; a companion legend maps each letter
# back to the actual variable name. Independent of any particular vendor's
# chart -- this is a generic, decades-old textbook labeling convention.
effect_term_codes <- function(term_names, var_names) {
    code_letters <- if (length(var_names) <= 26) LETTERS[seq_along(var_names)]
                     else paste0("X", seq_along(var_names))
    code_map <- setNames(code_letters, var_names)
    code_for_term <- function(tn) {
        parts <- strsplit(tn, ":", fixed=TRUE)[[1]]
        mapped <- code_map[parts]
        unmapped <- is.na(mapped)
        if (any(unmapped)) mapped[unmapped] <- parts[unmapped]
        paste(mapped, collapse="")
    }
    list(codes  = vapply(term_names, code_for_term, character(1), USE.NAMES=FALSE),
         legend = data.frame(Letter=code_letters, Name=var_names, stringsAsFactors=FALSE))
}

# BUGFIX/HARDENING: the existing setwd(tempdir()) guard (below, in both
# create_all_plots() and create_varselect_plots()) assumed tempdir() itself
# is always writable, which is the normal case but is not guaranteed for
# every Windows R Integration Plug-in install (e.g. a locked-down service
# account, a per-session temp folder that was never created, or a redirected/
# read-only profile). If tempdir() isn't actually writable, spssRGraphics.
# Submit()'s own internal fallback device still fails with "invalid 'file'
# argument" even after the cwd change, which matches reports of that error
# recurring on every single plot despite the existing guard. This does an
# ACTUAL write test (not just calling tempdir()) and tries several fallback
# candidates in order before giving up. Returns the first writable directory
# found, or NULL if none are (in which case the caller should leave cwd
# alone rather than setwd() into a directory that turned out not to help).
.doe_find_writable_dir <- function() {
    candidates <- unique(Filter(function(x) !is.null(x) && nzchar(x), list(
        tryCatch(tempdir(), error=function(e) NULL),
        Sys.getenv("TEMP", unset=NA),
        Sys.getenv("TMP", unset=NA),
        Sys.getenv("TMPDIR", unset=NA),
        tryCatch(getwd(), error=function(e) NULL)
    )))
    for (d in candidates) {
        ok <- tryCatch({
            probe <- file.path(d, paste0(".doe_wtest_", Sys.getpid(), "_",
                                          as.integer(runif(1, 1, 1e6))))
            con <- file(probe, "w"); close(con)
            file.remove(probe)
            TRUE
        }, error=function(e) FALSE, warning=function(w) FALSE)
        if (isTRUE(ok)) return(d)
    }
    NULL
}

# BUGFIX (root cause, confirmed via live diagnostic): every call site below
# used to call spssRGraphics.Submit({ <plotting code> }) directly -- but R
# has no special handling for that {} block; it is a completely ordinary
# function argument, so R
# evaluates it EAGERLY (running the plotting code as a side effect) and
# passes whatever its LAST statement returns (often NULL, e.g. from
# mtext()/axis()/legend()) into spssRGraphics.Submit as if it were the
# function's real argument. A live diagnostic run (July 2026, macOS/Darwin,
# R 4.4.1) traced "invalid 'file' argument" directly to grDevices::pdf()'s
# own checkIntFormat(file) validation, visible in that session's dumped
# getOption("device") source -- i.e. a non-path value really was reaching a
# device-open call downstream, exactly as this explains. IBM's own R
# Integration Package documentation ("Displaying Graphical Output from R")
# shows spssRGraphics.Submit() taking an actual FILE PATH ON DISK, e.g.
# spssRGraphics.Submit("/temp/R_graphic.jpg") -- not a code block. This
# wrapper captures the block UNEVALUATED (substitute(), matching how e.g.
# with() or tryCatch()'s own expr argument works), opens an explicit png()
# device at a verified-writable path, evaluates the plotting code into that
# file, closes the device, and submits the real path -- the documented,
# working calling convention. Every call site keeps its exact existing
# {...} block; only the function name changes, so none of the plotting
# logic itself needed to change.
.doe_submit_plot <- function(expr, width=960, height=720, res=115) {
    e  <- substitute(expr)
    pe <- parent.frame()
    # an interactive version of this chart was already registered for the HTML report
    skip_img <- exists(".fa_capture") && isTRUE(.fa_capture$skip_img)
    if (skip_img) .fa_capture$skip_img <- FALSE
    wd <- .doe_find_writable_dir()
    if (is.null(wd)) wd <- tempdir()
    fp <- file.path(wd, paste0("doe_plot_", as.integer(Sys.time()), "_",
                                sample.int(1e6, 1), ".png"))
    grDevices::png(filename=fp, width=width, height=height, res=res)
    tryCatch({
        eval(e, envir=pe)
    }, finally={
        tryCatch(grDevices::dev.off(), error=function(e2) NULL)
    })
    if (file.exists(fp)) {
        if (exists(".fa_capture") && isTRUE(.fa_capture$on) && !skip_img)
            tryCatch(.fa_capture_add(list(type="image", b64=.fa_b64(fp))), error=function(e2) NULL)
        spssRGraphics.Submit(fp)
        tryCatch(file.remove(fp), error=function(e2) NULL)
    } else {
        stop("plot image file was not created")
    }
    invisible(NULL)
}

create_all_plots <- function(fit, data, variables, designtype,
                              do_main, do_inter, do_cube,
                              do_contour, do_resid, do_pareto, warns, use_external=FALSE, responsevar=NULL,
                              do_curvature=FALSE) {
    resp      <- if (!is.null(responsevar) && responsevar %in% names(data)) responsevar else tail(names(data), 1)
    meta_cols <- c("Reps","Proportion","StdOrder","RunOrder","Block","CenterPt","PtType")
    var_names <- intersect(as.character(variables),
                           setdiff(names(data), c(meta_cols, resp)))

    # Defensive: if an earlier STATS DOE ANALYSIS call in the same R session
    # (e.g. the POWER planning tool's own spssRGraphics.Submit() chart) left a
    # graphics device open -- because its own plotting tryCatch swallowed an
    # error without a matching dev.off() -- the R Integration Plug-in's next
    # spssRGraphics.Submit() call here can fail with "invalid 'file' argument"
    # even though this plot's own code is fine. Force a clean device slate
    # before the first plot of this call so a prior call's leftover state
    # can't break this one.
    while (!is.null(dev.list())) tryCatch(dev.off(), error=function(e) NULL)

    # Defensive: spssRGraphics.Submit() relies on R being able to fall back to
    # its default (e.g. pdf()) device if no on-screen device is already open.
    # That fallback writes to "Rplots.pdf" in the R process's current working
    # directory -- and the R Integration Plug-in's working directory is not
    # always writable (it can be the SPSS install folder rather than a temp
    # folder), producing "cannot open file 'Rplots.pdf'" on the very first
    # plot of the call. Force the cwd to a directory VERIFIED writable (see
    # .doe_find_writable_dir() above -- plain tempdir() was tried here before
    # and was not sufficient on every install) for the duration of plotting,
    # then restore it so nothing else in this session (e.g. relative paths
    # elsewhere) is affected.
    .doe_old_wd <- getwd()
    on.exit(tryCatch(setwd(.doe_old_wd), error=function(e) NULL), add=TRUE)
    .doe_wd <- .doe_find_writable_dir()
    if (!is.null(.doe_wd)) tryCatch(setwd(.doe_wd), error=function(e) NULL)

    StartProcedure(gtxt("DOE Plots"), "STATSDOEPLOTS")
    
    # Check for saturated model (zero residual df)
    df_resid <- summary(fit)$df[2]
    use_lenth <- FALSE
    if (df_resid < 1) {
        warns$warn(gtxt(
            "Model is saturated (0 residual df) — residual diagnostic plots skipped (residuals are exactly zero). Pareto/effects plots will use Lenth's (1989) pseudo-error method instead of t-tests, which does not require residual degrees of freedom. Add replicates or center points to enable full residual-based analysis."),
            dostop=FALSE)
        # Residuals are identically zero in a saturated fit - diagnostic plots are meaningless
        do_resid  <- FALSE
        use_lenth <- TRUE
    }
    
    # ── External ggplot2 plots (publication-style, now the default whenever
    # the ggplot2 package is installed) ───────────────────────────────────────
    # Renders Main Effects, Interaction, and Pareto Chart of Effects using
    # ggplot2 (clearer labels, no overlap). Previously this only ran when the
    # separate "Publication-Style Plots" checkbox was on, and even then the
    # base-R versions of these same three charts below would ALSO render,
    # producing visible duplicates (this is why Pareto labels looked
    # overlapped -- two Pareto charts, one base-R and one ggplot2, were both
    # being drawn). ggplot2 is now used whenever available, and the base-R
    # duplicates for these three charts are skipped below when it is.
    # Cube, contour, residual, half-normal, and normal plots have no ggplot2
    # equivalent and always continue to show in the SPSS Viewer as before.
    if (has_ggplot2) {
        tryCatch({
            create_external_plots(fit, data, variables, designtype,
                                 do_main, do_inter, do_cube, do_contour,
                                 do_resid, do_pareto, warns, responsevar)
        }, error=function(e) {
            warns$warn(gtxtf("External plot error: %s", e$message), dostop=FALSE)
        })
    }

    # ── Main Effects ─────────────────────────────────────────────────────────
    # (skipped in base-R form when ggplot2 already drew this chart above)
    if (do_main && length(var_names)>0 && !has_ggplot2) {
        tryCatch({
            .doe_submit_plot({
                nv  <- length(var_names)
                nc  <- min(nv, 3); nr <- ceiling(nv/nc)
                old <- par(mfrow=c(nr,nc), mar=c(4.5,4.5,3.5,1.5), oma=c(0,0,3,0),
                          family="sans", cex.lab=1.1, cex.axis=1.0, cex.main=1.0)
                on.exit(par(old), add=TRUE)
                y   <- data[[resp]]
                gm  <- mean(y, na.rm=TRUE)
                for (vn in var_names) {
                    x    <- data[[vn]]
                    xcat <- if (length(unique(x))<=5) as.factor(x) else cut(x, breaks=3)
                    ms   <- tapply(y, xcat, mean, na.rm=TRUE)
                    lvls <- levels(xcat)
                    rng  <- range(c(ms, gm), na.rm=TRUE)
                    pad  <- diff(rng)*0.15
                    # Modern styling
                    plot(seq_along(ms), ms, type="n",
                         xaxt="n", ylim=c(rng[1]-pad, rng[2]+pad),
                         xlab=vn, ylab=resp, main=vn,
                         col.lab="#2C3E50", col.axis="#34495E", col.main="#2C3E50")
                    # Add subtle grid
                    grid(col="gray90", lty=1, lwd=0.5)
                    # Plot line and points with modern style
                    lines(seq_along(ms), ms, col="#3498DB", lwd=2.5)
                    points(seq_along(ms), ms, pch=21, bg="#3498DB", col="white",
                           cex=1.8, lwd=2)
                    axis(1, at=seq_along(ms), labels=lvls, col.axis="#34495E")
                    abline(h=gm, lty=2, col="#95A5A6", lwd=1.5)
                }
                mtext(gtxt("Main Effects Plot"), outer=TRUE, cex=1.3, font=2, col="#2C3E50")
            })
        }, error=function(e) {
            warns$warn(gtxtf("Main effects plot error: %s", e$message), dostop=FALSE)
        })
    }

    # ── Interaction Plot ──────────────────────────────────────────────────────
    # (skipped in base-R form when ggplot2 already drew this chart above)
    if (do_inter && length(var_names)>=2 && !has_ggplot2) {
        tryCatch({
            pairs <- combn(var_names, 2, simplify=FALSE)
            np    <- length(pairs); nc <- min(np,3); nr <- ceiling(np/nc)
            .doe_submit_plot({
                old <- par(mfrow=c(nr,nc), mar=c(4.5,4.5,3.5,1.5), oma=c(0,0,3,0),
                          family="sans", cex.lab=1.1, cex.axis=1.0, cex.main=1.0)
                on.exit(par(old))
                y <- data[[resp]]
                for (pr in pairs) {
                    x1  <- data[[pr[1]]]; x2 <- data[[pr[2]]]
                    x1c <- if (length(unique(x1))<=4) as.factor(x1)
                           else as.factor(ifelse(x1 < median(x1,na.rm=TRUE),"Low","High"))
                    # Modern interaction plot with better styling
                    interaction.plot(x1c, x2, y, fun=mean, type="b",
                                     pch=c(21,24), col=c("#E74C3C","#3498DB"),
                                     lwd=2.5, lty=c(1,2),
                                     xlab=pr[1], ylab=resp, trace.label=pr[2],
                                     main=paste(pr[1],"x",pr[2]),
                                     col.lab="#2C3E50", col.axis="#34495E", col.main="#2C3E50",
                                     legend=TRUE, cex=1.5)
                    grid(col="gray90", lty=1, lwd=0.5)
                }
                mtext(gtxt("Interaction Plot"), outer=TRUE, cex=1.3, font=2, col="#2C3E50")
            })
        }, error=function(e) warns$warn(gtxtf("Interaction plot: %s", e$message), dostop=FALSE))
    }

    # ── Cube Plot ─────────────────────────────────────────────────────────────
    # A cube can only show 3 axes at once. With >3 named factors, rather than
    # silently averaging away the 4th+ factor (the old behavior, which made it
    # look like that factor was dropped/ignored), facet into one small cube
    # per distinct level of whichever remaining factor has a manageable
    # number of distinct values (2-6, i.e. categorical/few-level) -- the
    # standard "small multiples" treatment for >3-factor cube plots. If no
    # remaining factor qualifies (e.g. all are continuous with many distinct
    # values), fall back to holding it at its mean, same as before, but the
    # caption now says explicitly which factor(s) and value(s) were held.
    if (do_cube && length(var_names)>=2) {
        tryCatch({
            nlev_of   <- function(v) length(unique(data[[v]]))
            facet_var <- NULL
            if (length(var_names) > 3) {
                cand <- var_names[sapply(var_names, function(v) { nl <- nlev_of(v); nl>=2 && nl<=6 })]
                if (length(cand) > 0) {
                    nls       <- sapply(cand, nlev_of)
                    facet_var <- cand[which.min(nls)]
                }
            }
            vn        <- if (!is.null(facet_var)) setdiff(var_names, facet_var) else var_names
            vn        <- vn[seq_len(min(3, length(vn)))]
            held_vars <- setdiff(var_names, c(vn, facet_var))

            facet_levels <- if (!is.null(facet_var)) sort(unique(data[[facet_var]])) else NA
            n_facets     <- if (!is.null(facet_var)) length(facet_levels) else 1

            corners_list <- vector("list", n_facets)
            pred_list    <- vector("list", n_facets)
            for (fi in seq_len(n_facets)) {
                corners <- as.data.frame(expand.grid(lapply(vn, function(v)
                    c(min(data[[v]],na.rm=TRUE), max(data[[v]],na.rm=TRUE)))))
                names(corners) <- vn
                if (!is.null(facet_var)) corners[[facet_var]] <- facet_levels[fi]
                for (ov in held_vars) corners[[ov]] <- mean(data[[ov]], na.rm=TRUE)
                corners             <- .doe_add_centerpt(fit, corners)
                corners_list[[fi]] <- corners
                pred_list[[fi]]    <- suppressWarnings(predict(fit, newdata=corners))
            }

            .doe_submit_plot({
                nc  <- min(n_facets, 3); nr <- ceiling(n_facets/nc)
                old <- par(mfrow=c(nr,nc), mar=c(3,3,3,2),
                           oma=c(0,0,if (!is.null(facet_var)) 2 else 0,0))
                on.exit(par(old))
                for (fi in seq_len(n_facets)) {
                    corners   <- corners_list[[fi]]
                    pred_vals <- pred_list[[fi]]
                    panel_title <- if (!is.null(facet_var))
                        gtxtf("%s = %s", facet_var, format(facet_levels[fi], digits=4))
                    else gtxt("Cube Plot (Fitted Means)")
                    if (length(vn)==2) {
                        xv <- corners[[vn[1]]]; yv <- corners[[vn[2]]]
                        plot(xv, yv, type="n",
                             xlim=range(xv)+diff(range(xv))*c(-0.15,0.15),
                             ylim=range(yv)+diff(range(yv))*c(-0.15,0.15),
                             xlab=vn[1], ylab=vn[2], main=panel_title)
                        for (i in seq_along(pred_vals))
                            text(xv[i], yv[i], round(pred_vals[i],3), cex=1.1, col="#2980B9", font=2)
                        box()
                    } else {
                        # 3D cube projection
                        proj <- function(x,y,z) c(x*0.6 + z*0.3, y*0.6 + z*0.2)
                        coords <- list(
                            c(0,0,0),c(1,0,0),c(1,1,0),c(0,1,0),
                            c(0,0,1),c(1,0,1),c(1,1,1),c(0,1,1)
                        )
                        p2d <- t(sapply(coords, function(c) proj(c[1],c[2],c[3])))
                        edges <- list(c(1,2),c(2,3),c(3,4),c(4,1),
                                      c(5,6),c(6,7),c(7,8),c(8,5),
                                      c(1,5),c(2,6),c(3,7),c(4,8))
                        plot(p2d, type="n", xaxt="n", yaxt="n", xlab="", ylab="",
                             main=panel_title, bty="n",
                             xlim=range(p2d[,1])+c(-0.1,0.1),
                             ylim=range(p2d[,2])+c(-0.1,0.1))
                        for (e in edges) lines(p2d[e,1], p2d[e,2], col="gray60")
                        for (i in seq_len(min(8, length(pred_vals))))
                            text(p2d[i,1], p2d[i,2], round(pred_vals[i],2),
                                 cex=0.85, col="#2980B9", font=2)
                        text(mean(p2d[c(1,2),1]), p2d[1,2]-0.08, vn[1], cex=0.8)
                        text(p2d[1,1]-0.08, mean(p2d[c(1,4),2]), vn[2], cex=0.8, srt=90)
                        text(mean(p2d[c(1,5),1])+0.05, mean(p2d[c(1,5),2]), vn[3], cex=0.8)
                    }
                }
                if (!is.null(facet_var))
                    mtext(gtxtf("Cube Plot faceted by %s (other factors held at their mean)", facet_var),
                          outer=TRUE, cex=0.9, font=2, col="#2C3E50")
            })
        }, error=function(e) warns$warn(gtxtf("Cube plot: %s", e$message), dostop=FALSE))
    }

    # ── Contour & Surface ─────────────────────────────────────────────────────
    if (do_contour && length(var_names)>=2) {
        tryCatch({
            v1 <- var_names[1]; v2 <- var_names[2]
            x1s <- seq(min(data[[v1]],na.rm=TRUE), max(data[[v1]],na.rm=TRUE), length.out=40)
            x2s <- seq(min(data[[v2]],na.rm=TRUE), max(data[[v2]],na.rm=TRUE), length.out=40)
            # BUGFIX: outer(X, Y, FUN) calls FUN once with the FULL recycled
            # vectors (length(X)*length(Y) each), not once per (x,y) pair --
            # this function body was written assuming scalar a/b (building a
            # single-row nd per call), which crashed with "replacement has
            # 1600 rows, data has 1" the moment a/b arrived as length-1600
            # vectors. Vectorize() restores the originally-intended one-call-
            # per-pair semantics without changing any of the row-building
            # logic itself.
            pm  <- outer(x1s, x2s, Vectorize(function(a,b) {
                nd <- data[1,,drop=FALSE]
                nd[[v1]] <- a; nd[[v2]] <- b
                for (ov in setdiff(var_names, c(v1,v2)))
                    nd[[ov]] <- mean(data[[ov]], na.rm=TRUE)
                nd[[resp]]     <- NA
                nd             <- .doe_add_centerpt(fit, nd)
                tryCatch(suppressWarnings(predict(fit, newdata=nd)), error=function(e) NA)
            }))
            .doe_submit_plot({
                # oma reserves an outer top margin for an overall panel title
                # (same technique used by Main Effects/Interaction/Residual
                # Diagnostics above) -- additive only; the two existing
                # per-panel main= titles ("Contour Plot", "Response Surface")
                # are unchanged.
                old <- par(mfrow=c(1,2), mar=c(4,4,3,2), oma=c(0,0,3,0))
                on.exit(par(old))
                # Filled contour
                image(x1s, x2s, pm,
                      col=hcl.colors(20,"YlOrRd",rev=TRUE),
                      xlab=v1, ylab=v2, main=gtxt("Contour Plot"))
                contour(x1s, x2s, pm, nlevels=10, add=TRUE, col="gray30")
                box()
                # 3D surface
                persp(x1s, x2s, pm,
                      theta=30, phi=25, expand=0.6, shade=0.3,
                      col=hcl.colors(20,"YlOrRd",rev=TRUE)[
                          cut(c(pm), breaks=20, labels=FALSE, include.lowest=TRUE)
                      ][1:(length(x1s)*length(x2s))],
                      xlab=v1, ylab=v2, zlab=resp,
                      main=gtxt("Response Surface"),
                      ticktype="detailed")
                mtext(gtxt("Contour & Response Surface Plot"), outer=TRUE, cex=1.3, font=2, col="#2C3E50")
            })
        }, error=function(e) warns$warn(gtxtf("Contour plot: %s", e$message), dostop=FALSE))
    }

    # ── Residual Diagnostics ──────────────────────────────────────────────────
    if (do_resid) {
        tryCatch({
            .doe_submit_plot({
                # oma reserves an outer top margin (same technique already used by
                # the Main Effects Plot above) so an overall panel title can be
                # added via mtext(outer=TRUE) below -- purely additive: none of
                # the 4 existing per-panel main= titles are changed.
                old <- par(mfrow=c(2,2), mar=c(4,4,3,1), oma=c(0,0,3,0))
                on.exit(par(old))
                # 1 Residuals vs Fitted
                plot(fit$fitted.values, fit$residuals, pch=19, col="#2980B9", cex=0.8,
                     xlab=gtxt("Fitted Values"), ylab=gtxt("Residuals"),
                     main=gtxt("Residuals vs Fitted"))
                abline(h=0, lty=2, col="red")
                lines(lowess(fit$fitted.values, fit$residuals), col="#C0392B", lwd=2)
                # 2 Normal Q-Q
                qqnorm(fit$residuals, main=gtxt("Normal Q-Q Plot"),
                       pch=19, col="#2980B9", cex=0.8)
                qqline(fit$residuals, col="red", lwd=2)
                # 3 Scale-Location
                sres <- sqrt(abs(rstandard(fit)))
                plot(fit$fitted.values, sres, pch=19, col="#2980B9", cex=0.8,
                     xlab=gtxt("Fitted Values"),
                     ylab=expression(sqrt("|Std. Residuals|")),
                     main=gtxt("Scale-Location"))
                lines(lowess(fit$fitted.values, sres), col="#C0392B", lwd=2)
                # 4 Residuals vs Run Order
                plot(seq_along(fit$residuals), fit$residuals, type="b",
                     pch=19, col="#2980B9", cex=0.8,
                     xlab=gtxt("Run Order"), ylab=gtxt("Residuals"),
                     main=gtxt("Residuals vs Run Order"))
                abline(h=0, lty=2, col="red")
                # Overall panel name -- same pattern as Main Effects Plot's mtext
                # below, so the combined 2x2 image carries a name (Viewer outline
                # / default export filename) instead of being unlabeled.
                mtext(gtxt("Residual Diagnostics"), outer=TRUE, cex=1.3, font=2, col="#2C3E50")
            })
        }, error=function(e) warns$warn(gtxtf("Residual plots: %s", e$message), dostop=FALSE))
    }

    # ── Pareto Chart of Effects ───────────────────────────────────────────────
    # (skipped in base-R form when ggplot2 already drew this chart above --
    # this was the source of the overlapping-label complaint: both the
    # base-R and ggplot2 Pareto charts used to render, one on top of the
    # other's Viewer entry. The Half-Normal and Normal/BsMD plots further
    # below have no ggplot2 equivalent and are untouched by this change.)
    if (do_pareto && !has_ggplot2) {
        tryCatch({
            sm    <- summary(fit)
            coefs <- sm$coefficients
            coefs <- coefs[rownames(coefs)!="(Intercept)",,drop=FALSE]

            if (use_lenth) {
                # Saturated/near-saturated: use Lenth's PSE method instead of t-tests
                eff <- coefs[,1]
                lp  <- lenth_pse(eff)
                if (!is.null(lp)) {
                    tvals <- sort(abs(eff), decreasing=TRUE)
                    tcrit <- lp$ME
                    sig_label   <- gtxtf("ME (95%%) = %.3f  [Lenth's PSE method]", lp$ME)
                } else {
                    tvals <- numeric(0); tcrit <- NA
                }
            } else {
                tvals <- sort(abs(coefs[,3]), decreasing=TRUE)
                tcrit <- qt(0.975, sm$df[2])
                sig_label <- paste0("t* = ", round(tcrit,2), "\n(alpha = 0.05)")
            }

            # Guard against zero residuals and invalid values
            if (length(tvals) > 0 && !is.na(tcrit) && !is.nan(tcrit) && is.finite(max(tvals))) {
                # Re-label bars with short letter codes (A, B, C, ... for
                # factors; AB, ABC, ... for interactions, in the order the
                # factors appear in the design) instead of full term names --
                # standard textbook factorial notation (Montgomery) that
                # keeps long interaction labels readable, paired with a
                # legend mapping each letter back to its variable name.
                tc        <- effect_term_codes(names(tvals), var_names)
                bar_codes <- tc$codes
                .doe_submit_plot({
                    # cex.main kept at the device default (1.0, not 1.2) and the
                    # "(Lenth's PSE method)" qualifier moved to a separate, smaller
                    # mtext() subtitle line rather than appended to main= -- a long
                    # title at cex.main=1.2 on the SPSS Viewer's default (narrow)
                    # plot width was overflowing the device and getting clipped to
                    # a fragment (e.g. just "...Effect..."); a shorter main title
                    # plus a small subtitle keeps everything fully visible.
                    old <- par(mar=c(5,6,4.5,9), family="sans", cex.lab=1.1, cex.axis=1.0, cex.main=1.0, xpd=FALSE)
                    on.exit(par(old))
                    cols <- ifelse(tvals > tcrit, "#C0392B", "#5DADE2")
                    chart_title <- gtxt("Pareto Chart of Effects")
                    xlab_txt <- if (use_lenth) gtxt("Effect Magnitude") else gtxt("Standardized Effect (|t|)")
                    # BUGFIX: xlim used to be sized from the bars alone
                    # (max(tvals)*1.25), with no regard for where the
                    # significance threshold line (tcrit) would be drawn.
                    # Whenever tcrit exceeded every bar -- routine with
                    # Lenth's PSE method on small/noisy designs, and exactly
                    # the scenario the conventional chart is designed to show --
                    # the dashed line and its value label landed at or past
                    # the plot's right edge instead of cleanly inside it.
                    # Including tcrit in the axis-max calculation guarantees
                    # the line is always fully visible, matching the conventional
                    # actual behavior (the line always draws; the conventional
                    # docs note it's omitted only in the unrelated
                    # zero-standard-error case). Purely a display change --
                    # bar heights, tcrit's value, and the significance
                    # coloring above are all untouched.
                    axis_max <- max(c(tvals, tcrit), na.rm=TRUE)
                    bp   <- barplot(tvals, horiz=TRUE, las=1, names.arg=bar_codes,
                                    col=cols, border="white", lwd=1.5,
                                    xlab=xlab_txt,
                                    main=chart_title,
                                    xlim=c(0, axis_max*1.25),
                                    col.lab="#2C3E50", col.axis="#34495E", col.main="#2C3E50")
                    mtext(gtxtf("response is %s%s", resp, if (use_lenth) gtxt(" (Lenth's PSE method)") else gtxt(", alpha = 0.05")),
                          side=3, line=0.4, cex=0.85, col="#2C3E50")
                    # Add grid behind bars
                    grid(col="gray90", lty=1, lwd=0.5, nx=NULL, ny=NA)
                    # Redraw bars on top of grid
                    barplot(tvals, horiz=TRUE, las=1, col=cols, border="white",
                            lwd=1.5, add=TRUE, axes=FALSE)
                    # Significance line, value labeled just above the tallest bar
                    abline(v=tcrit, lty=2, col="#C0392B", lwd=2)
                    text(tcrit, max(bp)+diff(range(bp))*0.06, round(tcrit,3),
                         col="#C0392B", cex=0.8, font=2, xpd=TRUE)
                    legend("bottomright",
                           legend=c(gtxt("Significant"), gtxt("Not significant")),
                           fill=c("#C0392B","#5DADE2"), border="white",
                           bty="n", cex=0.85)
                    # Factor key: letter code -> variable name, drawn in the
                    # right margin (xpd=TRUE lets it sit outside the plot
                    # region so it never overlaps the bars themselves).
                    par(xpd=TRUE)
                    # Anchored to the same axis_max used for xlim above (not
                    # max(tvals) alone) so this key always sits just outside
                    # the actual plot's right edge, even when the threshold
                    # line pushed that edge further out than the bars alone
                    # would have.
                    legend(x=axis_max*1.28, y=max(bp), legend=paste0(tc$legend$Letter, " = ", tc$legend$Name),
                           bty="n", cex=0.78, title=gtxt("Factor"), text.col="#2C3E50",
                           title.col="#2C3E50", xjust=0, yjust=1)
                })
            } else if (use_lenth) {
                warns$warn(gtxt("Pareto chart skipped: too few effects to compute Lenth's PSE (need at least 3)."), dostop=FALSE)
            }
        }, error=function(e) warns$warn(gtxtf("Pareto chart: %s", e$message), dostop=FALSE))
    }

    # ── Half-Normal Plot of Effects (Lenth's method) ─────────────────────────
    # Industry-standard complement to the signed Normal Plot below: plots
    # ordered |effects| against half-normal quantiles, with ME/SME reference
    # lines from Lenth's PSE. Valid whether or not residual df exist, and is
    # the recommended method (standard DOE texts (Montgomery)) for factorial-family
    # designs with sparse active effects.
    if (do_pareto) {
        tryCatch({
            sm    <- summary(fit)
            coefs <- sm$coefficients
            coefs <- coefs[rownames(coefs)!="(Intercept)",,drop=FALSE]
            eff   <- coefs[,1]
            lp    <- lenth_pse(eff)
            if (!is.null(lp)) {
                ord    <- order(abs(eff))
                aeff   <- abs(eff)[ord]
                enames <- names(eff)[ord]
                m      <- length(aeff)
                qprob  <- 0.5 + 0.5 * (seq_len(m) - 0.5) / m
                hq     <- qnorm(qprob)
                .doe_submit_plot({
                    # As with the Pareto chart above: the full title
                    # "Half-Normal Plot of Effects (Lenth's PSE)" at the device's
                    # default cex.main was wide enough to overflow the SPSS
                    # Viewer's default plot width and get clipped. Shortened the
                    # main title and moved the "(Lenth's PSE)" qualifier to a
                    # smaller mtext() subtitle so nothing is lost, just resized.
                    old <- par(mar=c(4.5,4.5,4,1.5), family="sans", cex.main=1.0)
                    on.exit(par(old))
                    plot(hq, aeff, pch=21, bg="#3498DB", col="white", cex=1.6,
                         xlab=gtxt("Half-Normal Quantile"),
                         ylab=gtxt("|Effect|"),
                         main=gtxt("Half-Normal Plot of Effects"),
                         col.lab="#2C3E50", col.axis="#34495E", col.main="#2C3E50")
                    mtext(gtxt("(Lenth's PSE)"), side=3, line=0.4, cex=0.85, col="#2C3E50")
                    grid(col="gray90", lty=1, lwd=0.5)
                    abline(h=lp$ME,  lty=2, col="#E67E22", lwd=2)
                    abline(h=lp$SME, lty=3, col="#E74C3C", lwd=2)
                    sig <- aeff > lp$ME
                    if (any(sig)) text(hq[sig], aeff[sig], enames[sig], pos=4, cex=0.8, col="#C0392B", font=2)
                    legend("topleft",
                           legend=c(gtxtf("ME (95%%) = %.3f", lp$ME), gtxtf("SME (95%%) = %.3f", lp$SME)),
                           lty=c(2,3), col=c("#E67E22","#E74C3C"), lwd=2, bty="n", cex=0.85)
                })
            }
        }, error=function(e) warns$warn(gtxtf("Half-normal plot: %s", e$message), dostop=FALSE))
    }

    # ── Normal Plot of Effects (if BsMD available) ────────────────────────────
    if (do_pareto && has_BsMD) {
        tryCatch({
            sm    <- summary(fit)
            coefs <- sm$coefficients
            coefs <- coefs[rownames(coefs)!="(Intercept)",,drop=FALSE]
            eff   <- coefs[,1]  # actual effects (estimates)
            .doe_submit_plot({
                old <- par(mar=c(4,4,3,2))
                on.exit(par(old))
                qqnorm(eff, main=gtxt("Normal Plot of Effects"),
                       pch=19, col="#2980B9",
                       xlab=gtxt("Normal Score"), ylab=gtxt("Effect"))
                qqline(eff, col="red", lty=2)
                # label outliers
                q   <- qqnorm(eff, plot.it=FALSE)
                thr <- 1.5*IQR(q$y, na.rm=TRUE)
                out <- abs(q$y - median(q$y, na.rm=TRUE)) > thr
                if (any(out)) text(q$x[out], q$y[out], names(eff)[out],
                                   pos=4, cex=0.7, col="#C0392B")
            })
        }, error=function(e) NULL)  # silent - optional
    }

    # ── Curvature Check Plot (Cube vs. Center Point average response) ───────
    # Companion visual to the additive curvature TERM in fit_model() (see the
    # "Curvature check term" comment there): plots the average response at
    # the factorial ("Cube") points against the average response at the
    # Center points. A roughly flat/horizontal line indicates no curvature; a
    # pronounced slope indicates the true surface bends within the design
    # region. Purely additive -- gated by its own off-by-default flag, and
    # silently skipped (with an informational note, not an error) whenever no
    # CenterPt marker column exists or it doesn't actually vary, exactly
    # mirroring the guard already used for the curvature TERM.
    if (do_curvature) {
        tryCatch({
            if (!("CenterPt" %in% names(data))) {
                warns$warn(gtxt("Curvature plot skipped: no center-point runs found in the data (requires the optional center-points feature)."), dostop=FALSE)
            } else {
                cp_vals <- suppressWarnings(as.numeric(data$CenterPt))
                if (length(unique(cp_vals[!is.na(cp_vals)])) < 2) {
                    warns$warn(gtxt("Curvature plot skipped: CenterPt column does not vary (no factorial/center contrast available)."), dostop=FALSE)
                } else if (!(resp %in% names(data))) {
                    warns$warn(gtxt("Curvature plot skipped: response variable not found in the data."), dostop=FALSE)
                } else {
                    y_all   <- suppressWarnings(as.numeric(data[[resp]]))
                    # NA-safe row selection: rows where CenterPt itself is NA
                    # (e.g. the newly-added axial/center rows from the
                    # AUGMENTTOCCD feature, which carry no CenterPt value)
                    # must resolve to FALSE here, not NA, otherwise sum(is_cube)
                    # below would itself become NA and abort the whole plot.
                    valid_row <- !is.na(cp_vals) & !is.na(y_all)
                    is_cube <- valid_row & (cp_vals == 0)
                    is_ctr  <- valid_row & (cp_vals == 1)
                    if (sum(is_cube) >= 1 && sum(is_ctr) >= 1) {
                        cube_y <- y_all[is_cube]
                        ctr_y  <- y_all[is_ctr]
                        means  <- c(mean(cube_y), mean(ctr_y))
                        ses    <- c(if (length(cube_y) > 1) sd(cube_y)/sqrt(length(cube_y)) else 0,
                                    if (length(ctr_y)  > 1) sd(ctr_y) /sqrt(length(ctr_y))  else 0)
                        .doe_submit_plot({
                            old <- par(mar=c(5,5,4.5,2), family="sans", cex.lab=1.1, cex.axis=1.0, cex.main=1.0)
                            on.exit(par(old))
                            ylim_pad <- max(c(ses, 0), na.rm=TRUE) * 1.5 + diff(range(means)) * 0.1 + 1e-9
                            plot(1:2, means, type="b", pch=19, cex=1.6, lwd=2, col="#2980B9",
                                 xlim=c(0.6, 2.4), ylim=range(means) + c(-1,1) * ylim_pad,
                                 xaxt="n", xlab="", ylab=gtxtf("Mean %s", resp),
                                 main=gtxt("Curvature Check Plot"),
                                 col.lab="#2C3E50", col.axis="#34495E", col.main="#2C3E50")
                            axis(1, at=1:2, labels=c(gtxt("Factorial (Cube) Points"), gtxt("Center Points")),
                                 col.axis="#34495E")
                            mtext(gtxtf("response is %s", resp), side=3, line=0.4, cex=0.85, col="#2C3E50")
                            grid(col="gray90", lty=1, lwd=0.5, nx=NA, ny=NULL)
                            segments(1:2, means - ses, 1:2, means + ses, col="#2980B9", lwd=1.5)
                            segments((1:2) - 0.05, means - ses, (1:2) + 0.05, means - ses, col="#2980B9", lwd=1.5)
                            segments((1:2) - 0.05, means + ses, (1:2) + 0.05, means + ses, col="#2980B9", lwd=1.5)
                            text(1:2, means, round(means, 3), pos=3, cex=0.85, col="#2C3E50", font=2)
                        })
                    } else {
                        warns$warn(gtxt("Curvature plot skipped: need at least one factorial point and one center point with non-missing response values."), dostop=FALSE)
                    }
                }
            }
        }, error=function(e) warns$warn(gtxtf("Curvature plot: %s", e$message), dostop=FALSE))
    }

    spsspkg.EndProcedure()
}

# ════════════════════════════════════════════════════════════════════════════
# EXTERNAL GGPLOT2 PLOTS (SEPARATE WINDOW)
# ════════════════════════════════════════════════════════════════════════════

create_external_plots <- function(fit, data, variables, designtype,
                                   do_main, do_inter, do_cube, do_contour,
                                   do_resid, do_pareto, warns, responsevar=NULL) {

    if (!requireNamespace("ggplot2", quietly=TRUE)) {
        warns$warn(gtxt("ggplot2 package required for external plots. Install with: install.packages('ggplot2')"),
                   dostop=FALSE)
        return(NULL)
    }

    suppressMessages(suppressWarnings(library(ggplot2)))

    resp      <- if (!is.null(responsevar) && responsevar %in% names(data)) responsevar else tail(names(data), 1)
    meta_cols <- c("Reps","Proportion","StdOrder","RunOrder","Block","CenterPt","PtType")
    var_names <- intersect(as.character(variables),
                           setdiff(names(data), c(meta_cols, resp)))

    plot_list <- list()
    
    # ── Main Effects Plot (ggplot2) ──────────────────────────────────────────
    if (do_main && length(var_names) > 0) {
        tryCatch(suppressWarnings({
            y <- data[[resp]]
            gm <- mean(y, na.rm=TRUE)
            
            # Prepare data for all variables
            plot_data_list <- lapply(var_names, function(vn) {
                x <- data[[vn]]
                xcat <- if (length(unique(x)) <= 5) as.factor(x) else cut(x, breaks=3)
                ms <- tapply(y, xcat, mean, na.rm=TRUE)
                data.frame(
                    Variable = vn,
                    Level = names(ms),
                    Mean = as.numeric(ms),
                    stringsAsFactors = FALSE
                )
            })
            plot_data <- do.call(rbind, plot_data_list)
            
            # Create faceted main effects plot
            p <- ggplot(plot_data, aes(x=Level, y=Mean, group=Variable)) +
                geom_line(color="#3498DB", linewidth=1.5) +
                geom_point(color="#3498DB", fill="white", shape=21, size=4, stroke=2) +
                geom_hline(yintercept=gm, linetype="dashed", color="#95A5A6", linewidth=1) +
                facet_wrap(~Variable, scales="free_x", ncol=3) +
                labs(title="Main Effects Plot", 
                     x="Factor Level", 
                     y=paste("Mean", resp)) +
                theme_minimal(base_size=10) +
                theme(
                    plot.title = element_text(hjust=0.5, face="bold", size=13, color="#2C3E50"),
                    axis.title = element_text(face="bold", color="#2C3E50", size=9.5),
                    axis.text = element_text(color="#34495E", size=8),
                    strip.text = element_text(face="bold", size=9, color="#2C3E50"),
                    strip.background = element_rect(fill="#ECF0F1", color=NA),
                    panel.grid.major = element_line(color="#E8E8E8"),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(color="#BDC3C7", fill=NA, linewidth=0.5)
                )

            plot_list[["main_effects"]] <- p
            # Route through spssRGraphics.Submit() like every other plot in
            # this file — printing a ggplot grob directly (bypassing Submit)
            # can leave a stray/competing graphics device open under the R
            # Integration Plug-in's batch session, which then breaks the
            # *next* spssRGraphics.Submit() call with "invalid 'file' argument".
            # ggplot2's theme text is specified in fixed point sizes. An
            # earlier version of this fix tried enlarging the canvas
            # (width=1400,height=1000,res=130) to shrink text relative to
            # the plot area -- but that also made the exported image
            # physically larger, so it displayed oversized/overflowing in
            # the SPSS Viewer (reported: "even bigger... weirdly fitting
            # almost the window"). The correct fix is the opposite: keep the
            # SAME canvas as every other chart in this file (default
            # 960x720@115, i.e. no width/height/res override here) and
            # instead reduce the theme's absolute point sizes above so text
            # is proportioned correctly on that standard canvas.
            .doe_submit_plot({ print(p) })

        }), error=function(e) {
            warns$warn(gtxtf("External main effects plot error: %s", e$message), dostop=FALSE)
        })
    }
    
    # ── Interaction Plot (ggplot2) ───────────────────────────────────────────
    if (do_inter && length(var_names) >= 2) {
        tryCatch(suppressWarnings({
            pairs <- combn(var_names, 2, simplify=FALSE)
            
            # Prepare data for all interactions
            int_data_list <- lapply(pairs, function(pr) {
                v1 <- pr[1]; v2 <- pr[2]
                x1 <- data[[v1]]; x2 <- data[[v2]]
                y <- data[[resp]]
                
                x1cat <- if (length(unique(x1)) <= 5) as.factor(x1) else cut(x1, breaks=3)
                x2cat <- if (length(unique(x2)) <= 5) as.factor(x2) else cut(x2, breaks=3)
                
                agg <- aggregate(y ~ x1cat + x2cat, FUN=mean, na.rm=TRUE)
                agg$Var1 <- v1
                agg$Var2 <- v2
                names(agg)[1:2] <- c("Level1", "Level2")
                agg
            })
            int_data <- do.call(rbind, int_data_list)
            int_data$Interaction <- paste(int_data$Var1, "x", int_data$Var2)
            
            # Create faceted interaction plot
            p <- ggplot(int_data, aes(x=Level1, y=y, color=Level2, group=Level2)) +
                geom_line(linewidth=1.5) +
                geom_point(size=4, shape=21, fill="white", stroke=2) +
                facet_wrap(~Interaction, scales="free", ncol=3) +
                labs(title="Interaction Plot",
                     x="Factor Level",
                     y=paste("Mean", resp),
                     color="Second Factor") +
                scale_color_brewer(palette="Set1") +
                theme_minimal(base_size=10) +
                theme(
                    plot.title = element_text(hjust=0.5, face="bold", size=13, color="#2C3E50"),
                    axis.title = element_text(face="bold", color="#2C3E50", size=9.5),
                    axis.text = element_text(color="#34495E", angle=45, hjust=1, size=8),
                    legend.title = element_text(face="bold", size=8.5),
                    legend.text = element_text(size=8),
                    legend.position = "bottom",
                    strip.text = element_text(face="bold", size=8.5, color="#2C3E50"),
                    strip.background = element_rect(fill="#ECF0F1", color=NA),
                    panel.grid.major = element_line(color="#E8E8E8"),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(color="#BDC3C7", fill=NA, linewidth=0.5)
                )

            plot_list[["interactions"]] <- p
            # See the Main Effects Plot comment above: keep the SAME canvas
            # as every other chart in this file (default 960x720@115, no
            # width/height/res override) and rely on the smaller theme point
            # sizes above instead of an enlarged canvas.
            .doe_submit_plot({ print(p) })

        }), error=function(e) {
            warns$warn(gtxtf("External interaction plot error: %s", e$message), dostop=FALSE)
        })
    }
    
    # ── Pareto Chart (ggplot2) ───────────────────────────────────────────────
    # NOTE: a genuine "Pareto Chart of Effects" (commercial DOE packages
    # convention; Montgomery's "Design and Analysis of Experiments") is a bar
    # chart of |standardized effect| (or |effect| under Lenth's PSE when the
    # model is saturated) sorted in descending order, with a SINGLE
    # significance reference line at the critical t-value (or Lenth's ME).
    # It deliberately has no cumulative-percentage curve -- unlike a classic
    # QC/Juran Pareto chart of defect counts, effect magnitudes are signed
    # regression coefficients on an arbitrary scale, so "percent of the sum
    # of all effects" is not a meaningful quantity here. The previous
    # implementation copied the QC-chart cumulative-% line by mistake; this
    # version matches the base-R spssRGraphics Pareto chart above instead.
    if (do_pareto) {
        tryCatch(suppressWarnings({
            sm <- summary(fit)
            coefs <- sm$coefficients
            coefs <- coefs[rownames(coefs) != "(Intercept)", , drop=FALSE]

            df_resid  <- sm$df[2]
            use_lenth <- df_resid < 1

            if (use_lenth) {
                lp <- lenth_pse(coefs[,1])
                if (is.null(lp)) {
                    eff_vals <- numeric(0); tcrit <- NA; ylab_txt <- gtxt("Effect Magnitude")
                } else {
                    eff_vals <- abs(coefs[,1]); tcrit <- lp$ME; ylab_txt <- gtxt("Effect Magnitude")
                }
            } else {
                eff_vals <- abs(coefs[,3]); tcrit <- qt(0.975, df_resid); ylab_txt <- gtxt("Standardized Effect (|t|)")
            }

            if (length(eff_vals) > 0 && !is.na(tcrit)) {
                # Letter-code the terms (A, B, C, ... / AB, ABC, ... for
                # interactions, in design order) so long interaction names
                # stay legible, with a Factor key in the subtitle mapping
                # each letter back to its variable name -- see comment on
                # the base-R Pareto chart above for the same convention.
                tc <- effect_term_codes(rownames(coefs), var_names)
                eff_data <- data.frame(
                    Term = tc$codes,
                    Effect = eff_vals,
                    stringsAsFactors = FALSE
                )
                eff_data <- eff_data[order(eff_data$Effect), ]
                eff_data$Term <- factor(eff_data$Term, levels=eff_data$Term)
                eff_data$Significant <- ifelse(eff_data$Effect > tcrit,
                                                gtxt("Significant"), gtxt("Not significant"))
                factor_key <- paste(paste0(tc$legend$Letter, "=", tc$legend$Name), collapse="  ")

                p <- ggplot(eff_data, aes(x=Term, y=Effect, fill=Significant)) +
                    geom_col(color="white", width=0.7) +
                    coord_flip() +
                    geom_hline(yintercept=tcrit, linetype="dashed", color="#C0392B", linewidth=1) +
                    annotate("text", x=Inf, y=tcrit, label=sprintf("%.3f", tcrit),
                             color="#C0392B", size=3.2, fontface="bold", vjust=-0.6, hjust=0.5) +
                    # BUGFIX: geom_hline() does not itself expand the y-scale
                    # to include its own yintercept (confirmed against
                    # ggplot2's own reference docs: "They also do not affect
                    # the x and y scales" -- geom_abline.html). Whenever tcrit
                    # exceeds every bar's Effect value -- routine with Lenth's
                    # PSE method on small/noisy designs -- the dashed
                    # threshold line and its value label above could render
                    # right at or past the panel's edge instead of cleanly
                    # inside it, instead of the fully-visible line the conventional
                    # own Pareto Chart of Effects always draws. expand_limits()
                    # guarantees tcrit is included in the trained range without
                    # overriding ggplot2's own default padding/expansion for
                    # the bars, so normal charts (tcrit already inside the
                    # bars' range) are completely unaffected.
                    expand_limits(y = tcrit) +
                    scale_fill_manual(values=c(setNames("#C0392B", gtxt("Significant")),
                                                setNames("#5DADE2", gtxt("Not significant")))) +
                    labs(title=gtxt("Pareto Chart of Effects"),
                         subtitle=gtxtf("response is %s%s   |   %s",
                                        resp, if (use_lenth) gtxt(" (Lenth's PSE method)") else gtxt(", alpha = 0.05"),
                                        factor_key),
                         x=NULL,
                         y=ylab_txt,
                         fill=NULL) +
                    theme_minimal(base_size=10) +
                    theme(
                        plot.title = element_text(hjust=0.5, face="bold", size=13, color="#2C3E50"),
                        plot.subtitle = element_text(hjust=0.5, size=8, color="#2C3E50"),
                        legend.position = "bottom",
                        legend.text = element_text(size=8),
                        axis.title = element_text(face="bold", color="#2C3E50", size=9.5),
                        axis.text = element_text(color="#34495E", size=8),
                        panel.grid.major.y = element_blank(),
                        panel.grid.major.x = element_line(color="#E8E8E8"),
                        panel.grid.minor = element_blank()
                    )

                plot_list[["pareto"]] <- p
                # See the Main Effects Plot comment above: keep the SAME
                # canvas as every other chart in this file (default
                # 960x720@115, no width/height/res override) and rely on the
                # smaller theme point sizes above instead of an enlarged
                # canvas.
                .doe_submit_plot({ print(p) })
            }

        }), error=function(e) {
            warns$warn(gtxtf("External Pareto plot error: %s", e$message), dostop=FALSE)
        })
    }
    
    
    invisible(plot_list)
}


# ════════════════════════════════════════════════════════════════════════════
# VARIABLE SELECTION CHARTS
# ════════════════════════════════════════════════════════════════════════════

create_varselect_plots <- function(vsel,
                                    do_selectiontrace=FALSE,
                                    do_fitprofile=FALSE,
                                    do_parsimonyplot=FALSE,
                                    do_factormap=FALSE,
                                    warns) {

    if (is.null(vsel)) return(invisible(NULL))
    any_chart <- do_selectiontrace || do_fitprofile || do_parsimonyplot || do_factormap
    if (!any_chart) return(invisible(NULL))

    while (!is.null(dev.list())) tryCatch(dev.off(), error=function(e) NULL)
    .doe_old_wd <- getwd()
    on.exit(tryCatch(setwd(.doe_old_wd), error=function(e) NULL), add=TRUE)
    # See .doe_find_writable_dir() near create_all_plots() -- plain
    # tempdir() was tried here before and was not sufficient on every
    # install (still produced "invalid 'file' argument").
    .doe_wd <- .doe_find_writable_dir()
    if (!is.null(.doe_wd)) tryCatch(setwd(.doe_wd), error=function(e) NULL)

    StartProcedure(gtxt("Variable Selection Charts"), "STATSDOEVSPLOTS")

    col_line   <- "#2E86AB"
    col_best   <- "#E74C3C"
    col_second <- "#E67E22"
    col_good   <- "#27AE60"
    col_ax     <- "#34495E"
    col_lab    <- "#2C3E50"

    # Chart 1 – Selection Criterion Trace (Stepwise)
    if (do_selectiontrace) {
        if (vsel$type != "stepwise") {
            warns$warn(gtxtf("Selection Criterion Trace applies to Stepwise selection only; skipped. (Detected variable selection method: '%s'.)", vsel$type), dostop=FALSE)
        } else {
            tryCatch(.doe_submit_plot({
                df <- vsel$df
                if (nrow(df) == 0) {
                    plot.new(); text(0.5, 0.5, gtxt("No steps taken."), cex=1.2, col=col_ax)
                    title(main=gtxt("Selection Criterion Trace"), col.main=col_lab)
                } else {
                    best_idx <- which.min(df$AIC)
                    rng <- range(df$AIC)
                    pad <- diff(rng)*0.18; if (pad==0) pad <- 1
                    par(mar=c(7,5,4,2), family="sans", bg="white",
                        col.lab=col_lab, col.axis=col_ax, col.main=col_lab)
                    plot(df$Step, df$AIC, type="b", pch=19, col=col_line, lwd=2.2, cex=1.5,
                         xlim=c(0.5, nrow(df)+0.5), ylim=c(rng[1]-pad, rng[2]+pad),
                         xaxt="n", xlab="", ylab="AIC  (lower = better)",
                         main=gtxt("Selection Criterion Trace"))
                    axis(1, at=df$Step, labels=df$Step, col.axis=col_ax)
                    mtext(gtxt("Selection Step"), side=1, line=2.5, col=col_lab, cex=0.95)
                    grid(col="gray90", lty=1, lwd=0.5)
                    points(df$Step[best_idx], df$AIC[best_idx], pch=18, col=col_best, cex=3)
                    abline(h=df$AIC[nrow(df)], lty=2, col=col_best, lwd=1.3)
                    mtext(df$Action, at=df$Step, side=1, line=4.5, cex=0.72, col="#555555", las=2)
                    legend("topright",
                           legend=c(gtxt("AIC at each step"), gtxt("Optimal step"), gtxt("Final AIC level")),
                           col=c(col_line, col_best, col_best),
                           pch=c(19, 18, NA), lty=c(1, NA, 2),
                           pt.cex=c(1.5, 2.5, NA), lwd=c(2.2, NA, 1.3), bty="n", cex=0.82)
                    mtext(gtxt("Lower AIC = better model. Optimal step marked in red."),
                          side=3, line=0, cex=0.75, col="#777777")
                }
            }), error=function(e) warns$warn(gtxtf("Selection Criterion Trace error: %s", e$message), dostop=FALSE))
        }
    }

    # Chart 2 – Predictive Fit Profile (Best Subsets)
    if (do_fitprofile) {
        if (vsel$type != "bestsubsets") {
            warns$warn(gtxtf("Predictive Fit Profile applies to Best Subsets only; skipped. (Detected variable selection method: '%s'.)", vsel$type), dostop=FALSE)
        } else {
            tryCatch(.doe_submit_plot({
                df    <- vsel$df
                r2    <- df[["R2 (%)"]]
                adjr2 <- df[["AdjR2 (%)"]]
                sizes <- df$Size
                ymin  <- min(c(r2, adjr2)) - 2
                ymax  <- max(c(r2, adjr2)) + 3
                par(mar=c(5,5,4,2), family="sans", bg="white",
                    col.lab=col_lab, col.axis=col_ax, col.main=col_lab)
                plot(sizes, r2, type="b", pch=19, col=col_line, lwd=2.2, cex=1.4,
                     ylim=c(ymin, ymax),
                     xlab=gtxt("Number of Predictors in Model"),
                     ylab=gtxt("Variance Explained (%)"),
                     main=gtxt("Predictive Fit Profile"), xaxt="n")
                axis(1, at=sizes, labels=sizes, col.axis=col_ax)
                grid(col="gray90", lty=1, lwd=0.5)
                lines(sizes, adjr2, type="b", pch=17, col=col_second, lwd=2.2, cex=1.4)
                best_idx <- which.max(adjr2)
                abline(v=sizes[best_idx], lty=2, col=col_best, lwd=1.8)
                polygon(c(sizes[best_idx]-0.08, sizes[best_idx]+0.08,
                           sizes[best_idx]+0.08, sizes[best_idx]-0.08),
                         c(adjr2[best_idx], adjr2[best_idx], r2[best_idx], r2[best_idx]),
                         col=adjustcolor(col_best, alpha.f=0.12), border=NA)
                legend("bottomright",
                       legend=c(gtxt("R-sq (%)"), gtxt("Adj. R-sq (%)"), gtxt("Recommended size")),
                       col=c(col_line, col_second, col_best),
                       pch=c(19, 17, NA), lty=c(1, 1, 2),
                       lwd=c(2.2, 2.2, 1.8), bty="n", cex=0.85)
                gap <- r2 - adjr2
                mtext(sprintf(gtxt("Overfitting gap at recommended size (%d predictors): %.1f%%"),
                              sizes[best_idx], gap[best_idx]),
                      side=1, line=4, cex=0.74, col="#666666")
            }), error=function(e) warns$warn(gtxtf("Predictive Fit Profile error: %s", e$message), dostop=FALSE))
        }
    }

    # Chart 3 – Model Parsimony Plot (Best Subsets)
    if (do_parsimonyplot) {
        if (vsel$type != "bestsubsets") {
            warns$warn(gtxtf("Model Parsimony Plot applies to Best Subsets only; skipped. (Detected variable selection method: '%s'.)", vsel$type), dostop=FALSE)
        } else {
            tryCatch(.doe_submit_plot({
                df    <- vsel$df
                p_par <- df$Size + 1
                cp    <- df$Cp
                ymax  <- max(cp, max(p_par)) * 1.15
                pt_col <- ifelse(cp <= p_par, col_good, col_second)
                par(mar=c(5,5,4,2), family="sans", bg="white",
                    col.lab=col_lab, col.axis=col_ax, col.main=col_lab)
                plot(p_par, cp, pch=21, bg=pt_col, col="white", cex=2.2, lwd=0.5,
                     xlim=c(min(p_par)-0.6, max(p_par)+0.6), ylim=c(0, ymax),
                     xlab=gtxt("Parameters in Model  (predictors + intercept)"),
                     ylab=gtxt("Mallows\' Cp Statistic"),
                     main=gtxt("Model Parsimony Plot"), xaxt="n")
                axis(1, at=p_par, labels=p_par, col.axis=col_ax)
                grid(col="gray90", lty=1, lwd=0.5)
                abline(a=0, b=1, col=col_best, lwd=1.6, lty=2)
                ref_x <- max(p_par)*0.55
                text(ref_x, ref_x*1.05, labels=gtxt("Cp = p  (reference)"),
                     col=col_best, cex=0.82, srt=40)
                text(p_par, cp, labels=round(cp,1), pos=ifelse(cp<=p_par,3,1), cex=0.75, col="#444444")
                legend("topleft",
                       legend=c(gtxt("Cp <= p  (good fit)"), gtxt("Cp > p  (add more terms?)"), gtxt("Cp = p  (reference line)")),
                       col=c(col_good, col_second, col_best),
                       pch=c(21,21,NA), pt.bg=c(col_good, col_second, NA),
                       lty=c(NA,NA,2), lwd=c(NA,NA,1.6), pt.cex=c(1.8,1.8,NA), bty="n", cex=0.82)
                mtext(gtxt("Models at or below the reference line have low bias and are candidates for selection."),
                      side=1, line=4, cex=0.74, col="#666666")
            }), error=function(e) warns$warn(gtxtf("Model Parsimony Plot error: %s", e$message), dostop=FALSE))
        }
    }

    # Chart 4 – Factor Inclusion Map (Best Subsets)
    if (do_factormap) {
        if (vsel$type != "bestsubsets" || is.null(vsel$which_matrix)) {
            warns$warn(gtxtf("Factor Inclusion Map applies to Best Subsets only; skipped. (Detected variable selection method: '%s'.)", vsel$type), dostop=FALSE)
        } else {
            tryCatch(.doe_submit_plot({
                wm <- vsel$which_matrix
                wm <- wm[, colnames(wm) != "(Intercept)", drop=FALSE]
                if (ncol(wm)==0 || nrow(wm)==0) {
                    plot.new(); text(0.5, 0.5, gtxt("No factors available."), cex=1.1)
                    title(main=gtxt("Factor Inclusion Map"), col.main=col_lab)
                } else {
                    nf   <- ncol(wm); ns <- nrow(wm)
                    stab <- round(colSums(wm)/ns*100)
                    best_s <- vsel$best_size
                    pal  <- colorRampPalette(c("gray94", "#2E86AB"))(100)
                    incl_mat   <- matrix(as.numeric(wm), nrow=ns, ncol=nf)
                    stab_norm  <- stab/100
                    intensity  <- t(t(incl_mat) * (0.5 + 0.5*stab_norm))
                    intensity[intensity>1] <- 1
                    par(mar=c(9,5,4,1), family="sans", bg="white",
                        col.lab=col_lab, col.axis=col_ax, col.main=col_lab)
                    image(1:nf, 1:ns, t(intensity), xaxt="n", yaxt="n",
                          xlab="", ylab=gtxt("Model Size (number of predictors)"),
                          main=gtxt("Factor Inclusion Map"), col=pal, zlim=c(0,1))
                    axis(1, at=1:nf, labels=colnames(wm), las=2,
                         cex.axis=max(0.65, 1-nf*0.03), col.axis=col_ax)
                    axis(2, at=1:ns, labels=1:ns, cex.axis=0.9, col.axis=col_ax)
                    mtext(gtxt("Factor"), side=1, line=7.5, col=col_lab, cex=0.9)
                    abline(h=seq(0.5, ns+0.5, 1), col="white", lwd=0.8)
                    abline(v=seq(0.5, nf+0.5, 1), col="white", lwd=0.8)
                    if (!is.null(best_s) && best_s>=1 && best_s<=ns)
                        rect(0.5, best_s-0.5, nf+0.5, best_s+0.5, border=col_best, lwd=2.5, lty=1)
                    mtext(paste0(stab, "%"), at=1:nf, side=1, line=5.8, cex=0.72, col="#444444")
                    mtext(gtxt("Stability"), at=0.0, side=1, line=5.8, cex=0.72, col="#444444", adj=1)
                    mtext(gtxt("Darker fill = higher stability. Red border = recommended model (peak Adj. R-sq)."),
                          side=3, line=0, cex=0.75, col="#777777")
                }
            }), error=function(e) warns$warn(gtxtf("Factor Inclusion Map error: %s", e$message), dostop=FALSE))
        }
    }

    spsspkg.EndProcedure()
    invisible(NULL)
}

# ════════════════════════════════════════════════════════════════════════════
# RESPONSE OPTIMIZER
# ════════════════════════════════════════════════════════════════════════════

do_optimization <- function(fit, data, variables, spec, goal, warns, responsevar=NULL, optdetailtable=FALSE, target=NA_real_) {
    resp      <- if (!is.null(responsevar) && responsevar %in% names(data)) responsevar else tail(names(data), 1)
    meta_cols <- c("Reps","Proportion","StdOrder","RunOrder","Block","CenterPt","PtType")
    var_names <- intersect(as.character(variables),
                           setdiff(names(data), c(meta_cols, resp)))
    if (length(var_names)==0) return(NULL)

    # Resolved numeric target for goal=="target": use the caller-supplied
    # `target` when it's a real, finite number; otherwise fall back to the
    # original, pre-existing behavior (mean of the observed response) so
    # every run that doesn't pass `target` is completely unaffected.
    target_resolved <- if (!is.null(target) && length(target) > 0 && is.finite(target[1]))
        as.numeric(target[1]) else mean(data[[resp]], na.rm=TRUE)
    # Deferred-plot closure: NULL unless the optimizer succeeds below, in
    # which case it's set to a zero-arg function the CALLER can invoke later
    # (see comment further down) to draw the optimization plot in its own
    # separate "plots" phase instead of inline here among this function's
    # own tables.
    plot_fn <- NULL

    # BUGFIX: this function calls spssRGraphics.Submit() (below, for the
    # optimization plot) without the same device/cwd defensive guard that
    # create_all_plots() and create_varselect_plots() have. That meant THIS
    # function could leave a bad device/cwd state behind for whichever of
    # those runs next (in Run()'s actual call order, create_varselect_plots()
    # runs immediately after this function) -- matching a report where every
    # other chart rendered fine except the next function's first plot, which
    # failed with "invalid 'file' argument". Same guard as the other plotting
    # functions, applied here too so this one can't leave a mess for the next.
    while (!is.null(dev.list())) tryCatch(dev.off(), error=function(e) NULL)
    .doe_old_wd <- getwd()
    on.exit(tryCatch(setwd(.doe_old_wd), error=function(e) NULL), add=TRUE)
    .doe_wd <- .doe_find_writable_dir()
    if (!is.null(.doe_wd)) tryCatch(setwd(.doe_wd), error=function(e) NULL)

    StartProcedure(gtxt("Response Optimizer"), "STATSDOEOPT")

    lowers <- sapply(var_names, function(v) {
        i <- which(as.character(spec$var)==v)
        if (length(i)) as.numeric(spec$lows[i[1]]) else min(data[[v]], na.rm=TRUE)
    })
    uppers <- sapply(var_names, function(v) {
        i <- which(as.character(spec$var)==v)
        if (length(i)) as.numeric(spec$highs[i[1]]) else max(data[[v]], na.rm=TRUE)
    })
    starts <- (lowers + uppers) / 2

    obj_fn <- function(x) {
        nd <- as.data.frame(t(x)); names(nd) <- var_names
        for (ov in setdiff(setdiff(names(data), resp), c(meta_cols, var_names)))
            nd[[ov]] <- mean(data[[ov]], na.rm=TRUE)
        nd   <- .doe_add_centerpt(fit, nd)
        pred <- tryCatch(suppressWarnings(predict(fit, newdata=nd)), error=function(e) NA)
        if (is.na(pred)) return(1e10)
        if (goal=="maximize") return(-pred)
        if (goal=="minimize") return( pred)
        return(abs(pred - target_resolved))
    }

    # Use desirability if available for desirability score display
    desir_score <- NA
    if (requireNamespace("desirability", quietly=TRUE)) {
        library(desirability)
        yr    <- range(data[[resp]], na.rm=TRUE)
        # dTarget(low, target, high) requires low <= target <= high. A
        # caller-supplied target can legitimately fall outside the observed
        # response range (extrapolating beyond what's been measured is often
        # the whole point of specifying one) -- widen the bounds to include it
        # instead of letting dTarget() error. When target_resolved is the
        # historical mean(yr) fallback, it's always inside [yr[1],yr[2]]
        # already, so dt_lo/dt_hi collapse to plain yr[1]/yr[2] and nothing
        # changes for existing runs.
        dt_lo <- min(yr[1], target_resolved)
        dt_hi <- max(yr[2], target_resolved)
        d_fn  <- switch(goal,
            "maximize" = dMax(yr[1], yr[2]),
            "minimize" = dMin(yr[1], yr[2]),
            "target"   = dTarget(dt_lo, target_resolved, dt_hi))
        # Multi-start: grid search
        gn    <- 4
        grid  <- as.matrix(expand.grid(lapply(seq_along(var_names), function(i)
            seq(lowers[i], uppers[i], length.out=gn))))
        bv    <- Inf; bs <- starts
        # grid_vals: additive bookkeeping only, same point made in the
        # multi-response optimizer above -- does not change bv/bs selection
        # or therefore the optimizer's result; used later (optdetailtable) to
        # list the next-best distinct grid points.
        grid_vals <- rep(NA_real_, nrow(grid))
        for (r in seq_len(nrow(grid))) {
            v <- obj_fn(grid[r,])
            grid_vals[r] <- v
            if (!is.na(v) && v < bv) { bv <- v; bs <- grid[r,] }
        }
        starts <- bs
    }

    opt <- tryCatch(
        optim(par=starts, fn=obj_fn, method="L-BFGS-B",
              lower=lowers, upper=uppers, control=list(maxit=1000)),
        error=function(e) { warns$warn(gtxtf("Optimizer failed: %s", e$message), dostop=FALSE); NULL }
    )

    if (!is.null(opt)) {
        opt_x  <- opt$par
        nd_opt <- as.data.frame(t(opt_x)); names(nd_opt) <- var_names
        for (ov in setdiff(setdiff(names(data), resp), c(meta_cols, var_names)))
            nd_opt[[ov]] <- mean(data[[ov]], na.rm=TRUE)
        nd_opt <- .doe_add_centerpt(fit, nd_opt)
        opt_y <- suppressWarnings(predict(fit, newdata=nd_opt))

        # Desirability score
        if (requireNamespace("desirability", quietly=TRUE))
            desir_score <- predict(d_fn, opt_y)

        # Results table
        result_df <- data.frame(
            Variable         = c(var_names, resp, gtxt("Desirability")),
            `Optimal Setting`= c(round(opt_x,4), round(opt_y,4), round(desir_score,4)),
            `Lower Bound`    = c(round(lowers,4),
                                  round(min(data[[resp]],na.rm=TRUE),4), NA),
            `Upper Bound`    = c(round(uppers,4),
                                  round(max(data[[resp]],na.rm=TRUE),4), NA),
            check.names=FALSE
        )
        # BUGFIX/enhancement: when goal=="target", show what value was
        # actually targeted -- previously this was computed silently
        # (always mean(response)) with no indication anywhere in the output
        # of what the optimizer aimed for, making a "target" run
        # indistinguishable from any other goal in the report.
        opt_caption <- if (identical(goal, "target"))
            gtxtf("Optimal settings found by L-BFGS-B with multi-start grid search. Target value: %s",
                  format(target_resolved, digits=6))
        else
            gtxt("Optimal settings found by L-BFGS-B with multi-start grid search")
        spsspivottable.Display(result_df,
            title  = gtxtf("Response Optimizer — Goal: %s", toupper(goal)),
            caption= opt_caption,
            templateName="DOEOPTIMIZE", outline=gtxt("Optimizer"))

        # Sensitivity table: response at ±10% of optimal
        sens_rows <- list()
        for (i in seq_along(var_names)) {
            for (delta in c(-0.1, 0.1)) {
                x_try   <- opt_x
                x_try[i]<- pmax(lowers[i], pmin(uppers[i], opt_x[i]*(1+delta)))
                nd_try  <- as.data.frame(t(x_try)); names(nd_try) <- var_names
                nd_try  <- .doe_add_centerpt(fit, nd_try)
                y_try   <- tryCatch(suppressWarnings(predict(fit, newdata=nd_try)), error=function(e) NA)
                sens_rows[[length(sens_rows)+1]] <- data.frame(
                    Variable = var_names[i],
                    Change   = sprintf("%+.0f%%", delta*100),
                    `Factor Value` = round(x_try[i],4),
                    `Response`     = round(y_try,4),
                    `Delta Y`      = round(y_try - opt_y, 4),
                    check.names=FALSE
                )
            }
        }
        sens_df <- do.call(rbind, sens_rows)
        spsspivottable.Display(sens_df,
            title  = gtxt("Sensitivity Analysis (±10% from optimum)"),
            caption= gtxt("Shows response change when each factor is varied ±10% from its optimal value"),
            templateName="DOESENSITIVITY", outline=gtxt("Sensitivity"))

        # ── Output subdialog: Optimization Detail Table (CI/PI at optimum) ───
        # New uncertainty-quantification table -- not shown elsewhere. Gives a
        # 95% confidence interval (uncertainty in the mean response) and a 95%
        # prediction interval (uncertainty for one future observation) at the
        # optimal settings, per Montgomery's "Design and Analysis of Experiments".
        if (optdetailtable) {
            detail_info <- tryCatch({
                ci <- predict(fit, newdata=nd_opt, interval="confidence", level=0.95)
                pi <- predict(fit, newdata=nd_opt, interval="prediction", level=0.95)
                data.frame(
                    Metric = c(gtxt("Predicted Response"),
                               gtxt("95% CI Lower"), gtxt("95% CI Upper"),
                               gtxt("95% PI Lower"), gtxt("95% PI Upper")),
                    Value  = round(c(opt_y, ci[1,"lwr"], ci[1,"upr"], pi[1,"lwr"], pi[1,"upr"]), 4)
                )
            }, error=function(e) { warns$warn(gtxtf("Optimization detail table could not be computed: %s", e$message), dostop=FALSE); NULL })
            if (!is.null(detail_info)) {
                spsspivottable.Display(detail_info,
                    title  = gtxt("Optimization Detail: Confidence & Prediction Intervals"),
                    caption= gtxt("CI reflects uncertainty in the mean response at the optimal settings; PI reflects uncertainty for a single future observation at those settings."),
                    templateName="DOEOPTDETAIL", outline=gtxt("Optimizer"))
            }

            # ── Top alternative solutions (additive) ─────────────────────────
            # Same idea as the multi-response optimizer's equivalent table:
            # lists the next-best DISTINCT points from the multi-start grid
            # search already performed above (grid_vals, only created when the
            # 'desirability' package is available -- guarded by exists() so
            # this is silently skipped otherwise, never erroring). Read-only;
            # never changes which point is reported as the optimum.
            if (exists("grid_vals", inherits=FALSE)) {
                ord_alt <- tryCatch(order(grid_vals, na.last=NA), error=function(e) integer(0))
                n_alt   <- min(5, length(ord_alt))
                if (n_alt > 0) {
                    alt_rows <- lapply(ord_alt[seq_len(n_alt)], function(r) {
                        x_r  <- grid[r, ]
                        nd_r <- as.data.frame(t(x_r)); names(nd_r) <- var_names
                        for (ov in setdiff(setdiff(names(data), resp), c(meta_cols, var_names)))
                            nd_r[[ov]] <- mean(data[[ov]], na.rm=TRUE)
                        nd_r <- .doe_add_centerpt(fit, nd_r)
                        y_r <- tryCatch(suppressWarnings(predict(fit, newdata=nd_r)), error=function(e) NA)
                        c(x_r, y_r)
                    })
                    alt_mat <- round(do.call(rbind, alt_rows), 4)
                    alt_df  <- as.data.frame(alt_mat)
                    names(alt_df) <- c(var_names, resp)
                    alt_df <- cbind(Rank=seq_len(n_alt), alt_df)
                    spsspivottable.Display(alt_df,
                        title  = gtxt("Response Optimizer — Top Alternative Solutions"),
                        caption= gtxt("Next-best distinct settings from the same multi-start grid search used to seed the optimizer, for comparison against the optimal settings above."),
                        templateName="DOEOPTALT", outline=gtxt("Optimizer"))
                }
            }
        }

        # Optimization plot -- deferred into a closure instead of drawn here,
        # so all of this function's TABLES (Results, Sensitivity,
        # Optimization Detail, Top Alternative Solutions) finish first, and
        # the actual chart is only rendered later when the caller (Run())
        # invokes the returned closure, during a dedicated "plots" phase that
        # runs after every table in the whole output (not just this
        # function's own tables). Still gated only on !is.null(opt)
        # (unchanged from before, since plot_fn is only ever assigned inside
        # this same if-block), not on optdetailtable.
        plot_fn <- function() {
            # Same device/cwd defensive guard used everywhere else a plot is
            # rendered -- this closure now runs standalone, potentially well
            # after this function's own StartProcedure/EndProcedure pair has
            # already closed, so it needs its own guard and its own
            # procedure block rather than relying on the one above.
            while (!is.null(dev.list())) tryCatch(dev.off(), error=function(e) NULL)
            .doe_old_wd2 <- getwd()
            on.exit(tryCatch(setwd(.doe_old_wd2), error=function(e) NULL), add=TRUE)
            .doe_wd2 <- .doe_find_writable_dir()
            if (!is.null(.doe_wd2)) tryCatch(setwd(.doe_wd2), error=function(e) NULL)
            StartProcedure(gtxt("Response Optimizer"), "STATSDOEOPT")
            tryCatch({
                .doe_submit_plot({
                    if (length(var_names)==1) {
                        old <- par(mar=c(4,4,3,2)); on.exit(par(old))
                        xs <- seq(lowers[1], uppers[1], length.out=200)
                        ys <- sapply(xs, function(xi) {
                            nd <- as.data.frame(setNames(list(xi), var_names[1]))
                            nd <- .doe_add_centerpt(fit, nd)
                            suppressWarnings(predict(fit, newdata=nd))
                        })
                        plot(xs, ys, type="l", col="#2980B9", lwd=2,
                             xlab=var_names[1], ylab=resp,
                             main=gtxtf("Response Optimization (%s)", toupper(goal)))
                        abline(v=opt_x[1], lty=2, col="#C0392B", lwd=2)
                        points(opt_x[1], opt_y, pch=19, col="#C0392B", cex=1.5)
                        text(opt_x[1], opt_y,
                             sprintf("  Opt: x=%.3f\n  y=%.3f", opt_x[1], opt_y),
                             adj=0, cex=0.8, col="#C0392B")
                    } else {
                        # Dynamic left margin based on variable name length
                        max_name_len <- max(nchar(var_names))
                        left_margin  <- max(8, max_name_len * 0.7)
                        old <- par(mar=c(5, left_margin, 3, 4)); on.exit(par(old))
                        d_norm <- (opt_x - lowers) / (uppers - lowers)
                        # xlim widened from 1.15 to 1.35 to leave real headroom for
                        # the value labels (previously they were placed right at the
                        # edge of a 1.15 axis limit and could clip/overlap).
                        bp <- barplot(d_norm, names.arg=var_names, horiz=TRUE, las=1,
                                xlim=c(0,1.35), col="#2980B9",
                                xlab=gtxt("Normalized Optimal Setting (0=Low, 1=High)"),
                                main=gtxtf("Optimal Settings (%s)", toupper(goal)))
                        abline(v=0.5, lty=2, col="gray")
                        # Label y-positions now come from barplot()'s own returned bar
                        # centers (bp) instead of a hardcoded seq(0.7, by=1.2, ...)
                        # guess -- the guess silently assumed barplot()'s default bar
                        # spacing/width, so labels could drift away from their bars
                        # whenever that default didn't match the assumption. Using
                        # bp directly guarantees each label sits on its own bar.
                        text(d_norm + 0.03, bp[,1],
                             round(opt_x,3), cex=0.8, col="#C0392B", adj=0)
                    }
                })
            }, error=function(e) NULL)
            spsspkg.EndProcedure()
        }
    }

    spsspkg.EndProcedure()
    invisible(plot_fn)
}

# ════════════════════════════════════════════════════════════════════════════
# MULTI-RESPONSE SIMULTANEOUS OPTIMIZATION (COMPOSITE DESIRABILITY)
# ════════════════════════════════════════════════════════════════════════════
# A true sibling of do_optimization() above, which is left completely
# untouched: when exactly one response variable is supplied, that existing
# function still runs unchanged. This new function is only ever invoked when
# RESPONSEVAR contains 2+ space-separated names, fitting the conventional Response
# Optimizer behavior -- simultaneous optimization across all responses via a
# single composite desirability score, the geometric mean of each response's
# individual desirability (Derringer & Suich, 1980, "Simultaneous
# Optimization of Several Response Variables", J. Qual. Technol. 12(4)):
#     D = (d_1 * d_2 * ... * d_k) ^ (1/k)
# Uses the same already-soft-dependent 'desirability' package as the existing
# single-response optimizer (dMax/dMin/dTarget) and gracefully degrades to a
# hand-rolled linear 0-1 normalization if that package is unavailable, so the
# feature works either way.
do_multi_response_optimization <- function(fits, data, variables, spec, goals, warns, responsevars,
                                             optdetailtable=FALSE, contourplot=FALSE, targets=NULL) {
    meta_cols <- c("Reps","Proportion","StdOrder","RunOrder","Block","CenterPt","PtType")
    var_names <- intersect(as.character(variables), setdiff(names(data), c(meta_cols, responsevars)))
    if (length(var_names) == 0) return(NULL)

    # Per-response resolved numeric target, parallel to `goals`, built BEFORE
    # the `valid` filter below (so its positions still line up 1:1 with the
    # original, unfiltered `responsevars`/`goals`), then subset by the same
    # `valid` mask alongside them. `targets` may be NULL or shorter than
    # `responsevars` -- recycled/padded defensively so indexing by position
    # can never go out of bounds. NA entries (unspecified) are resolved to
    # the historical mean(range()) fallback below, at each of the two places
    # "target" is used, so a caller that never passes `targets` is completely
    # unaffected.
    targets_all <- if (is.null(targets)) rep(NA_real_, length(responsevars))
        else suppressWarnings(as.numeric(rep(targets, length.out=length(responsevars))))

    valid <- !sapply(fits, is.null)
    if (!any(valid)) {
        warns$warn(gtxt("Multi-response optimizer: no response variable could be fitted"), dostop=FALSE)
        return(NULL)
    }
    fits         <- fits[valid]
    responsevars <- responsevars[valid]
    goals        <- goals[valid]
    targets_resolved <- targets_all[valid]

    # BUGFIX: same device/cwd defensive guard as do_optimization() above --
    # see that function's comment. Without it, this function's own
    # spssRGraphics.Submit() call could leave bad state for whatever plots
    # next.
    while (!is.null(dev.list())) tryCatch(dev.off(), error=function(e) NULL)
    .doe_old_wd <- getwd()
    on.exit(tryCatch(setwd(.doe_old_wd), error=function(e) NULL), add=TRUE)
    .doe_wd <- .doe_find_writable_dir()
    if (!is.null(.doe_wd)) tryCatch(setwd(.doe_wd), error=function(e) NULL)

    StartProcedure(gtxt("Multi-Response Optimizer (Composite Desirability)"), "STATSDOEMOPT")

    lowers <- sapply(var_names, function(v) {
        i <- which(as.character(spec$var) == v)
        if (length(i)) as.numeric(spec$lows[i[1]]) else min(data[[v]], na.rm=TRUE)
    })
    uppers <- sapply(var_names, function(v) {
        i <- which(as.character(spec$var) == v)
        if (length(i)) as.numeric(spec$highs[i[1]]) else max(data[[v]], na.rm=TRUE)
    })
    starts <- (lowers + uppers) / 2

    has_desir <- requireNamespace("desirability", quietly=TRUE)
    if (has_desir) library(desirability)

    d_fns <- list()
    if (has_desir) {
        for (k in seq_along(responsevars)) {
            rv <- responsevars[k]; g <- goals[k]
            yr <- range(data[[rv]], na.rm=TRUE)
            # Per-response resolved target: caller-supplied value at position
            # k when finite, else the historical mean(yr) fallback -- and the
            # dTarget() bounds are widened to include it if it falls outside
            # the observed range, same reasoning as do_optimization() above.
            tgt_k <- if (is.finite(targets_resolved[k])) targets_resolved[k] else mean(yr)
            dt_lo_k <- min(yr[1], tgt_k); dt_hi_k <- max(yr[2], tgt_k)
            d_fns[[rv]] <- switch(g,
                "maximize" = dMax(yr[1], yr[2]),
                "minimize" = dMin(yr[1], yr[2]),
                "target"   = dTarget(dt_lo_k, tgt_k, dt_hi_k),
                dMax(yr[1], yr[2]))
        }
    }

    predict_all <- function(x) {
        nd <- as.data.frame(t(x)); names(nd) <- var_names
        sapply(seq_along(fits), function(k) {
            fit        <- fits[[k]]
            other_vars <- setdiff(all.vars(formula(fit)), c(responsevars[k], var_names))
            for (ov in other_vars) nd[[ov]] <- mean(data[[ov]], na.rm=TRUE)
            tryCatch(suppressWarnings(predict(fit, newdata=nd)), error=function(e) NA)
        })
    }

    # Per-response desirability for a vector of already-predicted values.
    desirability_of <- function(preds) {
        if (has_desir) {
            sapply(seq_along(preds), function(k) predict(d_fns[[responsevars[k]]], preds[k]))
        } else {
            sapply(seq_along(preds), function(k) {
                rv <- responsevars[k]; g <- goals[k]
                yr <- range(data[[rv]], na.rm=TRUE)
                span <- diff(yr)
                if (!is.finite(span) || span <= 0) return(1)
                norm <- (preds[k] - yr[1]) / span
                if (g == "minimize") norm <- 1 - norm
                if (g == "target") {
                    tgt_k <- if (is.finite(targets_resolved[k])) targets_resolved[k] else mean(yr)
                    norm  <- 1 - abs(preds[k] - tgt_k) / (span / 2)
                }
                min(max(norm, 1e-6), 1)
            })
        }
    }

    composite_obj <- function(x) {
        preds <- predict_all(x)
        if (any(is.na(preds))) return(1e10)
        ds <- pmax(desirability_of(preds), 1e-6)
        -exp(mean(log(ds)))   # maximize composite == minimize -composite
    }

    gn   <- 4
    grid <- as.matrix(expand.grid(lapply(seq_along(var_names), function(i)
        seq(lowers[i], uppers[i], length.out=gn))))
    bv <- Inf; bs <- starts
    # grid_vals keeps every grid point's composite objective value (purely
    # additive bookkeeping -- the selection logic for bv/bs below, and
    # therefore the optimizer's behavior, is completely unchanged from
    # before). Used afterwards only to list the next-best distinct grid
    # points alongside the gradient-refined optimum, the same multi-start
    # search the optimizer already performs is just made visible.
    grid_vals <- rep(NA_real_, nrow(grid))
    for (r in seq_len(nrow(grid))) {
        v <- composite_obj(grid[r,])
        grid_vals[r] <- v
        if (!is.na(v) && v < bv) { bv <- v; bs <- grid[r,] }
    }

    opt <- tryCatch(
        optim(par=bs, fn=composite_obj, method="L-BFGS-B",
              lower=lowers, upper=uppers, control=list(maxit=1000)),
        error=function(e) { warns$warn(gtxtf("Multi-response optimizer failed: %s", e$message), dostop=FALSE); NULL }
    )
    if (is.null(opt)) { spsspkg.EndProcedure(); return(NULL) }

    opt_x       <- opt$par
    preds       <- predict_all(opt_x)
    per_d       <- desirability_of(preds)
    composite_d <- -opt$value

    factors_df <- data.frame(
        Variable          = var_names,
        `Optimal Setting` = round(opt_x, 4),
        `Lower Bound`      = round(lowers, 4),
        `Upper Bound`      = round(uppers, 4),
        check.names=FALSE
    )
    spsspivottable.Display(factors_df,
        title  = gtxt("Multi-Response Optimizer — Optimal Factor Settings"),
        caption= gtxt("Joint optimum found by maximizing composite desirability (geometric mean of per-response desirabilities) via L-BFGS-B with multi-start grid search."),
        templateName="DOEMOPTFACTORS", outline=gtxt("Multi-Response Optimizer"))

    # BUGFIX/enhancement: show what value each "target"-goal response was
    # actually targeted at -- previously silent (always mean(range())), with
    # no way to tell from the output. Blank for maximize/minimize responses.
    target_col <- sapply(seq_along(responsevars), function(k) {
        if (!identical(goals[k], "target")) return(NA_character_)
        yr    <- range(data[[responsevars[k]]], na.rm=TRUE)
        tgt_k <- if (is.finite(targets_resolved[k])) targets_resolved[k] else mean(yr)
        format(round(tgt_k, 4))
    })
    resp_df <- data.frame(
        Response          = c(responsevars, gtxt("Composite")),
        Goal               = c(goals, ""),
        Target             = c(target_col, NA),
        `Predicted Value`  = c(round(preds, 4), NA),
        Desirability       = c(if (has_desir) round(per_d, 4) else rep(gtxt("N/A (approx.)"), length(per_d)),
                                round(composite_d, 4)),
        check.names=FALSE
    )
    spsspivottable.Display(resp_df,
        title  = gtxt("Multi-Response Optimizer — Per-Response Desirability"),
        caption= if (has_desir)
                     gtxt("Composite Desirability = geometric mean of individual response desirabilities (Derringer & Suich, 1980).")
                 else
                     gtxt("The 'desirability' package is not installed; desirability scores were approximated with a linear 0-1 normalization. Install 'desirability' for the standard dMax/dMin/dTarget functions."),
        templateName="DOEMOPTRESP", outline=gtxt("Multi-Response Optimizer"))

    if (optdetailtable) {
        nd_opt <- as.data.frame(t(opt_x)); names(nd_opt) <- var_names
        detail_rows <- lapply(seq_along(fits), function(k) {
            other_vars <- setdiff(all.vars(formula(fits[[k]])), c(responsevars[k], var_names))
            for (ov in other_vars) nd_opt[[ov]] <- mean(data[[ov]], na.rm=TRUE)
            ci <- tryCatch(predict(fits[[k]], newdata=nd_opt, interval="confidence", level=0.95), error=function(e) NULL)
            pi <- tryCatch(predict(fits[[k]], newdata=nd_opt, interval="prediction", level=0.95), error=function(e) NULL)
            if (is.null(ci) || is.null(pi)) return(NULL)
            data.frame(Response=responsevars[k],
                       `95% CI Lower`=round(ci[1,"lwr"],4), `95% CI Upper`=round(ci[1,"upr"],4),
                       `95% PI Lower`=round(pi[1,"lwr"],4), `95% PI Upper`=round(pi[1,"upr"],4),
                       check.names=FALSE)
        })
        detail_rows <- detail_rows[!sapply(detail_rows, is.null)]
        if (length(detail_rows) > 0) {
            detail_df <- do.call(rbind, detail_rows)
            spsspivottable.Display(detail_df,
                title  = gtxt("Multi-Response Optimizer — Confidence & Prediction Intervals at Optimum"),
                caption= gtxt("CI reflects uncertainty in the mean response; PI reflects uncertainty for a single future observation, both at the joint optimal settings."),
                templateName="DOEMOPTDETAIL", outline=gtxt("Multi-Response Optimizer"))
        }

        # ── Top alternative solutions (additive) ────────────────────────────
        # Surfaces the next-best DISTINCT points from the multi-start grid
        # search already performed above (grid_vals), each with its predicted
        # response(s) and composite desirability -- lets the user compare
        # near-optimal alternative factor settings against the single
        # gradient-refined optimum already reported in the Optimal Factor
        # Settings table. Read-only summary of values already computed; never
        # changes which point is selected/reported as the optimum, and is
        # gated on the same existing OPTDETAILTABLE checkbox (off by default),
        # so it never appears unless the user already opted into extra detail.
        ord <- tryCatch(order(grid_vals, na.last=NA), error=function(e) integer(0))
        n_alt <- min(5, length(ord))
        if (n_alt > 0) {
            alt_rows <- lapply(ord[seq_len(n_alt)], function(r) {
                x_r     <- grid[r, ]
                preds_r <- predict_all(x_r)
                c(x_r, preds_r, -grid_vals[r])
            })
            alt_mat  <- round(do.call(rbind, alt_rows), 4)
            alt_df   <- as.data.frame(alt_mat)
            names(alt_df) <- c(var_names,
                                sapply(responsevars, function(rv) gtxtf("Predicted %s", rv)),
                                gtxt("Composite Desirability"))
            alt_df <- cbind(Rank=seq_len(n_alt), alt_df)
            spsspivottable.Display(alt_df,
                title  = gtxt("Multi-Response Optimizer — Top Alternative Solutions"),
                caption= gtxt("Next-best distinct settings from the same multi-start grid search used to seed the optimizer, for comparison against the joint optimum above."),
                templateName="DOEMOPTALT", outline=gtxt("Multi-Response Optimizer"))
        }
    }

    # Overlaid contour plot: one response per color, joint optimum marked --
    # only meaningful (and only attempted) with exactly 2 numeric factors.
    # Deferred into a closure (same pattern as do_optimization()'s plot_fn)
    # instead of drawn here, so the caller can render it later during a
    # dedicated "plots" phase that runs after every table in the whole
    # output, not just this function's own tables. Still gated only on
    # contourplot/var count (unchanged from before), not on optdetailtable.
    plot_fn <- NULL
    if (contourplot && length(var_names) == 2) {
        plot_fn <- function() {
            while (!is.null(dev.list())) tryCatch(dev.off(), error=function(e) NULL)
            .doe_old_wd2 <- getwd()
            on.exit(tryCatch(setwd(.doe_old_wd2), error=function(e) NULL), add=TRUE)
            .doe_wd2 <- .doe_find_writable_dir()
            if (!is.null(.doe_wd2)) tryCatch(setwd(.doe_wd2), error=function(e) NULL)
            StartProcedure(gtxt("Multi-Response Optimizer (Composite Desirability)"), "STATSDOEMOPT")
            tryCatch({
                .doe_submit_plot({
                    x1s  <- seq(lowers[1], uppers[1], length.out=40)
                    x2s  <- seq(lowers[2], uppers[2], length.out=40)
                    cols <- c("#2980B9","#C0392B","#27AE60","#8E44AD","#D35400","#16A085")
                    old  <- par(mar=c(4,4,3,4)); on.exit(par(old))
                    plot(NULL, xlim=range(x1s), ylim=range(x2s), xlab=var_names[1], ylab=var_names[2],
                         main=gtxt("Overlaid Contour Plot — Multi-Response Optimum"))
                    for (k in seq_along(fits)) {
                        pm <- outer(x1s, x2s, function(a, b) {
                            nd <- data.frame(a, b); names(nd) <- var_names
                            other_vars <- setdiff(all.vars(formula(fits[[k]])), c(responsevars[k], var_names))
                            for (ov in other_vars) nd[[ov]] <- mean(data[[ov]], na.rm=TRUE)
                            suppressWarnings(predict(fits[[k]], newdata=nd))
                        })
                        contour(x1s, x2s, pm, add=TRUE, col=cols[((k-1) %% length(cols)) + 1], nlevels=6, lwd=1.5)
                    }
                    points(opt_x[1], opt_x[2], pch=19, col="black", cex=1.6)
                    text(opt_x[1], opt_x[2], gtxt("  Joint optimum"), adj=0, cex=0.8)
                    legend("topright", legend=responsevars, col=cols[((seq_along(fits)-1) %% length(cols))+1],
                           lwd=2, cex=0.8, bg="white")
                })
            }, error=function(e) NULL)
            spsspkg.EndProcedure()
        }
    }

    spsspkg.EndProcedure()
    invisible(plot_fn)
}

# ════════════════════════════════════════════════════════════════════════════
# POWER & SAMPLE SIZE CALCULATOR (PLANNING TOOL, INDEPENDENT OF DESIGN GEN)
# ════════════════════════════════════════════════════════════════════════════
# Standard noncentral-t power calculation for detecting a single effect from
# an unreplicated 2-level design (Montgomery, "Design and Analysis of
# Experiments", and the same model the conventional "Power and Sample Size for
# Factorial Designs" is built on): for n runs, Var(effect estimate) =
# 4*sigma^2/n, so SE = 2*sigma/sqrt(n); expressing the effect in
# standard-deviation units d = effect/sigma cancels sigma entirely (so no
# numeric sigma estimate is required at the planning stage, matching standard
# practice), giving noncentrality ncp = d*sqrt(n)/2 against a t distribution
# with df = n-2 (one degree of freedom for the intercept, one for the effect
# itself -- the minimal model that can estimate one effect at all). Uses only
# base R's pt()/qt() noncentral-t support; no new package dependency.
compute_power_value <- function(n, d, alpha) {
    df <- n - 2
    if (df < 1 || n <= 0) return(NA_real_)
    ncp   <- d * sqrt(n) / 2
    tcrit <- qt(1 - alpha / 2, df)
    1 - pt(tcrit, df, ncp=ncp) + pt(-tcrit, df, ncp=ncp)
}

compute_required_runs <- function(d, target_power, alpha, maxn=512) {
    for (n in seq(4, maxn, by=2)) {
        p <- compute_power_value(n, d, alpha)
        if (!is.na(p) && p >= target_power) return(n)
    }
    NA_integer_
}

do_power_analysis <- function(effectsizes, runs, alpha, target_power, warns) {
    if (length(effectsizes) == 0) effectsizes <- c(0.5, 1, 1.5, 2)
    if (length(runs) == 0)        runs        <- c(4, 8, 16, 32)

    # BUGFIX: same device/cwd defensive guard as do_optimization() above --
    # this is specifically the "POWER planning tool's own spssRGraphics.
    # Submit() chart" already referenced by create_all_plots()'s own comment
    # as a known way to leave bad state for a later call; it just never got
    # the matching guard itself until now.
    while (!is.null(dev.list())) tryCatch(dev.off(), error=function(e) NULL)
    .doe_old_wd <- getwd()
    on.exit(tryCatch(setwd(.doe_old_wd), error=function(e) NULL), add=TRUE)
    .doe_wd <- .doe_find_writable_dir()
    if (!is.null(.doe_wd)) tryCatch(setwd(.doe_wd), error=function(e) NULL)

    StartProcedure(gtxt("Power and Sample Size for Factorial Designs"), "STATSDOEPOWER")

    # ── Power table: rows = candidate run counts, columns = effect sizes ────
    power_mat <- t(sapply(runs, function(n) sapply(effectsizes, function(d) compute_power_value(n, d, alpha))))
    power_df  <- as.data.frame(round(power_mat, 4))
    names(power_df) <- sprintf("d=%.3g", effectsizes)
    power_df <- cbind(Runs=runs, power_df)
    spsspivottable.Display(power_df,
        title  = gtxt("Power Table (Effect Size in Standard-Deviation Units)"),
        caption= gtxtf("Power to detect an effect of size d = effect/sigma at alpha=%.3g, assuming an unreplicated 2-level design with df=Runs-2.", alpha),
        templateName="DOEPOWERTABLE", outline=gtxt("Power Analysis"))

    # ── Required-runs table: minimum n achieving target power, per effect size ─
    req_runs <- sapply(effectsizes, function(d) compute_required_runs(d, target_power, alpha))
    req_df <- data.frame(`Effect Size (d)`=effectsizes, `Required Runs`=req_runs, check.names=FALSE)
    spsspivottable.Display(req_df,
        title  = gtxtf("Required Run Size for %.0f%% Power", target_power*100),
        caption= gtxtf("Minimum number of runs to achieve %.0f%% power at alpha=%.3g for each effect size. Blank/NA means not achievable within %d runs.",
                       target_power*100, alpha, 512),
        templateName="DOEPOWERRUNS", outline=gtxt("Power Analysis"))

    # ── Power Curve chart: power vs. effect size, one line per candidate n ──
    tryCatch({
        .doe_submit_plot({
            cols  <- c("#2980B9","#C0392B","#27AE60","#8E44AD","#D35400","#16A085")
            dseq  <- seq(max(0.05, min(effectsizes)*0.5), max(effectsizes)*1.5, length.out=100)
            old   <- par(mar=c(4,4,3,2)); on.exit(par(old))
            plot(NULL, xlim=range(dseq), ylim=c(0,1), xlab=gtxt("Effect Size (standard-deviation units)"),
                 ylab=gtxt("Power"), main=gtxt("Power Curve"))
            abline(h=target_power, lty=2, col="gray40")
            for (i in seq_along(runs)) {
                pw <- sapply(dseq, function(d) compute_power_value(runs[i], d, alpha))
                lines(dseq, pw, col=cols[((i-1) %% length(cols)) + 1], lwd=2)
            }
            legend("bottomright", legend=gtxtf("n=%d", runs),
                   col=cols[((seq_along(runs)-1) %% length(cols)) + 1], lwd=2, cex=0.8, bg="white")
        })
    }, error=function(e) NULL)

    spsspkg.EndProcedure()
}

# ════════════════════════════════════════════════════════════════════════════
# SINGLE-FILE INTERACTIVE CHART EMBEDDING HELPERS
# ════════════════════════════════════════════════════════════════════════════
# htmlwidgets::saveWidget()/htmltools::save_html() copy a whole tree of JS/CSS
# dependency files (plotly, jquery, htmlwidgets, crosstalk, ...) into a sibling
# "*_files" folder next to the saved .html - NOT a single self-contained file.
# To produce a genuine single-file report (matching the reference Gage R&R
# extension's pattern) while still using plotly::plot_ly()/layout()/config()
# R-side (no chart-building rewrite needed), each finished plotly object is
# converted here into raw "<div>+<script>Plotly.newPlot(...)</script>" HTML
# using the resolved trace/layout/config JSON - the page is then assembled as
# one literal HTML string and written with plain writeLines(), loading
# Plotly.js itself from a single CDN <script> tag. No "_files" folder is ever
# created. If 'jsonlite' is unavailable, returns the plotly widget object
# unchanged so the caller can fall back to the older multi-file save path.
plotly_embed <- function(p, divid, height="460px") {
    if (!has_jsonlite) return(p)
    b <- tryCatch(suppressWarnings(plotly::plotly_build(p)$x), error=function(e) NULL)
    if (is.null(b)) return(p)
    data_json   <- tryCatch(jsonlite::toJSON(b$data, auto_unbox=TRUE, null="null", digits=NA, force=TRUE),
                             error=function(e) NULL)
    if (is.null(data_json)) return(p)
    layout_json <- jsonlite::toJSON(if (!is.null(b$layout)) b$layout else list(),
                                     auto_unbox=TRUE, null="null", digits=NA, force=TRUE)
    config_json <- jsonlite::toJSON(if (!is.null(b$config)) b$config else list(),
                                     auto_unbox=TRUE, null="null", digits=NA, force=TRUE)
    # NOTE: the "Flip Orientation" modebar button (a click handler) cannot be
    # expressed as JSON, so it is added on the JS side via
    # window.__doeTransposeChart() (defined once in the page header by
    # save_single_file_report()). It swaps the x/y data and axis properties
    # (a true transpose — e.g. horizontal bars <-> vertical bars), not a
    # left-right mirror. responsive:true is also forced here so the chart
    # (and its title) always fits the actual rendered container width
    # instead of a stale/oversized snapshot width — additive fix, never makes
    # rendering worse than the previous fixed-size behavior. Both changes are
    # real Plotly data/layout state, so the modebar's "Download plot as png"
    # button captures them exactly as displayed.
    htmltools::HTML(sprintf(
        '<div id="%s" style="width:100%%;height:%s;"></div><script>(function(){
  var __cfg = %s;
  __cfg.responsive = true;
  __cfg.modeBarButtonsToAdd = (__cfg.modeBarButtonsToAdd || []).concat([
    { name: "flipOrientation", title: "Flip Orientation (e.g. horizontal/vertical bars)",
      icon: Plotly.Icons.autoscale,
      click: function(gd){ if (window.__doeTransposeChart) window.__doeTransposeChart(gd); } }
  ]);
  Plotly.newPlot("%s", %s, %s, __cfg).then(function(gd){
    // When two or more charts share a flex row (Cube Plot panels, the
    // Response Surface/Contour pair, Pareto/Half-Normal, residual
    // diagnostics, etc.), each chart\'s <script> runs the instant the parser
    // reaches it -- i.e. while only the chart divs already parsed so far
    // exist in the flex container, before later sibling divs in the same
    // row have been inserted. Plotly.newPlot() measures the container\'s
    // width at that exact moment, so an earlier chart in the row can be
    // sized as if it had the *whole* row to itself; once the remaining
    // siblings appear and the flex layout reflows everyone down to their
    // true (smaller) share, that earlier chart\'s already-drawn canvas does
    // not auto-shrink to match -- it stays rendered at the old, larger
    // size and visually overflows/clips inside its now-narrower box. This
    // is the root cause of the first chart in a row looking cramped or cut
    // off while later charts in the same row render at full, correct size.
    // Fix: force an explicit resize once the whole page (so the whole flex
    // row) has finished loading, plus on every window resize -- this
    // re-measures the *final* container width and redraws every chart
    // (2D and 3D/WebGL alike) to fit it exactly.
    function __doeResize(){ try { Plotly.Plots.resize(gd); } catch(e){} }
    if (document.readyState === "complete") { __doeResize(); }
    else { window.addEventListener("load", __doeResize); }
    window.addEventListener("resize", __doeResize);
  });
})();</script>',
        divid, height, config_json, divid, data_json, layout_json))
}

# Renders a heatmap (confounding / partial-aliasing matrices) with three
# improvements over a plain plotly::plot_ly(type="heatmap"): (1) the numeric
# value is printed inside every cell via texttemplate, so the matrix is
# readable without hovering; (2) the chart is drawn at a content-sized
# width/height (scaled to the number of rows/cols, capped so it never
# balloons to the full ~1100px report column the way a "responsive" chart
# would for what is usually a small matrix) instead of stretching to fill
# the page; (3) a small color-theme <select> above the chart calls
# Plotly.restyle() on change -- since that mutates the live trace (not a
# CSS overlay), the modebar's "Download plot as png" button captures
# whichever theme is currently selected. Falls back to a plain widget (no
# value labels/theme picker, but never worse than the previous behavior) if
# 'jsonlite' is unavailable, mirroring plotly_embed()'s own fallback.
heatmap_embed <- function(cm, divid, title, filename="heatmap", zmid=0, default_colorscale="RdBu") {
    if (!has_jsonlite) {
        return(plotly::plot_ly(z=cm, x=colnames(cm), y=rownames(cm), type="heatmap",
                   colorscale=default_colorscale, zmid=zmid))
    }
    nr <- nrow(cm); nc <- ncol(cm)
    w  <- min(820, max(360, 95*nc + 170))
    h  <- min(720, max(340, 70*nr + 160))
    txt_matrix <- matrix(sprintf("%.3f", cm), nrow=nr, ncol=nc)
    z_json     <- jsonlite::toJSON(unname(cm), na="null", digits=NA, force=TRUE)
    text_json  <- jsonlite::toJSON(unname(txt_matrix), na="null", digits=NA, force=TRUE)
    x_json     <- jsonlite::toJSON(colnames(cm), force=TRUE)
    y_json     <- jsonlite::toJSON(rownames(cm), force=TRUE)
    title_json <- jsonlite::toJSON(title, auto_unbox=TRUE, force=TRUE)
    zmid_js    <- if (is.null(zmid) || is.na(zmid)) "null" else as.character(zmid)
    themes     <- c("RdBu","Viridis","Portland","Picnic","Earth","Greys","Jet","YlOrRd")
    theme_opts <- paste(sprintf('<option value="%s"%s>%s</option>', themes,
                                 ifelse(themes==default_colorscale, " selected", ""), themes),
                         collapse="")
    htmltools::HTML(sprintf('
<div style="max-width:%dpx;margin:0 auto;">
  <div style="text-align:right;margin-bottom:4px;">
    <label style="font-size:12px;color:#555;margin-right:6px;">%s</label>
    <select id="%s_theme" style="font-size:12px;padding:2px 4px;">%s</select>
  </div>
  <div id="%s" style="width:%dpx;height:%dpx;margin:0 auto;"></div>
</div>
<script>(function(){
  var z = %s, x = %s, y = %s, txt = %s;
  var data = [{ z: z, x: x, y: y, text: txt, texttemplate: "%%{text}", type: "heatmap",
                colorscale: "%s", zmid: %s,
                hovertemplate: "%%{y} ~ %%{x}: %%{z:.3f}<extra></extra>" }];
  var layout = { title: { text: %s, font: {size:14}, x:0.02, xanchor:"left" },
                 margin: {t:50,l:90,r:20,b:90} };
  var config = { displaylogo:false, responsive:false,
                  toImageButtonOptions: {format:"png", filename:"%s", scale:2} };
  Plotly.newPlot("%s", data, layout, config);
  var __sel = document.getElementById("%s_theme");
  if (__sel) __sel.addEventListener("change", function(){
    Plotly.restyle("%s", {colorscale: this.value}, [0]);
  });
})();</script>',
        w+40, gtxt("Color theme:"), divid, theme_opts, divid, w, h,
        z_json, x_json, y_json, text_json, default_colorscale, zmid_js,
        title_json, filename, divid, divid, divid))
}

# Renders a tagList of (now widget-free, since plotly_embed() was used) HTML
# tags into one literal HTML document string with Plotly.js loaded from CDN,
# and writes it as a single file (no companion "_files" directory). Falls
# back to the old htmltools::save_html()/libdir approach (multi-file) only if
# 'jsonlite' is missing, so behavior is never worse than before.
save_single_file_report <- function(sections, out_path, title, warns) {
    title <- paste(as.character(title), collapse=" "); out_path <- as.character(out_path)[1]
    if (has_jsonlite) {
        body_html <- tryCatch(htmltools::doRenderTags(htmltools::tagList(sections)),
                               error=function(e) NULL)
        if (!is.null(body_html)) {
            full_html <- paste0(
                '<!DOCTYPE html><html><head><meta charset="utf-8"><title>', title, '</title>',
                '<script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>',
                '<style>body > div { max-width: 1100px; margin: 0 auto; }</style>',
                '<script>window.__doeTransposeChart = function(gd){',
                # Multi-panel (faceted/subplot) charts have a second axis set
                # (xaxis2/yaxis2, ...) -- safely transposing those would also
                # need to relocate each panel's domain, so this is skipped
                # there (no-op) rather than risking a broken layout. Single-
                # axis charts (Pareto, Half-Normal, Residuals, Q-Q, Histogram,
                # 2-factor Cube) are fully supported.
                'if (gd.layout && gd.layout.xaxis2) return;',
                'var newData = (gd.data || []).map(function(tr){',
                '  var nt = {}; for (var k in tr) nt[k] = tr[k];',
                '  if (nt.type === "scatter3d" || nt.type === "surface" || nt.type === "contour") return tr;',
                '  if (nt.type === "histogram") {',
                '    if (nt.x !== undefined && nt.y === undefined) { nt.y = nt.x; delete nt.x; }',
                '    else if (nt.y !== undefined && nt.x === undefined) { nt.x = nt.y; delete nt.y; }',
                '    return nt;',
                '  }',
                '  var tx = nt.x, ty = nt.y; nt.x = ty; nt.y = tx;',
                '  if (nt.type === "bar") nt.orientation = (nt.orientation === "h") ? "v" : "h";',
                '  return nt;',
                '});',
                'var xa = gd.layout.xaxis || {}, ya = gd.layout.yaxis || {};',
                'var newXaxis = {}; for (var k1 in ya) newXaxis[k1] = ya[k1]; newXaxis.domain = xa.domain;',
                'var newYaxis = {}; for (var k2 in xa) newYaxis[k2] = xa[k2]; newYaxis.domain = ya.domain;',
                'var newLayout = {}; for (var k3 in gd.layout) newLayout[k3] = gd.layout[k3];',
                'newLayout.xaxis = newXaxis; newLayout.yaxis = newYaxis;',
                'Plotly.react(gd, newData, newLayout); };</script></head>',
                '<body style="margin:0;font-family:\'Segoe UI\',Helvetica,Arial,sans-serif;background:#fff;">',
                body_html, '</body></html>')
            full_html <- paste(full_html, collapse="\n")   # always one string, whatever the parts
            ok <- tryCatch({ con <- file(out_path, open="w", encoding="UTF-8")
                             tryCatch(writeLines(full_html, con, useBytes=TRUE), finally=close(con)); TRUE },
                            error=function(e) { warns$warn(gtxtf("Could not save interactive HTML report: %s", e$message), dostop=FALSE); FALSE })
            if (isTRUE(ok)) .doe_open_html(out_path)
            return(ok)
        }
    }
    # Fallback: old multi-file save (only reached if jsonlite or tag rendering unavailable)
    ok <- tryCatch({
        htmltools::save_html(htmltools::tagList(sections), file=out_path,
            libdir=paste0(tools::file_path_sans_ext(basename(out_path)), "_files"))
        TRUE
    }, error=function(e) {
        warns$warn(gtxtf("Could not save interactive HTML report: %s", e$message), dostop=FALSE)
        FALSE
    })
    if (isTRUE(ok)) .doe_open_html(out_path)
    ok
}

# Displays the "report saved" confirmation as a proper Output Viewer item
# (spsspkg.TextBlock) rather than letting it surface only as plain text in
# the syntax/console output. Mirrors the proven pattern from the reference
# Gage R&R extension. If a procedure is already open (design-generation
# phase calls this from inside displayresults(), before that procedure is
# closed), the TextBlock is added directly; otherwise a short-lived
# dedicated procedure is opened just to host it.
notify_html_saved <- function(msg, title, omsid, open_new=TRUE) {
    if (open_new) {
        ok <- tryCatch({ StartProcedure(title, omsid); TRUE }, error=function(e) FALSE)
        if (ok) {
            tryCatch(spsspkg.TextBlock(title, msg), error=function(e) NULL)
            tryCatch(spsspkg.EndProcedure(), error=function(e) NULL)
        } else {
            tryCatch(spsspkg.TextBlock(title, msg), error=function(e) message(msg))
        }
    } else {
        tryCatch(spsspkg.TextBlock(title, msg), error=function(e) message(msg))
    }
}

# ════════════════════════════════════════════════════════════════════════════
# INTERACTIVE HTML REPORT (plotly / htmlwidgets) — ADDITIVE, STATE-OF-THE-ART
# ════════════════════════════════════════════════════════════════════════════
# Writes a standalone, self-contained interactive HTML file to htmlpath, in
# ADDITION to the static spssRGraphics charts already shown in the Viewer
# (the Viewer cannot render true interactive widgets, so this is a separate
# file the user opens in a browser). Includes: interactive Pareto / Half-
# Normal plot of effects, residual diagnostics, a rotatable 3D response
# surface, an interactive contour plot, and an optimizer summary panel.
export_html_report <- function(fit, data, variables, designtype, warns,
                                responsevar=NULL, htmlpath=NULL,
                                do_main=TRUE, do_inter=TRUE, do_contour=TRUE,
                                do_resid=TRUE, do_pareto=TRUE, do_cube_html=TRUE,
                                opt_goal=NULL, spec=NULL,
                                do_curvature=FALSE, varselect=FALSE,
                                varselmethod="stepwise", stepdir="both") {

    if (!has_plotly || !has_htmlwidgets) {
        warns$warn(gtxt("Interactive HTML report skipped: requires the 'plotly' and 'htmlwidgets' R packages. Install with: install.packages(c('plotly','htmlwidgets'))"), dostop=FALSE)
        return(invisible(NULL))
    }
    has_htmltools <- requireNamespace("htmltools", quietly=TRUE)
    if (!has_htmltools) {
        warns$warn(gtxt("Interactive HTML report skipped: requires the 'htmltools' R package (normally installed automatically with htmlwidgets)."), dostop=FALSE)
        return(invisible(NULL))
    }
    if (is.null(htmlpath) || !nzchar(htmlpath)) {
        warns$warn(gtxt("Interactive HTML report skipped: no output path supplied (HTMLPATH)."), dostop=FALSE)
        return(invisible(NULL))
    }

    suppressMessages(suppressWarnings(library(plotly)))
    suppressMessages(suppressWarnings(library(htmlwidgets)))
    suppressMessages(suppressWarnings(library(htmltools)))

    resp      <- if (!is.null(responsevar) && responsevar %in% names(data)) responsevar else tail(names(data), 1)
    meta_cols <- c("Reps","Proportion","StdOrder","RunOrder","Block","CenterPt","PtType")
    var_names <- intersect(as.character(variables), setdiff(names(data), c(meta_cols, resp)))
    if (!(resp %in% names(data)) || length(var_names) == 0) {
        warns$warn(gtxt("Interactive HTML report skipped: response or factor variables not found."), dostop=FALSE)
        return(invisible(NULL))
    }

    y        <- data[[resp]]
    sections <- list()

    # ── Header ────────────────────────────────────────────────────────────
    sections[[length(sections)+1]] <- htmltools::tags$div(
        style="font-family:'Segoe UI',Helvetica,Arial,sans-serif;padding:24px 32px;background:linear-gradient(135deg,#2C3E50,#34495E);color:#fff;",
        htmltools::tags$h1(style="margin:0 0 4px 0;font-size:28px;", gtxt("Design of Experiments — Interactive Report")),
        htmltools::tags$p(style="margin:0;opacity:0.85;font-size:14px;",
            sprintf("%s: %s  |  %s: %s  |  %s",
                    gtxt("Design Type"), toupper(designtype),
                    gtxt("Response"), resp,
                    format(Sys.time(), "%Y-%m-%d %H:%M")))
    )

    sm <- tryCatch(summary(fit), error=function(e) NULL)

    # Shared helper for Plotly chart titles: a smaller, left-anchored font
    # keeps long titles from being clipped by the SVG canvas bounds in the
    # narrower side-by-side chart panels (a real Plotly limitation -- SVG
    # text outside the canvas viewBox is clipped, not wrapped). Also drops
    # redundant "(interactive)"-style suffixes since each chart already has
    # a visible section heading above it.
    ptitle <- function(txt) list(text=txt, font=list(size=14), x=0.02, xanchor="left")

    # ── Interactive Main Effects Plot ─────────────────────────────────────
    # Same level-binning and grand-mean reference line as the static
    # ggplot2/base-R Main Effects Plot (create_external_plots()), so the
    # HTML report matches the SPSS Viewer output exactly.
    if (do_main && length(var_names) > 0) {
        tryCatch({
            gm      <- mean(y, na.rm=TRUE)
            nv      <- length(var_names)
            ncol_sp <- min(3, nv)
            nrow_sp <- ceiling(nv / ncol_sp)
            me_plots <- lapply(var_names, function(vn) {
                xvar <- data[[vn]]
                xcat <- if (length(unique(xvar)) <= 5) as.factor(xvar) else cut(xvar, breaks=3)
                ms   <- tapply(y, xcat, mean, na.rm=TRUE)
                lv   <- names(ms); mv <- as.numeric(ms)
                pp <- plotly::plot_ly(x=lv, y=mv, type="scatter", mode="lines+markers",
                    line=list(color="#3498DB", width=3),
                    marker=list(color="white", line=list(color="#3498DB", width=2), size=10),
                    showlegend=FALSE, name=vn,
                    hovertemplate=paste0(vn, ": %{x}<br>", gtxt("Mean"), " ", resp, ": %{y:.4f}<extra></extra>"))
                pp <- plotly::add_trace(pp, x=lv, y=rep(gm, length(lv)), type="scatter", mode="lines",
                    line=list(color="#95A5A6", dash="dash", width=1.5), showlegend=FALSE,
                    hoverinfo="skip")
                plotly::layout(pp, xaxis=list(title=vn), yaxis=list(title=""))
            })
            p_main <- plotly::subplot(me_plots, nrows=nrow_sp, margin=0.06, titleX=TRUE, titleY=TRUE)
            p_main <- plotly::layout(p_main, title=ptitle(gtxt("Main Effects Plot")),
                                      showlegend=FALSE)
            p_main <- plotly::config(p_main, displaylogo=FALSE,
                toImageButtonOptions=list(format="png", filename="main_effects", scale=2))

            sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                htmltools::tags$h2(style="color:#2C3E50;", gtxt("Main Effects Plot")),
                htmltools::tags$p(style="color:#555;font-size:13px;",
                    gtxt("Dashed line marks the grand mean response. Levels are binned into up to 5 groups for continuous factors.")),
                plotly_embed(p_main, "chart_main_effects", height="420px"))
        }, error=function(e) warns$warn(gtxtf("Interactive main effects plot: %s", e$message), dostop=FALSE))
    }

    # ── Interactive Interaction Plot ──────────────────────────────────────
    # Same pairwise level-binning/aggregation as the static
    # ggplot2/base-R Interaction Plot (create_external_plots()).
    if (do_inter && length(var_names) >= 2) {
        tryCatch({
            ipairs   <- combn(var_names, 2, simplify=FALSE)
            np       <- length(ipairs)
            ncol_sp2 <- min(3, np)
            nrow_sp2 <- ceiling(np / ncol_sp2)
            palette  <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00",
                          "#A65628","#F781BF","#999999")
            inter_plots <- lapply(ipairs, function(pr) {
                v1 <- pr[1]; v2 <- pr[2]
                x1 <- data[[v1]]; x2 <- data[[v2]]; yy <- data[[resp]]
                x1cat <- if (length(unique(x1)) <= 5) as.factor(x1) else cut(x1, breaks=3)
                x2cat <- if (length(unique(x2)) <= 5) as.factor(x2) else cut(x2, breaks=3)
                agg   <- aggregate(yy ~ x1cat + x2cat, FUN=mean, na.rm=TRUE)
                names(agg) <- c("Level1","Level2","Mean")
                lv2 <- sort(unique(as.character(agg$Level2)))
                pp  <- plotly::plot_ly()
                for (i in seq_along(lv2)) {
                    sub <- agg[as.character(agg$Level2)==lv2[i], , drop=FALSE]
                    sub <- sub[order(as.character(sub$Level1)), ]
                    pp  <- plotly::add_trace(pp, x=as.character(sub$Level1), y=sub$Mean,
                        type="scatter", mode="lines+markers",
                        # Prefix with the pair so the combined legend (Plotly has no
                        # separate per-subplot legend) doesn't show ambiguous repeated
                        # labels like "stg=30" for unrelated panels sharing that color index.
                        name=paste0(v1, " x ", v2, ": ", v2, "=", lv2[i]),
                        line=list(color=palette[((i-1) %% length(palette))+1], width=3),
                        marker=list(size=9, line=list(color="white", width=1)),
                        hovertemplate=paste0(v1, ": %{x}<br>", gtxt("Mean"), " ", resp,
                                              ": %{y:.4f}<extra>", v2, " = ", lv2[i], "</extra>"))
                }
                # Label each panel with the variable pair it shows (e.g. "qwe x dff"),
                # matching the per-panel titles in the static ggplot2/base-R Interaction
                # Plot. The interactive version has no per-subplot title slot, so this
                # is folded into the x-axis label, which subplot(titleX=TRUE) keeps
                # attached to its own panel.
                plotly::layout(pp, xaxis=list(title=paste0(v1, " x ", v2)), yaxis=list(title=""))
            })
            p_inter <- plotly::subplot(inter_plots, nrows=nrow_sp2, margin=0.07,
                                        titleX=TRUE, titleY=FALSE, shareY=FALSE)
            p_inter <- plotly::layout(p_inter, title=ptitle(gtxt("Interaction Plot")))
            p_inter <- plotly::config(p_inter, displaylogo=FALSE,
                toImageButtonOptions=list(format="png", filename="interaction_plot", scale=2))

            sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                htmltools::tags$h2(style="color:#2C3E50;", gtxt("Interaction Plot")),
                htmltools::tags$p(style="color:#555;font-size:13px;",
                    gtxt("Each panel: mean response by level of the first-named factor, with one line per level of the second.")),
                plotly_embed(p_inter, "chart_interactions", height=paste0(220*nrow_sp2, "px")))
        }, error=function(e) warns$warn(gtxtf("Interactive interaction plot: %s", e$message), dostop=FALSE))
    }

    # ── Interactive Cube Plot ───────────────────────────────────────────────
    # Same corner-fitted-mean formula, and same >3-factor faceting strategy
    # ("small multiples", one cube per level of a manageable extra factor),
    # as the static base-R Cube Plot above -- kept identical so the HTML
    # report and the SPSS Viewer output agree.
    if (do_cube_html && length(var_names) >= 2) {
        tryCatch({
            nlev_of   <- function(v) length(unique(data[[v]]))
            facet_var <- NULL
            if (length(var_names) > 3) {
                cand <- var_names[sapply(var_names, function(v) { nl <- nlev_of(v); nl>=2 && nl<=6 })]
                if (length(cand) > 0) {
                    nls       <- sapply(cand, nlev_of)
                    facet_var <- cand[which.min(nls)]
                }
            }
            vn        <- if (!is.null(facet_var)) setdiff(var_names, facet_var) else var_names
            vn        <- vn[seq_len(min(3, length(vn)))]
            held_vars <- setdiff(var_names, c(vn, facet_var))

            facet_levels <- if (!is.null(facet_var)) sort(unique(data[[facet_var]])) else NA
            n_facets     <- if (!is.null(facet_var)) length(facet_levels) else 1

            cube_widgets <- list()
            for (fi in seq_len(n_facets)) {
                corners <- as.data.frame(expand.grid(lapply(vn, function(v)
                    c(min(data[[v]], na.rm=TRUE), max(data[[v]], na.rm=TRUE)))))
                names(corners) <- vn
                if (!is.null(facet_var)) corners[[facet_var]] <- facet_levels[fi]
                for (ov in held_vars) corners[[ov]] <- mean(data[[ov]], na.rm=TRUE)
                corners   <- .doe_add_centerpt(fit, corners)
                pred_vals <- suppressWarnings(predict(fit, newdata=corners))

                panel_title <- if (!is.null(facet_var))
                    gtxtf("%s = %s", facet_var, format(facet_levels[fi], digits=4))
                else gtxt("Cube Plot (Fitted Means)")

                if (length(vn) == 2) {
                    p_cube <- plotly::plot_ly(x=corners[[vn[1]]], y=corners[[vn[2]]],
                        type="scatter", mode="markers+text",
                        text=sprintf("%.3f", pred_vals), textposition="top center",
                        marker=list(size=14, color="#2980B9"),
                        hovertemplate=paste0(vn[1], ": %{x}<br>", vn[2], ": %{y}<br>",
                                              gtxt("Fitted"), ": %{text}<extra></extra>"))
                    # No Plotly-rendered title here -- it's now a plain HTML heading
                    # above the chart div (see panel_html below), so it never shares
                    # screen space with this chart's own modebar.
                    p_cube <- plotly::layout(p_cube,
                        margin=list(t=20),
                        xaxis=list(title=vn[1]), yaxis=list(title=vn[2]))
                } else {
                    coords <- list(c(0,0,0),c(1,0,0),c(1,1,0),c(0,1,0),
                                    c(0,0,1),c(1,0,1),c(1,1,1),c(0,1,1))
                    edges  <- list(c(1,2),c(2,3),c(3,4),c(4,1),
                                    c(5,6),c(6,7),c(7,8),c(8,5),
                                    c(1,5),c(2,6),c(3,7),c(4,8))
                    xs <- sapply(coords, `[`, 1); ys <- sapply(coords, `[`, 2); zs <- sapply(coords, `[`, 3)
                    n8 <- min(8, length(pred_vals))
                    p_cube <- plotly::plot_ly()
                    for (e in edges) {
                        p_cube <- plotly::add_trace(p_cube, type="scatter3d", mode="lines",
                            x=xs[e], y=ys[e], z=zs[e],
                            line=list(color="gray", width=3), showlegend=FALSE, hoverinfo="skip")
                    }
                    # Push each corner's label away from the cube's center in BOTH the
                    # vertical (top/bottom, by z) and horizontal (left/right, by x)
                    # screen directions, not just top/bottom -- with only a top/bottom
                    # split, the 4 corners sharing the same face still print their
                    # labels stacked at (almost) the same horizontal position and
                    # collide. Combining z and x gives each of the 8 labels its own
                    # quadrant to sit in.
                    vpos   <- ifelse(zs[1:n8] > 0.5, "top", "bottom")
                    hpos   <- ifelse(xs[1:n8] > 0.5, "right", "left")
                    txtpos <- paste(vpos, hpos)
                    p_cube <- plotly::add_trace(p_cube, type="scatter3d", mode="markers+text",
                        x=xs[1:n8], y=ys[1:n8], z=zs[1:n8],
                        text=sprintf("%.3f", pred_vals[1:n8]), textposition=txtpos,
                        textfont=list(size=10),
                        marker=list(size=5, color="#2980B9"), showlegend=FALSE,
                        hovertemplate=paste0(gtxt("Fitted"), ": %{text}<extra></extra>"))
                    # No Plotly title (see panel_html below). A balanced camera (similar
                    # magnitude on all 3 eye axes) keeps the cube looking like a cube
                    # rather than a flattened sheared box -- a flatter z compresses the
                    # top/bottom faces together on screen and makes same-face corners'
                    # labels crowd each other even with the top/bottom/left/right split.
                    p_cube <- plotly::layout(p_cube,
                        margin=list(t=20),
                        scene=list(xaxis=list(title=vn[1], showticklabels=FALSE),
                                   yaxis=list(title=vn[2], showticklabels=FALSE),
                                   zaxis=list(title=vn[3], showticklabels=FALSE),
                                   aspectmode="cube",
                                   camera=list(eye=list(x=1.6, y=1.6, z=1.3))))
                }
                p_cube <- plotly::config(p_cube, displaylogo=FALSE,
                    # Trim the 3D modebar down to the essentials (zoom/pan/orbit/reset/
                    # download). The full default 3D modebar has enough buttons that in
                    # a narrow side-by-side facet panel it can run wide enough to crowd
                    # against the neighboring panel -- fewer buttons keeps it
                    # comfortably inside this panel's own width.
                    modeBarButtonsToRemove=if (length(vn)==3)
                        list("tableRotation", "resetCameraLastSave3d", "hoverClosest3d") else list(),
                    toImageButtonOptions=list(format="png",
                        filename=paste0("cube_plot", if (n_facets>1) paste0("_", fi) else ""), scale=2))

                # The facet label (e.g. "qwe = 10") is now a plain HTML heading,
                # not a Plotly title -- this is the only way to guarantee it never
                # visually competes with this chart's own modebar, since Plotly
                # always reserves the modebar's screen position independently of
                # whatever the title says.
                cube_widgets[[fi]] <- htmltools::tags$div(style="flex:1;min-width:460px;",
                    htmltools::tags$div(style="font-weight:600;color:#2C3E50;margin-bottom:6px;", panel_title),
                    plotly_embed(p_cube, paste0("chart_cube", fi),
                                 height=if (length(vn)==3) "480px" else "400px"))
            }

            held_txt <- if (length(held_vars) > 0)
                paste0(" ", gtxtf("Remaining factor(s) held at their mean: %s.", paste(held_vars, collapse=", ")))
            else ""
            caption_txt <- if (!is.null(facet_var))
                paste0(gtxtf("Fitted mean response at the corners of the design space for %s (min/max of each); one small cube per level of %s.",
                             paste(vn, collapse=", "), facet_var), held_txt)
            else
                paste0(gtxt("Fitted mean response at the corners of the design space (min/max of each factor)."), held_txt)

            sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                htmltools::tags$h2(style="color:#2C3E50;", gtxt("Cube Plot")),
                htmltools::tags$p(style="color:#555;font-size:13px;", caption_txt),
                htmltools::tags$div(style="display:flex;flex-wrap:wrap;gap:32px;", cube_widgets))
        }, error=function(e) warns$warn(gtxtf("Interactive cube plot: %s", e$message), dostop=FALSE))
    }

    # ── Interactive Pareto / Half-Normal of Effects ──────────────────────
    # As with the static ggplot2 Pareto chart: a genuine "Pareto Chart of
    # Effects" is bars of |standardized effect| (or |effect| under Lenth's
    # PSE when saturated) sorted descending, with ONE significance threshold
    # line -- no cumulative-percentage curve. Effect magnitudes are signed
    # regression coefficients, not counts of a whole, so "cumulative % of
    # total effect" is not a meaningful statistic and was removed. The
    # t-test-vs-Lenth's-PSE method choice now also matches the base-R and
    # static-ggplot2 versions (governed by residual df), instead of always
    # using Lenth's PSE regardless of whether real residual df are available.
    if (do_pareto && !is.null(sm)) {
        tryCatch({
            coefs <- sm$coefficients
            coefs <- coefs[rownames(coefs) != "(Intercept)", , drop=FALSE]
            eff   <- coefs[,1]

            df_resid  <- sm$df[2]
            use_lenth <- df_resid < 1
            # lp is used for the Half-Normal plot's ME/SME reference lines
            # below regardless of use_lenth -- that plot is always valid via
            # Lenth's PSE, with or without residual df (see base-R comment
            # above the Half-Normal section). The Pareto bars/threshold,
            # however, switch to a genuine t-test once residual df exist.
            lp <- lenth_pse(eff)

            if (use_lenth) {
                if (!is.null(lp)) { aeff_all <- abs(eff); tcrit <- lp$ME; ylab_txt <- gtxt("Effect Magnitude") }
            } else {
                aeff_all <- abs(coefs[,3]); tcrit <- qt(0.975, df_resid); ylab_txt <- gtxt("Standardized Effect (|t|)")
            }

            if (((use_lenth && !is.null(lp)) || !use_lenth) && !is.null(lp)) {
                ord    <- order(aeff_all, decreasing=TRUE)
                aeff   <- aeff_all[ord]
                enames <- names(eff)[ord]
                sig    <- aeff > tcrit
                # Letter-code the terms (see effect_term_codes()) so long
                # interaction names stay short on the bar axis; the mapping
                # back to variable names is shown as an inset annotation
                # instead of a separate legend box.
                tc        <- effect_term_codes(enames, var_names)
                bar_codes <- tc$codes
                factor_key<- paste(paste0(tc$legend$Letter, " = ", tc$legend$Name), collapse="<br>")

                # Threshold label text -- built once and shown in TWO places:
                # (1) as a second title line (guaranteed visible regardless of
                #     where the dashed line happens to fall on the x-axis --
                #     an in-plot annotation pinned at x=tcrit can land too
                #     close to the y-axis / get crowded by automargin when
                #     tcrit is small, which made it effectively invisible),
                # and (2) still next to the dashed line itself for reference.
                thresh_label <- if (use_lenth) gtxtf("ME (95%%) = %.3f", tcrit) else gtxtf("t* = %.2f (alpha = 0.05)", tcrit)

                p_pareto <- plotly::plot_ly()
                p_pareto <- plotly::add_trace(p_pareto,
                    y = factor(bar_codes, levels=bar_codes), x = aeff, type="bar", orientation="h",
                    name=gtxt("Effect"),
                    marker=list(color=ifelse(sig, "#C0392B", "#5DADE2")),
                    text=enames, hovertemplate=paste0("%{text}<br>", gtxt("Effect"), ": %{x:.4f}<extra></extra>"))
                # BUGFIX: Plotly's default autorange sizes the x-axis from the
                # bar trace alone -- a `shapes` line (the dashed threshold
                # below) isn't included in that calculation, so whenever
                # tcrit exceeded every bar (routine with Lenth's PSE method on
                # small/noisy designs) the line itself could render outside
                # the visible plot area even though its label (in the title
                # above) was still shown. Setting an explicit range that
                # always includes tcrit guarantees the line itself is visible
                # too, matching the conventional chart. Purely a display change;
                # normal charts (tcrit already inside the bars' range) get
                # essentially the same range Plotly's autorange would have
                # picked anyway.
                pareto_axis_max <- max(c(aeff, tcrit), na.rm=TRUE) * 1.1
                p_pareto <- plotly::layout(p_pareto,
                    title=ptitle(gtxtf("Pareto Chart of Effects (response is %s%s)<br><sup>%s</sup>", resp,
                        if (use_lenth) gtxt(", Lenth's PSE method") else gtxt(", alpha = 0.05"), thresh_label)),
                    yaxis=list(title="", automargin=TRUE, autorange="reversed"),
                    xaxis=list(title=ylab_txt, automargin=TRUE, range=c(0, pareto_axis_max)),
                    shapes=list(list(type="line", yref="paper", y0=0, y1=1, x0=tcrit, x1=tcrit,
                                      line=list(color="#C0392B", dash="dash", width=2))),
                    annotations=list(
                        list(yref="paper", y=1, x=tcrit, xshift=4, showarrow=FALSE, xanchor="left",
                             text=paste0("<b>", thresh_label, "</b>"),
                             font=list(color="#C0392B", size=13)),
                        list(xref="paper", yref="paper", x=1.02, y=1, xanchor="left", yanchor="top",
                             showarrow=FALSE, align="left", text=paste0("<b>", gtxt("Factor"), "</b><br>", factor_key),
                             font=list(color="#2C3E50", size=11))),
                    legend=list(orientation="h", x=0.5, xanchor="center", y=1.28),
                    margin=list(t=110, r=130))
                p_pareto <- plotly::config(p_pareto, displaylogo=FALSE,
                    toImageButtonOptions=list(format="png", filename="pareto_effects", scale=2))

                ord2    <- order(abs(eff))
                aeff2   <- abs(eff)[ord2]
                enames2 <- names(eff)[ord2]
                m       <- length(aeff2)
                hq      <- qnorm(0.5 + 0.5*(seq_len(m)-0.5)/m)

                p_halfnorm <- plotly::plot_ly(
                    x=hq, y=aeff2, type="scatter", mode="markers+text",
                    text=enames2, textposition="top right",
                    marker=list(size=10, color="#3498DB", line=list(color="white", width=1)),
                    hovertemplate=paste0("%{text}<br>", gtxt("|Effect|"), ": %{y:.4f}<extra></extra>"))
                p_halfnorm <- plotly::layout(p_halfnorm,
                    title=ptitle(gtxt("Half-Normal Plot of Effects")),
                    xaxis=list(title=gtxt("Half-Normal Quantile")),
                    yaxis=list(title=gtxt("|Effect|")),
                    shapes=list(
                        list(type="line", x0=min(hq), x1=max(hq), y0=lp$ME,  y1=lp$ME,
                             line=list(color="#E67E22", dash="dash", width=2)),
                        list(type="line", x0=min(hq), x1=max(hq), y0=lp$SME, y1=lp$SME,
                             line=list(color="#E74C3C", dash="dot",  width=2))))
                p_halfnorm <- plotly::config(p_halfnorm, displaylogo=FALSE,
                    toImageButtonOptions=list(format="png", filename="half_normal_effects", scale=2))

                sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                    htmltools::tags$h2(style="color:#2C3E50;", gtxt("Effects Analysis")),
                    htmltools::tags$div(style="display:flex;flex-wrap:wrap;gap:16px;",
                        htmltools::tags$div(style="flex:1;min-width:460px;", plotly_embed(p_pareto, "chart_pareto")),
                        htmltools::tags$div(style="flex:1;min-width:460px;", plotly_embed(p_halfnorm, "chart_halfnorm"))))
            } else {
                sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                    htmltools::tags$h2(style="color:#2C3E50;", gtxt("Effects Analysis")),
                    htmltools::tags$p(style="color:#999;font-size:13px;",
                        gtxt("Skipped: fewer than 3 finite (non-collinear) effect estimates are available - Lenth's pseudo-standard-error method requires at least 3 effects. This typically happens with a rank-deficient/saturated model (e.g. CONSTANT=NO with a mixture constraint, or too few runs for the number of model terms). Add replicates or reduce the model to enable this section.")))
            }
        }, error=function(e) warns$warn(gtxtf("Interactive Pareto/Half-normal plot: %s", e$message), dostop=FALSE))
    }

    # ── Interactive Residual Diagnostics ─────────────────────────────────
    if (do_resid) {
        tryCatch({
            fitted_v <- fitted(fit)
            resid_v  <- residuals(fit)
            rstd_v   <- tryCatch(rstandard(fit), error=function(e) resid_v)

            # Same lowess(fitted, residuals) trend line as the base-R "Residuals
            # vs Fitted" panel (which calls lines(lowess(fit$fitted.values,
            # fit$residuals), ...)) -- previously omitted here, so the
            # interactive chart showed only the raw points without the trend
            # that the static plot has, which can hide a real fitted-vs-residual
            # pattern (non-constant variance, missed nonlinearity) that the
            # base-R version makes visible.
            lo_fr <- lowess(fitted_v, resid_v)
            p_fr <- plotly::plot_ly(x=fitted_v, y=resid_v, type="scatter", mode="markers",
                name=gtxt("Residuals"),
                marker=list(color="#2980B9", size=9, line=list(color="white", width=1)),
                hovertemplate=paste0(gtxt("Fitted"), ": %{x:.4f}<br>", gtxt("Residual"), ": %{y:.4f}<extra></extra>"))
            p_fr <- plotly::add_trace(p_fr, x=lo_fr$x, y=lo_fr$y, type="scatter", mode="lines",
                name=gtxt("Trend (lowess)"), line=list(color="#C0392B", width=2), showlegend=FALSE,
                hoverinfo="skip")
            p_fr <- plotly::layout(p_fr,
                margin=list(t=20),
                xaxis=list(title=gtxt("Fitted Value")), yaxis=list(title=gtxt("Residual")),
                shapes=list(list(type="line", x0=min(fitted_v), x1=max(fitted_v), y0=0, y1=0,
                                  line=list(color="red", dash="dash", width=1.5))))
            p_fr <- plotly::config(p_fr, displaylogo=FALSE,
                toImageButtonOptions=list(format="png", filename="residuals_vs_fitted", scale=2))

            qq   <- qqnorm(rstd_v, plot.it=FALSE)
            qord <- order(qq$x)
            qline_y <- range(qq$x) * sd(qq$y, na.rm=TRUE) + mean(qq$y, na.rm=TRUE)
            p_qq <- plotly::plot_ly(x=qq$x[qord], y=qq$y[qord], type="scatter", mode="markers",
                name=gtxt("Residuals"),
                marker=list(color="#27AE60", size=9, line=list(color="white", width=1)),
                hovertemplate=paste0(gtxt("Theoretical"), ": %{x:.3f}<br>", gtxt("Sample"), ": %{y:.3f}<extra></extra>"))
            p_qq <- plotly::add_trace(p_qq, x=range(qq$x), y=qline_y, type="scatter", mode="lines",
                name=gtxt("Reference"), line=list(color="#C0392B", dash="dash"), showlegend=FALSE)
            p_qq <- plotly::layout(p_qq, margin=list(t=20),
                xaxis=list(title=gtxt("Theoretical Quantile")), yaxis=list(title=gtxt("Standardized Residual")))
            p_qq <- plotly::config(p_qq, displaylogo=FALSE,
                toImageButtonOptions=list(format="png", filename="normal_qq_plot", scale=2))

            # ── Scale-Location -- present in the base-R 2x2 residual panel but
            # previously missing here entirely. Same sqrt(|standardized
            # residuals|) vs fitted formula and lowess trend as the base-R
            # "Scale-Location" panel, so both outputs agree on whether variance
            # looks constant across the fitted range.
            sres   <- sqrt(abs(rstd_v))
            lo_sl  <- lowess(fitted_v, sres)
            p_scale <- plotly::plot_ly(x=fitted_v, y=sres, type="scatter", mode="markers",
                name=gtxt("Residuals"),
                marker=list(color="#2980B9", size=9, line=list(color="white", width=1)),
                hovertemplate=paste0(gtxt("Fitted"), ": %{x:.4f}<br>", "%{y:.4f}<extra></extra>"))
            p_scale <- plotly::add_trace(p_scale, x=lo_sl$x, y=lo_sl$y, type="scatter", mode="lines",
                name=gtxt("Trend (lowess)"), line=list(color="#C0392B", width=2), showlegend=FALSE,
                hoverinfo="skip")
            p_scale <- plotly::layout(p_scale, margin=list(t=20),
                xaxis=list(title=gtxt("Fitted Value")), yaxis=list(title=gtxt("sqrt(|Std. Residual|)")))
            p_scale <- plotly::config(p_scale, displaylogo=FALSE,
                toImageButtonOptions=list(format="png", filename="scale_location", scale=2))

            # ── Residuals vs Run Order -- also present in the base-R 2x2 panel
            # (plot(seq_along(fit$residuals), fit$residuals, type="b", ...)) and
            # previously missing here. Flags run-to-run drift/trends over time
            # that "Residuals vs Fitted" alone cannot reveal.
            run_v <- seq_along(resid_v)
            p_run <- plotly::plot_ly(x=run_v, y=resid_v, type="scatter", mode="lines+markers",
                name=gtxt("Residuals"),
                marker=list(color="#2980B9", size=8, line=list(color="white", width=1)),
                line=list(color="#2980B9", width=1, dash="dot"),
                hovertemplate=paste0(gtxt("Run"), ": %{x}<br>", gtxt("Residual"), ": %{y:.4f}<extra></extra>"))
            p_run <- plotly::layout(p_run, margin=list(t=20),
                xaxis=list(title=gtxt("Run Order")), yaxis=list(title=gtxt("Residual")),
                shapes=list(list(type="line", x0=min(run_v), x1=max(run_v), y0=0, y1=0,
                                  line=list(color="red", dash="dash", width=1.5))))
            p_run <- plotly::config(p_run, displaylogo=FALSE,
                toImageButtonOptions=list(format="png", filename="residuals_vs_run_order", scale=2))

            p_hist <- plotly::plot_ly(x=resid_v, type="histogram",
                marker=list(color="#8E44AD", line=list(color="white", width=1)))
            p_hist <- plotly::layout(p_hist, margin=list(t=20),
                xaxis=list(title=gtxt("Residual")), yaxis=list(title=gtxt("Count")))
            p_hist <- plotly::config(p_hist, displaylogo=FALSE,
                toImageButtonOptions=list(format="png", filename="residual_histogram", scale=2))

            resid_panel <- function(title_txt, p, divid) htmltools::tags$div(style="flex:1;min-width:380px;",
                htmltools::tags$div(style="font-weight:600;color:#2C3E50;margin-bottom:6px;", title_txt),
                plotly_embed(p, divid))

            sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                htmltools::tags$h2(style="color:#2C3E50;", gtxt("Residual Diagnostics")),
                htmltools::tags$p(style="color:#555;font-size:13px;",
                    gtxt("Same four diagnostic views as the static Residual Plots in the SPSS Viewer (Residuals vs Fitted, Normal Q-Q, Scale-Location, Residuals vs Run Order), plus a residual histogram.")),
                htmltools::tags$div(style="display:flex;flex-wrap:wrap;gap:24px;",
                    resid_panel(gtxt("Residuals vs. Fitted"), p_fr, "chart_resid_fitted"),
                    resid_panel(gtxt("Normal Q-Q Plot of Std. Residuals"), p_qq, "chart_qq"),
                    resid_panel(gtxt("Scale-Location"), p_scale, "chart_scale_location"),
                    resid_panel(gtxt("Residuals vs Run Order"), p_run, "chart_resid_runorder"),
                    resid_panel(gtxt("Histogram of Residuals"), p_hist, "chart_resid_hist")))
        }, error=function(e) warns$warn(gtxtf("Interactive residual diagnostics: %s", e$message), dostop=FALSE))
    }

    # ── Rotatable 3D Response Surface + Interactive Contour ──────────────
    if (do_contour || do_main) {
        tryCatch({
            cont_vars <- var_names[sapply(var_names, function(v)
                is.numeric(data[[v]]) && length(unique(data[[v]])) >= 3)]
            if (length(cont_vars) >= 2) {
                v1 <- cont_vars[1]; v2 <- cont_vars[2]
                g  <- 40
                x1seq <- seq(min(data[[v1]], na.rm=TRUE), max(data[[v1]], na.rm=TRUE), length.out=g)
                x2seq <- seq(min(data[[v2]], na.rm=TRUE), max(data[[v2]], na.rm=TRUE), length.out=g)
                grid  <- expand.grid(x1seq, x2seq); names(grid) <- c(v1, v2)
                for (ov in setdiff(var_names, c(v1, v2)))
                    grid[[ov]] <- mean(data[[ov]], na.rm=TRUE)
                grid <- .doe_add_centerpt(fit, grid)
                grid$.pred <- tryCatch(suppressWarnings(predict(fit, newdata=grid)), error=function(e) NA)
                zmat <- matrix(grid$.pred, nrow=g, ncol=g)

                p_surf <- plotly::plot_ly(x=x1seq, y=x2seq, z=t(zmat), type="surface",
                    colorscale="Viridis",
                    contours=list(z=list(show=TRUE, usecolormap=TRUE,
                                          highlightcolor="#ff0000", project=list(z=TRUE))))
                # Titles are plain HTML headings below (not Plotly titles) so they
                # never compete on-screen with this chart's own modebar -- same
                # reasoning as the Cube Plot fix above.
                p_surf <- plotly::layout(p_surf,
                    margin=list(t=20),
                    scene=list(xaxis=list(title=v1), yaxis=list(title=v2), zaxis=list(title=resp)))
                p_surf <- plotly::config(p_surf, displaylogo=FALSE,
                    toImageButtonOptions=list(format="png", filename="response_surface_3d", scale=2))

                p_cont <- plotly::plot_ly(x=x1seq, y=x2seq, z=t(zmat), type="contour",
                    colorscale="Viridis", contours=list(showlabels=TRUE))
                p_cont <- plotly::layout(p_cont,
                    margin=list(t=20),
                    xaxis=list(title=v1), yaxis=list(title=v2))
                p_cont <- plotly::config(p_cont, displaylogo=FALSE,
                    toImageButtonOptions=list(format="png", filename="contour_plot", scale=2))

                sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                    htmltools::tags$h2(style="color:#2C3E50;", gtxt("Response Surface")),
                    htmltools::tags$p(style="color:#555;font-size:13px;",
                        gtxt("Drag to rotate the 3D surface. Other factors held at their mean.")),
                    htmltools::tags$div(style="display:flex;flex-wrap:wrap;gap:32px;",
                        htmltools::tags$div(style="flex:1;min-width:460px;",
                            htmltools::tags$div(style="font-weight:600;color:#2C3E50;margin-bottom:6px;",
                                gtxtf("3D Response Surface: %s vs %s, %s", v1, v2, resp)),
                            plotly_embed(p_surf, "chart_surface3d")),
                        htmltools::tags$div(style="flex:1;min-width:460px;",
                            htmltools::tags$div(style="font-weight:600;color:#2C3E50;margin-bottom:6px;",
                                gtxtf("Contour: %s vs %s", v1, v2)),
                            plotly_embed(p_cont, "chart_contour"))))
            } else {
                warns$warn(gtxt("Interactive 3D surface skipped: need at least 2 continuous numeric factors."), dostop=FALSE)
                sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                    htmltools::tags$h2(style="color:#2C3E50;", gtxt("Response Surface")),
                    htmltools::tags$p(style="color:#999;font-size:13px;",
                        gtxt("Skipped: need at least 2 numeric factors with 3 or more distinct values each. Categorical/2-level factors do not qualify for a continuous surface.")))
            }
        }, error=function(e) {
            warns$warn(gtxtf("Interactive 3D surface/contour: %s", e$message), dostop=FALSE)
            sections[[length(sections)+1]] <<- htmltools::tags$div(style="padding:16px 32px;",
                htmltools::tags$h2(style="color:#2C3E50;", gtxt("Response Surface")),
                htmltools::tags$p(style="color:#C0392B;font-size:13px;",
                    gtxtf("Skipped due to an error while building this section: %s", e$message)))
        })
    }

    # ── Curvature Check Plot (interactive) ────────────────────────────────
    # Interactive counterpart to the base-R Curvature Check Plot built in
    # create_all_plots() (Cube vs. Center Point average response -- see the
    # comment there for the full rationale). Same CenterPt-based logic and
    # the same skip conditions, rendered as a Plotly error-bar chart instead
    # of base graphics so it also appears in the HTML report, not just the
    # SPSS Viewer.
    if (do_curvature) {
        tryCatch({
            if (!("CenterPt" %in% names(data))) {
                warns$warn(gtxt("Interactive curvature plot skipped: no center-point runs found in the data (requires the optional center-points feature)."), dostop=FALSE)
            } else {
                cp_vals <- suppressWarnings(as.numeric(data$CenterPt))
                if (length(unique(cp_vals[!is.na(cp_vals)])) < 2) {
                    warns$warn(gtxt("Interactive curvature plot skipped: CenterPt column does not vary (no factorial/center contrast available)."), dostop=FALSE)
                } else {
                    y_all     <- suppressWarnings(as.numeric(data[[resp]]))
                    valid_row <- !is.na(cp_vals) & !is.na(y_all)
                    is_cube   <- valid_row & (cp_vals == 0)
                    is_ctr    <- valid_row & (cp_vals == 1)
                    if (sum(is_cube) >= 1 && sum(is_ctr) >= 1) {
                        cube_y <- y_all[is_cube]
                        ctr_y  <- y_all[is_ctr]
                        means  <- c(mean(cube_y), mean(ctr_y))
                        ses    <- c(if (length(cube_y) > 1) sd(cube_y)/sqrt(length(cube_y)) else 0,
                                    if (length(ctr_y)  > 1) sd(ctr_y) /sqrt(length(ctr_y))  else 0)
                        labs   <- c(gtxt("Factorial (Cube) Points"), gtxt("Center Points"))
                        p_curv <- plotly::plot_ly(x=labs, y=means, type="scatter", mode="lines+markers",
                            error_y=list(type="data", array=ses, color="#2980B9", thickness=1.5, width=6),
                            line=list(color="#2980B9", width=2),
                            marker=list(color="white", line=list(color="#2980B9", width=2), size=14),
                            text=sprintf("%.4f", means), textposition="top center",
                            hovertemplate=paste0("%{x}<br>", gtxt("Mean"), " ", resp, ": %{y:.4f}<extra></extra>"))
                        p_curv <- plotly::layout(p_curv, margin=list(t=20, b=50, l=60, r=20),
                            xaxis=list(title="", automargin=TRUE),
                            yaxis=list(title=gtxtf("Mean %s", resp), automargin=TRUE))
                        p_curv <- plotly::config(p_curv, displaylogo=FALSE,
                            toImageButtonOptions=list(format="png", filename="curvature_check", scale=2))

                        sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                            htmltools::tags$h2(style="color:#2C3E50;", gtxt("Curvature Check Plot")),
                            htmltools::tags$p(style="color:#555;font-size:13px;",
                                gtxtf("Compares the mean %s at the factorial (cube) points vs. the center points. A flat line indicates no curvature; a pronounced slope suggests the true surface bends within the design region.", resp)),
                            plotly_embed(p_curv, "chart_curvature", height="380px"))
                    } else {
                        warns$warn(gtxt("Interactive curvature plot skipped: need at least one factorial point and one center point with non-missing response values."), dostop=FALSE)
                    }
                }
            }
        }, error=function(e) warns$warn(gtxtf("Interactive curvature plot: %s", e$message), dostop=FALSE))
    }

    # ── Variable Selection Charts (interactive) ───────────────────────────
    # Interactive counterpart to create_varselect_plots(): recomputes the
    # same vsel object via compute_variable_selection() and renders
    # whichever chart(s) apply to the chosen method as Plotly charts --
    # Selection Criterion Trace for Stepwise; Predictive Fit Profile, Model
    # Parsimony Plot and Factor Inclusion Map for Best Subsets. This mirrors
    # the method-based gating already enforced by the dialog's per-chart
    # checkboxes (see the varselmethod ControlConditions on those checkboxes)
    # rather than exposing separate per-chart flags into this function.
    if (isTRUE(varselect)) {
        tryCatch({
            vsel_html <- compute_variable_selection(fit, varselmethod=varselmethod, stepdir=stepdir, warns=warns)
            if (!is.null(vsel_html) && vsel_html$type == "stepwise") {
                df <- vsel_html$df
                if (is.data.frame(df) && nrow(df) > 0) {
                    best_idx <- which.min(df$AIC)
                    p_trace <- plotly::plot_ly(x=df$Step, y=df$AIC, type="scatter", mode="lines+markers",
                        name=gtxt("AIC at each step"),
                        line=list(color="#2E86AB", width=2.5),
                        marker=list(color="#2E86AB", size=10),
                        text=df$Action,
                        hovertemplate=paste0(gtxt("Step"), " %{x}<br>AIC: %{y:.3f}<br>%{text}<extra></extra>"))
                    p_trace <- plotly::add_trace(p_trace, x=df$Step[best_idx], y=df$AIC[best_idx], inherit=FALSE,
                        type="scatter", mode="markers", name=gtxt("Optimal step"),
                        marker=list(color="#E74C3C", size=16, symbol="diamond"),
                        hovertemplate=paste0(gtxt("Optimal step"), "<br>AIC: %{y:.3f}<extra></extra>"))
                    p_trace <- plotly::layout(p_trace, margin=list(t=20, b=90, l=70, r=20),
                        xaxis=list(title=gtxt("Selection Step"), tickmode="array", tickvals=df$Step, ticktext=df$Action,
                                   tickangle=-30, automargin=TRUE),
                        yaxis=list(title=gtxt("AIC (lower = better)"), automargin=TRUE))
                    p_trace <- plotly::config(p_trace, displaylogo=FALSE,
                        toImageButtonOptions=list(format="png", filename="selection_criterion_trace", scale=2))

                    sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                        htmltools::tags$h2(style="color:#2C3E50;", gtxt("Selection Criterion Trace")),
                        htmltools::tags$p(style="color:#555;font-size:13px;",
                            gtxtf("Stepwise selection (%s direction). Lower AIC is better; the optimal step is marked in red. Final model: %s",
                                  stepdir, vsel_html$final_formula)),
                        plotly_embed(p_trace, "chart_selection_trace", height="420px"))
                }
            } else if (!is.null(vsel_html) && vsel_html$type == "bestsubsets") {
                df <- vsel_html$df
                if (is.data.frame(df) && nrow(df) > 0) {
                    sizes    <- df$Size
                    r2       <- df[["R2 (%)"]]
                    adjr2    <- df[["AdjR2 (%)"]]
                    cp       <- df$Cp
                    best_idx <- which.max(adjr2)

                    # Predictive Fit Profile
                    p_fit <- plotly::plot_ly(x=sizes, y=r2, type="scatter", mode="lines+markers",
                        name=gtxt("R-sq (%)"), line=list(color="#2E86AB", width=2.5),
                        marker=list(color="#2E86AB", size=10, symbol="circle"),
                        hovertemplate=paste0(gtxt("Size"), " %{x}<br>R-sq: %{y:.2f}%<extra></extra>"))
                    p_fit <- plotly::add_trace(p_fit, x=sizes, y=adjr2, type="scatter", mode="lines+markers",
                        name=gtxt("Adj. R-sq (%)"), line=list(color="#E67E22", width=2.5),
                        marker=list(color="#E67E22", size=10, symbol="triangle-up"),
                        hovertemplate=paste0(gtxt("Size"), " %{x}<br>Adj. R-sq: %{y:.2f}%<extra></extra>"))
                    p_fit <- plotly::layout(p_fit, margin=list(t=20, b=90, l=70, r=20),
                        xaxis=list(title=gtxt("Number of Predictors in Model"), tickmode="array", tickvals=sizes,
                                   automargin=TRUE),
                        yaxis=list(title=gtxt("Variance Explained (%)"), automargin=TRUE),
                        legend=list(orientation="h", x=0, y=-0.3),
                        shapes=list(list(type="line", x0=sizes[best_idx], x1=sizes[best_idx],
                                          y0=0, y1=1, yref="paper",
                                          line=list(color="#E74C3C", width=1.8, dash="dash"))))
                    p_fit <- plotly::config(p_fit, displaylogo=FALSE,
                        toImageButtonOptions=list(format="png", filename="predictive_fit_profile", scale=2))

                    # Model Parsimony Plot
                    p_par  <- sizes + 1
                    pt_col <- ifelse(cp <= p_par, "#27AE60", "#E67E22")
                    p_pars <- plotly::plot_ly(x=p_par, y=cp, type="scatter", mode="markers",
                        marker=list(color=pt_col, size=14, line=list(color="white", width=1)),
                        showlegend=FALSE,
                        hovertemplate=paste0(gtxt("Parameters"), " %{x}<br>Cp: %{y:.2f}<extra></extra>"))
                    p_pars <- plotly::add_trace(p_pars, x=c(min(p_par), max(p_par)), y=c(min(p_par), max(p_par)),
                        type="scatter", mode="lines", name=gtxt("Cp = p (reference)"),
                        line=list(color="#E74C3C", width=1.6, dash="dash"), hoverinfo="skip")
                    p_pars <- plotly::layout(p_pars, margin=list(t=20, b=90, l=70, r=20),
                        xaxis=list(title=gtxt("Parameters in Model (predictors + intercept)"), tickmode="array", tickvals=p_par,
                                   automargin=TRUE),
                        yaxis=list(title=gtxt("Mallows' Cp Statistic"), automargin=TRUE),
                        legend=list(orientation="h", x=0, y=-0.3))
                    p_pars <- plotly::config(p_pars, displaylogo=FALSE,
                        toImageButtonOptions=list(format="png", filename="model_parsimony_plot", scale=2))

                    sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                        htmltools::tags$h2(style="color:#2C3E50;", gtxt("Variable Selection: Best Subsets")),
                        htmltools::tags$p(style="color:#555;font-size:13px;",
                            gtxtf("Recommended model size: %d predictor(s) (peak Adjusted R-sq).", sizes[best_idx])),
                        htmltools::tags$div(style="display:flex;flex-wrap:wrap;gap:32px;",
                            htmltools::tags$div(style="flex:1;min-width:480px;",
                                htmltools::tags$div(style="font-weight:600;color:#2C3E50;margin-bottom:6px;", gtxt("Predictive Fit Profile")),
                                plotly_embed(p_fit, "chart_fit_profile", height="460px")),
                            htmltools::tags$div(style="flex:1;min-width:480px;",
                                htmltools::tags$div(style="font-weight:600;color:#2C3E50;margin-bottom:6px;", gtxt("Model Parsimony Plot")),
                                plotly_embed(p_pars, "chart_parsimony", height="460px"))))

                    # Factor Inclusion Map (heatmap)
                    wm <- vsel_html$which_matrix
                    if (!is.null(wm)) {
                        wm <- wm[, colnames(wm) != "(Intercept)", drop=FALSE]
                        if (ncol(wm) > 0 && nrow(wm) > 0) {
                            nf   <- ncol(wm); ns <- nrow(wm)
                            zmat <- matrix(as.numeric(wm), nrow=ns, ncol=nf)
                            # Same "Stability" percentage the base-R chart shows below each
                            # factor's column (share of models, at any size, that include
                            # this factor) -- baked directly into the x-axis tick label text
                            # (rather than a separately-positioned annotation row) so Plotly's
                            # own automargin sizing keeps it fully visible instead of risking
                            # being clipped by a manually-guessed margin/offset.
                            stab      <- round(colSums(wm)/ns*100)
                            tick_lbls <- paste0(colnames(wm), "  (", stab, "%)")
                            map_shapes <- list()
                            if (!is.null(vsel_html$best_size) && vsel_html$best_size >= 1 && vsel_html$best_size <= ns) {
                                map_shapes <- list(list(type="line", x0=0, x1=1, xref="paper",
                                                         y0=vsel_html$best_size, y1=vsel_html$best_size,
                                                         line=list(color="#E74C3C", width=2.5, dash="dot")))
                            }
                            # xgap/ygap carve a thin gap between adjacent cells (revealing the
                            # plot's background color underneath), which is how Plotly draws
                            # cell-separator gridlines on a heatmap trace -- the direct
                            # equivalent of the white abline() separators the base-R version
                            # of this chart uses between cells.
                            p_map <- plotly::plot_ly(x=colnames(wm), y=seq_len(ns), z=zmat, type="heatmap",
                                colorscale=list(c(0,"#F2F3F4"), c(1,"#2E86AB")), showscale=FALSE,
                                xgap=2, ygap=2,
                                hovertemplate=paste0(gtxt("Factor"), ": %{x}<br>", gtxt("Model Size"), ": %{y}<br>",
                                                      gtxt("Included"), ": %{z}<extra></extra>"))
                            p_map <- plotly::layout(p_map, margin=list(t=20, b=140, l=100, r=20, autoexpand=TRUE),
                                plot_bgcolor="#FFFFFF",
                                xaxis=list(title=gtxt("Factor  (Stability %)"), tickangle=-45,
                                           tickmode="array", tickvals=colnames(wm), ticktext=tick_lbls,
                                           automargin=TRUE, showgrid=TRUE, gridcolor="#FFFFFF"),
                                yaxis=list(title=gtxt("Model Size (number of predictors)"), tickmode="array",
                                           tickvals=seq_len(ns), automargin=TRUE, showgrid=TRUE, gridcolor="#FFFFFF"),
                                shapes=map_shapes)
                            p_map <- plotly::config(p_map, displaylogo=FALSE,
                                toImageButtonOptions=list(format="png", filename="factor_inclusion_map", scale=2))

                            sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                                htmltools::tags$h2(style="color:#2C3E50;", gtxt("Factor Inclusion Map")),
                                htmltools::tags$p(style="color:#555;font-size:13px;",
                                    gtxtf("Darker fill = factor included at that model size. Dashed red line marks the recommended model size (%d, peak Adj. R-sq).",
                                          vsel_html$best_size)),
                                plotly_embed(p_map, "chart_factor_map", height="540px"))
                        }
                    }
                }
            }
        }, error=function(e) warns$warn(gtxtf("Interactive variable selection charts: %s", e$message), dostop=FALSE))
    }

    # ── Optimizer Summary (if requested) ─────────────────────────────────
    if (!is.null(opt_goal)) {
        tryCatch({
            lowers <- sapply(var_names, function(v) {
                i <- if (!is.null(spec)) which(as.character(spec$var)==v) else integer(0)
                if (length(i)) as.numeric(spec$lows[i[1]]) else min(data[[v]], na.rm=TRUE)
            })
            uppers <- sapply(var_names, function(v) {
                i <- if (!is.null(spec)) which(as.character(spec$var)==v) else integer(0)
                if (length(i)) as.numeric(spec$highs[i[1]]) else max(data[[v]], na.rm=TRUE)
            })
            obj_fn <- function(x) {
                nd <- as.data.frame(t(x)); names(nd) <- var_names
                nd <- .doe_add_centerpt(fit, nd)
                pred <- tryCatch(suppressWarnings(predict(fit, newdata=nd)), error=function(e) NA)
                if (is.na(pred)) return(1e10)
                if (opt_goal=="maximize") return(-pred)
                if (opt_goal=="minimize") return( pred)
                return(abs(pred - mean(y, na.rm=TRUE)))
            }
            opt <- tryCatch(optim(par=(lowers+uppers)/2, fn=obj_fn, method="L-BFGS-B",
                                   lower=lowers, upper=uppers), error=function(e) NULL)
            if (!is.null(opt)) {
                nd_opt <- as.data.frame(t(opt$par)); names(nd_opt) <- var_names
                nd_opt <- .doe_add_centerpt(fit, nd_opt)
                opt_y  <- suppressWarnings(predict(fit, newdata=nd_opt))
                summary_txt <- paste(sprintf("%s = %.4f", c(var_names, resp),
                                              c(round(opt$par,4), round(opt_y,4))), collapse="   |   ")

                # Interactive counterpart to the base-R chart that
                # do_optimization() submits via spssRGraphics.Submit() -- the
                # Optimizer Summary previously rendered only this text line
                # with no chart at all in the HTML report. Mirrors the same
                # two cases base-R handles: a single factor gets a predicted-
                # response curve with the optimum marked; 2+ factors get a
                # horizontal bar of normalized optimal settings.
                opt_chart <- tryCatch({
                    if (length(var_names) == 1) {
                        xseq <- seq(lowers[1], uppers[1], length.out=100)
                        nd_seq <- data.frame(xseq); names(nd_seq) <- var_names
                        nd_seq <- .doe_add_centerpt(fit, nd_seq)
                        yseq <- suppressWarnings(predict(fit, newdata=nd_seq))
                        p_opt <- plotly::plot_ly(x=xseq, y=yseq, type="scatter", mode="lines",
                            name=gtxt("Predicted Response"),
                            line=list(color="#2980B9", width=2),
                            hovertemplate=paste0(var_names[1], ": %{x:.4f}<br>", resp, ": %{y:.4f}<extra></extra>"))
                        p_opt <- plotly::add_trace(p_opt, x=opt$par[1], y=opt_y, type="scatter", mode="markers",
                            name=gtxt("Optimum"), marker=list(color="#C0392B", size=12, symbol="diamond"),
                            hovertemplate=paste0(gtxt("Optimum"), "<br>", var_names[1], ": %{x:.4f}<br>",
                                                  resp, ": %{y:.4f}<extra></extra>"))
                        p_opt <- plotly::layout(p_opt, margin=list(t=20),
                            xaxis=list(title=var_names[1]), yaxis=list(title=resp))
                        plotly::config(p_opt, displaylogo=FALSE,
                            toImageButtonOptions=list(format="png", filename="optimizer_chart", scale=2))
                    } else {
                        d_norm <- (opt$par - lowers) / (uppers - lowers)
                        d_norm[!is.finite(d_norm)] <- 0
                        p_opt <- plotly::plot_ly(x=d_norm, y=var_names, type="bar", orientation="h",
                            marker=list(color="#2980B9"),
                            text=sprintf("%.4f", opt$par), textposition="outside",
                            hovertemplate=paste0("%{y}: %{text}<extra></extra>"))
                        p_opt <- plotly::layout(p_opt, margin=list(t=20, r=60),
                            xaxis=list(title=gtxt("Normalized Optimal Setting (0 = low, 1 = high)"), range=list(0,1.15)),
                            yaxis=list(title=""))
                        plotly::config(p_opt, displaylogo=FALSE,
                            toImageButtonOptions=list(format="png", filename="optimizer_chart", scale=2))
                    }
                }, error=function(e) NULL)

                sections[[length(sections)+1]] <- htmltools::tags$div(
                    style="padding:16px 32px;background:#F4F6F7;border-top:2px solid #2C3E50;",
                    htmltools::tags$h2(style="color:#2C3E50;", gtxtf("Optimizer Summary (%s)", toupper(opt_goal))),
                    htmltools::tags$p(style="font-family:monospace;font-size:14px;", summary_txt),
                    if (!is.null(opt_chart)) htmltools::tags$div(style="max-width:760px;margin-top:8px;",
                        plotly_embed(opt_chart, "chart_optimizer")))
            }
        }, error=function(e) NULL)
    }

    # Only the header so far means no chart was requested: write no (empty) report.
    if (length(sections) <= 1) return(invisible(NULL))

    sections[[length(sections)+1]] <- htmltools::tags$div(
        style="padding:12px 32px;color:#999;font-size:11px;border-top:1px solid #eee;",
        gtxt("Generated by STATS DOE ANALYSIS. This interactive report supplements the static charts in the SPSS Viewer; open it in any modern web browser."))

    out_path <- htmlpath
    if (!grepl("\\.html?$", out_path, ignore.case=TRUE)) out_path <- paste0(out_path, ".html")

    saved <- save_single_file_report(sections, out_path,
                 gtxtf("DOE Analysis Report: %s", resp), warns)
    if (isTRUE(saved)) {
        msg <- gtxtf("Interactive HTML report saved to:\n%s", out_path)
        notify_html_saved(msg, gtxt("Interactive HTML Report"), "STATSDOEHTML", open_new=TRUE)
    }

    invisible(NULL)
}

# ════════════════════════════════════════════════════════════════════════════
# INTERACTIVE HTML REPORT (plotly / htmlwidgets) — DESIGN-GENERATION PHASE
# ════════════════════════════════════════════════════════════════════════════
# Companion to export_html_report() (post-analysis phase, below). Builds a
# standalone, self-contained interactive HTML file covering the design that
# was just generated: settings, factor specifications, the design worksheet,
# an interactive scatterplot matrix of the design space (colored by run
# order), and - for optimal designs - the efficiency statistics and an
# interactive confounding-matrix heatmap. Saved automatically right after the
# SPSS Viewer output, independent of any GUI control (none exists, by design).
export_design_html_report <- function(res, specdata, variables, designtype, model, frml,
                                       constant, criterion, initial, repeats, designalg,
                                       ncand, outputdataset, confounding, designeval,
                                       warns, htmlpath, alias_info=NULL) {
    if (!has_plotly || !has_htmlwidgets) {
        warns$warn(gtxt("Interactive design HTML report skipped: requires the 'plotly' and 'htmlwidgets' R packages. Install with: install.packages(c('plotly','htmlwidgets'))"), dostop=FALSE)
        return(invisible(NULL))
    }
    has_htmltools <- requireNamespace("htmltools", quietly=TRUE)
    if (!has_htmltools) {
        warns$warn(gtxt("Interactive design HTML report skipped: requires the 'htmltools' R package (normally installed automatically with htmlwidgets)."), dostop=FALSE)
        return(invisible(NULL))
    }
    if (is.null(htmlpath) || !nzchar(htmlpath)) {
        warns$warn(gtxt("Interactive design HTML report skipped: no output path could be resolved."), dostop=FALSE)
        return(invisible(NULL))
    }

    suppressMessages(suppressWarnings(library(plotly)))
    suppressMessages(suppressWarnings(library(htmlwidgets)))
    suppressMessages(suppressWarnings(library(htmltools)))

    # Same shared chart-title helper used by export_html_report() -- defined
    # again here (own local copy) since each export_*_html_report() function
    # has its own scope; ptitle() is not shared across functions in R.
    ptitle <- function(txt) list(text=txt, font=list(size=14), x=0.02, xanchor="left")

    designtype_display <- switch(tolower(designtype),
        "optimal"        = "Optimal Design (D/A criterion)",
        "factorial"      = "Full / Fractional Factorial",
        "plackettburman" = "Plackett-Burman Screening",
        "rsm"            = "Response Surface (CCD)",
        "boxbehnken"     = "Box-Behnken Design",
        "ccd"            = "Central Composite Design (CCD)",
        "taguchi"        = "Taguchi Orthogonal Array",
        "lhs"            = "Latin Hypercube Sampling",
        "dsd"            = "Definitive Screening Design (DSD)",
        "simplexlattice" = "Simplex-Lattice Mixture Design",
        "simplexcentroid"= "Simplex-Centroid Mixture Design",
        "fullfactorial"  = "General Full Factorial",
        designtype)

    sections <- list()

    # ── Header ────────────────────────────────────────────────────────────
    sections[[length(sections)+1]] <- htmltools::tags$div(
        style="font-family:'Segoe UI',Helvetica,Arial,sans-serif;padding:24px 32px;background:linear-gradient(135deg,#1A5276,#2980B9);color:#fff;",
        htmltools::tags$h1(style="margin:0 0 4px 0;font-size:28px;", gtxt("Design of Experiments — Design Report")),
        htmltools::tags$p(style="margin:0;opacity:0.9;font-size:14px;",
            sprintf("%s: %s  |  %s: %s  |  %s",
                    gtxt("Design Type"), designtype_display,
                    gtxt("Output Dataset"), outputdataset,
                    format(Sys.time(), "%Y-%m-%d %H:%M"))))

    # ── Settings/variable-spec/design-worksheet tables intentionally NOT
    # duplicated into this HTML report -- by design decision, the
    # interactive HTML reports (both this Design report and the Analysis
    # report below) show ONLY charts/plots. All of this tabular content
    # (settings summary, variable specifications, the design worksheet,
    # design evaluation statistics, and the alias/confounding tables) is
    # already displayed in full as SPSS Viewer pivot tables by the caller
    # (displayresults()), so nothing is lost -- it is simply not repeated
    # here in a second, static HTML table.

    # ── Interactive design-space scatterplot matrix ─────────────────────
    suppressWarnings(tryCatch({
        meta_cols <- c("Reps","Proportion","StdOrder","RunOrder","Block","CenterPt","PtType")
        num_vars  <- intersect(as.character(variables), setdiff(names(res$design), meta_cols))
        num_vars  <- num_vars[sapply(num_vars, function(v) is.numeric(res$design[[v]]))]
        if (length(num_vars) == 2) {
            # plotly.js draws an empty panel for a 2-variable splom with the
            # upper half and diagonal hidden, so use an ordinary scatter here.
            order_col <- if ("RunOrder" %in% names(res$design)) res$design$RunOrder else seq_len(nrow(res$design))
            p_xy <- plotly::plot_ly(x=res$design[[num_vars[1]]], y=res$design[[num_vars[2]]],
                type="scatter", mode="markers",
                marker=list(color=order_col, colorscale="Viridis", showscale=TRUE,
                            colorbar=list(title=gtxt("Run Order")),
                            size=10, line=list(color="white", width=0.5)),
                text=paste(gtxt("Run"), order_col),
                hovertemplate=paste0("%{text}<br>", num_vars[1], ": %{x}<br>", num_vars[2], ": %{y}<extra></extra>"))
            p_xy <- plotly::layout(p_xy, title=ptitle(gtxt("Design Space Coverage (color = Run Order)")),
                xaxis=list(title=num_vars[1]), yaxis=list(title=num_vars[2]))
            p_xy <- plotly::config(p_xy, displaylogo=FALSE,
                toImageButtonOptions=list(format="png", filename="design_space", scale=2))
            sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                htmltools::tags$h2(style="color:#2C3E50;", gtxt("Design Space Visualization")),
                htmltools::tags$p(style="color:#555;font-size:13px;",
                    gtxt("Scatter of the two numeric factors, colored by run order, to visually confirm balanced coverage.")),
                plotly_embed(p_xy, "chart_splom", height="520px"))
        } else if (length(num_vars) > 2) {
            dims      <- lapply(num_vars, function(v) list(label=v, values=res$design[[v]]))
            order_col <- if ("RunOrder" %in% names(res$design)) res$design$RunOrder else seq_len(nrow(res$design))
            # NOTE: 'splom' traces have no 'mode' attribute (unlike 'scatter') -
            # marker styling alone is sufficient; plotly renders markers by
            # default for splom, so 'mode' must NOT be set here.
            p_splom <- plotly::plot_ly(type="splom", dimensions=dims,
                marker=list(color=order_col, colorscale="Viridis", showscale=TRUE,
                            size=7, line=list(color="white", width=0.5)),
                showupperhalf=FALSE, diagonal=list(visible=FALSE))
            p_splom <- plotly::layout(p_splom, title=ptitle(gtxt("Design Space Coverage (color = Run Order)")))
            p_splom <- plotly::config(p_splom, displaylogo=FALSE,
                toImageButtonOptions=list(format="png", filename="design_space_matrix", scale=2))

            sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                htmltools::tags$h2(style="color:#2C3E50;", gtxt("Design Space Visualization")),
                htmltools::tags$p(style="color:#555;font-size:13px;",
                    gtxt("Every pairwise scatter of the numeric factors, colored by run order, to visually confirm balanced space-filling/coverage.")),
                plotly_embed(p_splom, "chart_splom", height="520px"))
        } else if (length(num_vars) == 1) {
            v <- num_vars[1]
            p_strip <- plotly::plot_ly(x=res$design[[v]], y=rep(1, nrow(res$design)),
                type="scatter", mode="markers",
                marker=list(size=10, color="#2980B9", line=list(color="white", width=1)),
                hovertemplate=paste0(v, ": %{x}<extra></extra>"))
            p_strip <- plotly::layout(p_strip, title=ptitle(gtxtf("Design Points: %s", v)),
                yaxis=list(visible=FALSE, range=c(0.5,1.5)))
            p_strip <- plotly::config(p_strip, displaylogo=FALSE)
            sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                plotly_embed(p_strip, "chart_strip"))
        }
    }, error=function(e) warns$warn(gtxtf("Design space visualization: %s", e$message), dostop=FALSE)))

    # ── Design evaluation + confounding heatmap (optimal designs) ───────
    if (!is.null(designeval)) {
        tryCatch({
            el <- c("D","A (Avg Coeff Variance)","Ge (Minimax Efficiency)",
                    "Dea (D Efficiency Lower Bound)",
                    gtxt("Determinant"), gtxt("Diagonality"), gtxt("Geom Mean Variances"))
            ev <- c(res$D, res$A, res$Ge, res$Dea,
                    designeval$determinant, designeval$diagonality, designeval$gmean.variances)
            el      <- el[seq_along(ev)]
            # Design Evaluation statistics table intentionally not duplicated
            # here -- charts/plots only in this report; the full table is
            # already shown as an SPSS Viewer pivot table (templateName
            # "OPTDESEVAL" in displayresults()).

            if (confounding && !is.null(designeval$confounding)) {
                cm <- as.matrix(designeval$confounding)
                # BUGFIX: AlgDesign's eval.design()$confounding matrix carries
                # variable names on its ROWS but not its columns (as.matrix()
                # leaves the columns as bare positional indices 0,1,2,...),
                # which is why the x-axis showed plain numbers instead of
                # variable names. The SPSS Viewer pivot table version of this
                # same matrix (displayresults(), templateName "OPTDESCONF")
                # already works around this by copying the row names onto the
                # columns before display -- the matrix is a variable-by-
                # variable table, so column and row labels are meant to match.
                # This HTML heatmap version was missing that same fix.
                if (!is.null(rownames(cm))) colnames(cm) <- rownames(cm)
                sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:16px 32px;",
                    htmltools::tags$h2(style="color:#2C3E50;", gtxt("Confounding Matrix")),
                    heatmap_embed(cm, "chart_confound", gtxt("Confounding Matrix"),
                                  filename="confounding_matrix"))
            }
        }, error=function(e) warns$warn(gtxtf("Design evaluation HTML section: %s", e$message), dostop=FALSE))
    }

    # ── Alias / confounding structure (factorial & Plackett-Burman) ────────
    if (!is.null(alias_info)) {
        tryCatch({
            # Resolution/note and alias-chains tables intentionally not
            # duplicated here -- charts/plots only in this report; the full
            # tables are already shown as SPSS Viewer pivot tables
            # (templateName "DOEALIASRES"/"DOEALIASCHAINS" in
            # displayresults()). Only the partial-aliasing case below
            # produces any content here, since it is a heatmap chart.
            if (alias_info$type=="partial" && !is.null(alias_info$matrix)) {
                cm <- alias_info$matrix
                sections[[length(sections)+1]] <- htmltools::tags$div(style="padding:0 32px 16px 32px;",
                    htmltools::tags$h3(style="color:#2C3E50;font-size:16px;", gtxt("Partial Aliasing Matrix")),
                    heatmap_embed(cm, "chart_alias_partial",
                                  gtxt("Partial Aliasing: Main Effects vs Two-Factor Interactions"),
                                  filename="partial_aliasing"))
            }
        }, error=function(e) warns$warn(gtxtf("Alias/confounding HTML section: %s", e$message), dostop=FALSE))
    }

    sections[[length(sections)+1]] <- htmltools::tags$div(
        style="padding:12px 32px;color:#999;font-size:11px;border-top:1px solid #eee;",
        gtxt("Generated by STATS DOE ANALYSIS. This interactive report supplements the static output in the SPSS Viewer; open it in any modern web browser. Re-run with RESPONSEVAR= and ANALYZE=YES to generate the post-analysis report."))

    out_path <- htmlpath
    if (!grepl("\\.html?$", out_path, ignore.case=TRUE)) out_path <- paste0(out_path, ".html")

    saved <- save_single_file_report(sections, out_path,
                 gtxt("DOE Design Report"), warns)
    if (isTRUE(saved)) {
        msg <- gtxtf("Interactive design report saved to:\n%s", out_path)
        # displayresults() already has a procedure open at this point (it is
        # closed by the caller right after this function returns) - add the
        # TextBlock directly to that open procedure instead of starting a new
        # one (StartProcedure cannot be nested).
        notify_html_saved(msg, gtxt("Interactive Design Report"), "STATSOPTDESIGN", open_new=FALSE)
    }

    invisible(NULL)
}

# ════════════════════════════════════════════════════════════════════════════
# DISPLAY RESULTS (design summary)
# ════════════════════════════════════════════════════════════════════════════

displayresults <- function(res, data, hasformula, variables, vlevels, designtype, model,
                            constraintfunc, constant, criterion, center, initial, repeats,
                            designalg, ncand, outputdataset, frml, confounding, warns,
                            exporthtml=FALSE, htmlpath=NULL) {
    StartProcedure(gtxt("Design of Experiments"), "STATSOPTDESIGN")
    designeval_for_html <- NULL  # captured below for the HTML report, if computed

    designtype_display <- switch(tolower(designtype),
        "optimal"        = "Optimal Design (D/A criterion)",
        "factorial"      = "Full / Fractional Factorial",
        "plackettburman" = "Plackett-Burman Screening",
        "rsm"            = "Response Surface (CCD)",
        "boxbehnken"     = "Box-Behnken Design",
        "ccd"            = "Central Composite Design (CCD)",
        "taguchi"        = "Taguchi Orthogonal Array",
        "lhs"            = "Latin Hypercube Sampling",
        "dsd"            = "Definitive Screening Design (DSD)",
        "simplexlattice" = "Simplex-Lattice Mixture Design",
        "simplexcentroid"= "Simplex-Centroid Mixture Design",
        designtype)

    if (hasformula) model <- gtxt("formula")

    lbls <- c(gtxt("Design Type"), gtxt("Number of Candidate Points"),
              gtxt("Number of Runs"), gtxt("Model"), gtxt("Expanded Model"),
              gtxt("Include Constant"), gtxt("Initial Design"), gtxt("Criterion"),
              gtxt("Center Data"), gtxt("Repeats"), gtxt("Algorithm"), gtxt("Output Dataset"))
    vals <- c(designtype_display,
              ifelse(is.null(ncand), gtxt("not specified"), ncand),
              nrow(res$design), model, frml,
              ifelse(constant, gtxt("Yes"), gtxt("No")),
              initial, criterion, ifelse(center, gtxt("Yes"), gtxt("No")),
              repeats, designalg, outputdataset)
    spsspivottable.Display(data.frame(cbind(vals), row.names=lbls),
        title=gtxt("Settings and Results Summary"),
        collabels=c(gtxt("Summary")),
        templateName="OPTDESSUMMARYMC", outline=gtxt("Summary"),
        caption=gtxt("Results computed by AlgDesign / base R"))

    names(data) <- c(gtxt("Variable"), gtxt("Low"), gtxt("High"), gtxt("Center"),
                     gtxt("Levels"), gtxt("Round"), gtxt("Factor"), gtxt("Mixture"))
    data[7] <- sapply(data[7], function(x) ifelse(x, gtxt("Yes"), gtxt("No")))
    data[8] <- sapply(data[8], function(x) ifelse(x, gtxt("Yes"), gtxt("No")))
    spsspivottable.Display(data,
        title=gtxt("Variable Specifications"),
        caption=ifelse(!is.null(constraintfunc),
            gtxtf("Constraint function: %s", constraintfunc),
            gtxt("Constraint function: None")))

    # Design worksheet preview
    preview <- head(res$design, 20)
    # Format Proportion (approximate-theory optimal designs, DESIGNALG=APPROX)
    # as an explicit zero-padded string before display. SPSS's pivot-table
    # numeric rendering otherwise drops the leading zero for values below 1
    # (e.g. ".31" instead of "0.31"); converting to character here guarantees
    # the exact text shown, without altering the underlying numeric value
    # anywhere else (res$design itself, the saved output dataset, and every
    # other table/HTML use of res$design are untouched -- only this local
    # display copy is affected).
    if ("Proportion" %in% names(preview) && is.numeric(preview$Proportion)) {
        preview$Proportion <- sprintf("%.2f", preview$Proportion)
    }
    for (cn in names(preview)) {
        v <- preview[[cn]]
        if (is.numeric(v) && all(is.na(v) | abs(v - round(v)) < 1e-9))
            preview[[cn]] <- ifelse(is.na(v), "", format(round(v), scientific=FALSE, trim=TRUE))
    }
    spsspivottable.Display(preview,
        title  = if (nrow(res$design) > 20) gtxtf("Design Worksheet — %d runs (showing first 20)", nrow(res$design))
                 else gtxtf("Design Worksheet — %d runs", nrow(res$design)),
        caption= gtxt("Full design saved to output dataset. Add your response column, then re-run with RESPONSEVAR= and ANALYZE=YES."),
        templateName="DOEWORKSHEET", outline=gtxt("Design Worksheet"))

    # Evaluation — optimal designs only
    if (tolower(designtype)=="optimal") {
        emsg       <- ""
        designeval <- tryCatch(
            eval.design(frml=as.formula(frml), design=res$design,
                        confounding=TRUE, variances=TRUE, center=center),
            error=function(e) { emsg <<- e$message; warns$warn(e$message, dostop=FALSE); NULL })
        if (!is.null(designeval)) {
            el <- c("D","A (Avg Coeff Variance)","Ge (Minimax Efficiency)",
                    "Dea (D Efficiency Lower Bound)",
                    gtxt("Determinant"), gtxt("Diagonality"), gtxt("Geom Mean Variances"))
            ev <- c(res$D, res$A, res$Ge, res$Dea,
                    designeval$determinant, designeval$diagonality, designeval$gmean.variances)
            el <- el[seq_along(ev)]
            spsspivottable.Display(data.frame(cbind(ev), row.names=el),
                title=gtxt("Design Evaluation"), caption=emsg,
                collabels=c(gtxt("Statistics")),
                templateName="OPTDESEVAL", outline=gtxt("Evaluation"))
            if (confounding && !is.null(designeval$confounding)) {
                condf        <- data.frame(designeval$confounding)
                names(condf) <- row.names(condf)
                spsspivottable.Display(condf,
                    title=gtxt("Confounding Matrix"),
                    templateName="OPTDESCONF", outline=gtxt("Confounding"),
                    caption=gtxt("Each column: coefficients of that variable regressed on the others"))
            }
            designeval_for_html <- designeval
        }
    }

    # ── Alias / confounding structure -- factorial & Plackett-Burman designs ─
    # Additive only: gated by the same CONFOUNDING flag already used for the
    # optimal-design confounding matrix above; existing default output for
    # every design type is unchanged when this block finds nothing to add.
    alias_info_for_html <- NULL
    if (confounding && tolower(designtype) %in% c("factorial","plackettburman","fullfactorial")) {
        alias_info <- compute_alias_structure(res, designtype, warns)
        if (!is.null(alias_info)) {
            resn <- if (!is.na(alias_info$resolution)) alias_info$resolution else gtxt("not applicable")
            spsspivottable.Display(
                data.frame(Value=c(resn), row.names=c(gtxt("Design Resolution"))),
                title=gtxt("Alias / Confounding Structure"),
                collabels=c(gtxt("Resolution")),
                caption=if (!is.null(alias_info$note)) alias_info$note else "",
                templateName="DOEALIASRES", outline=gtxt("Aliasing"))

            if (alias_info$type=="full" && length(alias_info$chains) > 0) {
                spsspivottable.Display(
                    data.frame(Chain=alias_info$chains, stringsAsFactors=FALSE),
                    title=gtxt("Alias Chains"),
                    templateName="DOEALIASCHAINS", outline=gtxt("Aliasing"),
                    caption=gtxt("Effects within the same chain are fully confounded and cannot be separately estimated from this design."))
            } else if (alias_info$type=="partial" && !is.null(alias_info$matrix)) {
                condf <- data.frame(alias_info$matrix, check.names=FALSE)
                spsspivottable.Display(condf,
                    title=gtxt("Partial Aliasing: Main Effects vs Two-Factor Interactions"),
                    templateName="DOEALIASPARTIAL", outline=gtxt("Aliasing"),
                    caption=gtxt("Rows: main effects. Columns: two-factor interactions. Values: correlation coefficients (degree of confounding)."))
            }
            alias_info_for_html <- alias_info
        }
    }

    # ── Automatic interactive HTML report (design-generation phase) ─────────
    # Fires right after the SPSS Viewer output above, independent of any GUI
    # control (none exists, by design - see EXPORTHTML keyword default=YES in
    # Run()). Saves to a cross-platform default location unless HTMLPATH was
    # typed explicitly in syntax. Never alters anything already displayed above.
    if (isTRUE(exporthtml)) {
        tryCatch({
            rp <- if (!is.null(htmlpath) && nzchar(htmlpath)) htmlpath
                  else make_default_html_path("Design", outputdataset)
            export_design_html_report(res, data, variables, designtype, model, frml,
                                      constant, criterion, initial, repeats, designalg,
                                      ncand, outputdataset, confounding, designeval_for_html,
                                      warns, rp, alias_info=alias_info_for_html)
        }, error=function(e) warns$warn(gtxtf("Design HTML report error: %s", e$message), dostop=FALSE))
    }
}

# ════════════════════════════════════════════════════════════════════════════
# DATASET GENERATION
# ════════════════════════════════════════════════════════════════════════════

# ── Factor list without VARNAMES ─────────────────────────────────────────────
.doe_meta_cols <- c("reps","rep","replicate","replicates","proportion","stdorder","runorder","run",
                    "block","blocks","centerpt","pttype","id")
.doe_saved_factors <- function() {
    v <- tryCatch(unlist(spssdictionary.GetDataFileAttributes("DOE_Factors")), error=function(e) NULL)
    if (is.null(v)) character(0) else trimws(as.character(v))
}
.doe_resolve_factors <- function(data, responses) {
    nm <- names(data); resp_lc <- tolower(as.character(unlist(responses)))
    saved <- .doe_saved_factors()
    if (length(saved)) {
        m <- nm[match(tolower(saved), tolower(nm))]
        m <- m[!is.na(m) & !tolower(m) %in% resp_lc]
        if (length(m)) return(list(factors=m, source="saved"))
    }
    n <- nrow(data)
    cand <- nm[!tolower(nm) %in% c(.doe_meta_cols, resp_lc)]
    keep <- vapply(cand, function(v) {
        x <- data[[v]]; x <- x[!is.na(x)]
        if (is.character(x) || is.factor(x)) x <- trimws(as.character(x))
        if (is.character(x)) x <- x[nzchar(x)]
        k <- length(unique(x))
        k >= 2 && k <= min(7, max(2, floor(n / 2)))
    }, logical(1))
    list(factors=cand[keep], source="detected")
}
.doe_show_factors <- function(data, factors, source) {
    if (source == "given" || !length(factors)) return(invisible(NULL))
    lv <- vapply(factors, function(v) { x <- data[[v]]; x <- x[!is.na(x)]
        u <- sort(unique(if (is.numeric(x)) x else trimws(as.character(x))))
        paste(if (length(u) > 6) c(head(u, 6), "...") else u, collapse=", ") }, character(1))
    df <- data.frame(vapply(factors, function(v) if (is.numeric(data[[v]])) gtxt("Numeric") else gtxt("Text"), character(1)),
                     lv, stringsAsFactors=FALSE)
    names(df) <- c(gtxt("Type"), gtxt("Settings in the data"))
    cap <- if (source == "saved") gtxt("VARNAMES was not given: the factors were read from the design information saved in this dataset (data file attribute DOE_Factors). Give VARNAMES to use other factors.")
           else gtxt("VARNAMES was not given: these columns were detected as factors (2 to 7 distinct settings; order, block, center-point and response columns excluded). Check the list; if it is wrong, name the factors with VARNAMES.")
    tryCatch({
        StartProcedure(gtxt("Design of Experiments: Factors Used"), "STATSDOEFACTORS")
        spsspivottable.Display(df, title=gtxt("Factors Used"), templateName="DOEFACTORSUSED",
                               outline=gtxt("Factors Used"), rowlabels=factors, caption=cap)
    }, error=function(e) NULL, finally=tryCatch(spsspkg.EndProcedure(), error=function(e) NULL))
}
.doe_store_factors <- function(factors, warns) {
    saved <- .doe_saved_factors()
    if (length(saved) == length(factors) && all(tolower(saved) == tolower(factors))) return(invisible(NULL))
    q <- function(x) paste0("'", gsub("'", "''", x, fixed=TRUE), "'")
    cmd <- paste0("DATAFILE ATTRIBUTE ATTRIBUTE=",
                  paste0("DOE_Factors[", seq_along(factors), "](", q(factors), ")", collapse=" "),
                  " DOE_CreatedBy('STATS DOE ANALYSIS').")
    ok <- tryCatch({ if (length(saved)) spsspkg.Submit("DATAFILE ATTRIBUTE DELETE=DOE_Factors.")
                     spsspkg.Submit(cmd); TRUE }, error=function(e) FALSE)
    if (ok) warns$warn(gtxtf("The factor list (%s) was saved in this dataset's design information; save the dataset to keep it. Next time VARNAMES can be left blank.",
                             paste(factors, collapse=", ")), dostop=FALSE)
}

gendataset <- function(res, outputdataset, variables, factorlist, warns, designtype=NULL) {
    varspec    <- list()
    varnames   <- names(res$design)
    varnameslc <- lapply(varnames, tolower)
    while (varnameslc[1] %in% varnameslc[2:length(varnameslc)]) {
        varnames[1]  <- paste0(varnames[1],".")
        varnameslc[1]<- tolower(varnames[1])
    }
    for (v in seq_len(ncol(res$design))) {
        if (is.numeric(res$design[[v]])) {
            fmt <- "F8.2"; mlv <- "scale"; len <- 0
        } else {
            # NOTE: design columns reaching this branch are not guaranteed to
            # be factors (e.g. augment_to_ccd()'s "PtType" column is a plain
            # character vector) -- levels() on a plain character vector
            # returns NULL, and sapply(NULL, nchar) silently returns list()
            # rather than an empty numeric vector, which then makes
            # max(list(), 1) fail with "invalid 'type' (list) of argument".
            # Build the candidate label set from levels() when available and
            # fall back to the column's own unique non-missing values
            # otherwise, so both factor and plain character/logical columns
            # are handled identically.
            col <- res$design[[v]]
            ll  <- if (is.factor(col)) levels(col) else unique(as.character(col))
            ll  <- ll[!is.na(ll)]
            len <- if (length(ll) > 0) max(sapply(ll, nchar), 1) * 3 else 8
            fmt <- paste0("A",len); mlv <- "nominal"
        }
        varspec[[v]] <- c(varnames[v],"",len,fmt,mlv)
    }
    dsdict <- do.call(spssdictionary.CreateSPSSDictionary, varspec)
    spssdictionary.SetDictionaryToSPSS(outputdataset, dsdict)
    spssdata.SetDataToSPSS(outputdataset, res$design)
    # Store the design information in the dataset itself (data file
    # attributes, saved with the .sav file), so a later analysis of this
    # dataset does not need VARNAMES -- like a Minitab design worksheet.
    fnames <- intersect(as.character(unlist(variables)), names(res$design))
    if (length(fnames))
        tryCatch(spssdictionary.SetDataFileAttributes(outputdataset, DOE_Factors=fnames,
                     DOE_DesignType=if (is.null(designtype)) "" else as.character(designtype)[1],
                     DOE_CreatedBy="STATS DOE ANALYSIS"), error=function(e) NULL)
    spssdictionary.EndDataStep()
}

# ════════════════════════════════════════════════════════════════════════════
# UTILITY FUNCTIONS (coerce_yesno is defined at top of file)
# ════════════════════════════════════════════════════════════════════════════

validate <- function(varnames, frml, factors, nlevels, lows, highs, centers, roundtos, mixtures, warns,
                      designtype=NULL, model=NULL) {
    if (is.null(varnames) && is.null(frml))
        warns$warn(gtxt("Either VARNAMES or FORMULA must be specified."), dostop=TRUE)
    if (!is.null(frml)) {
        if (substr(frml,1,1)!="~") frml <- paste("~",frml)
        varnamesx <- tryCatch(all.vars(formula(frml)),
                              error=function(e) warns$warn(e$message, dostop=TRUE))
        if (!is.null(varnames)) {
            if (length(setdiff(varnames, varnamesx))>0)
                warns$warn(gtxt("Formula variable names inconsistent with VARNAMES"), dostop=TRUE)
        } else { varnames <- varnamesx }
    }
    nvars      <- length(varnames)
    lcvarnames <- tolower(varnames)
    if (length(union(lcvarnames, lcvarnames))!=nvars)
        warns$warn(gtxt("Duplicate variable names were found"), dostop=TRUE)
    # BUGFIX: enforce a per-name length limit on VARNAMES. Without this, a
    # single overlong token (e.g. no spaces typed between what was meant to
    # be several variable names) is silently accepted here even though it is
    # nowhere close to SPSS's 63-byte technical ceiling (see the Output
    # Dataset check elsewhere in this dialog for that limit) -- a 30-40
    # character gibberish string is perfectly legal to SPSS but not a
    # sensible variable name. Use a stricter, practical limit here instead.
    # Check each name individually (not the combined VARNAMES string) so
    # legitimate multi-variable lists longer than this in total are
    # unaffected; only a single name that is itself too long is rejected.
    max_varname_bytes <- 32
    overlong <- varnames[nchar(varnames, type="bytes") > max_varname_bytes]
    if (length(overlong) > 0)
        warns$warn(gtxtf("Variable name(s) too long (max %d bytes): %s",
                          max_varname_bytes, paste(overlong, collapse=", ")), dostop=TRUE)
    # BUGFIX: reject VARNAMES entries that are not valid SPSS variable names
    # (special characters, names starting with a digit, trailing period, or
    # a reserved keyword) up front, with a clear message -- instead of
    # letting them reach dataset/variable creation later, where they would
    # fail with a generic, hard-to-trace SPSS error. A valid SPSS variable
    # name starts with a letter and contains only letters, digits, '.', or
    # '_' after that, must not end in '.', and must not be a reserved word.
    valid_name_pattern <- "^[A-Za-z][A-Za-z0-9_.]*$"
    reserved_words <- c("all","and","by","eq","ge","gt","le","lt","ne","not","or","to","with")
    bad_chars   <- varnames[!grepl(valid_name_pattern, varnames)]
    bad_trail   <- varnames[grepl(valid_name_pattern, varnames) & grepl("\\.$", varnames)]
    bad_reserved<- varnames[tolower(varnames) %in% reserved_words]
    bad_names   <- unique(c(bad_chars, bad_trail, bad_reserved))
    if (length(bad_names) > 0)
        warns$warn(gtxtf("Variable name(s) not valid: %s (must start with a letter and contain only letters, digits, '.', or '_'; cannot end in '.'; cannot be a reserved word)",
                          paste(bad_names, collapse=", ")), dostop=TRUE)
    # BUGFIX: FACTORS/MIXTURES must be yes/no keywords only. coerce_yesno()
    # silently maps anything unrecognized to FALSE with no warning, so a typo
    # like "yess" or a stray value would silently behave as "no" instead of
    # being rejected. Validate against the known-good set *before* coercing,
    # so bad input gets a clear, translatable stop message instead of being
    # silently swallowed. (This mirrors the existing fixtype() helper's
    # validate-then-stop pattern used elsewhere in this file, applied here
    # without changing coerce_yesno()'s own behavior/signature, which other
    # call sites still rely on.)
    check_yesno_values <- function(item, label, warns) {
        vals <- tolower(trimws(as.character(unlist(item))))
        bad  <- setdiff(unique(vals), c("yes","no","true","false","1","0"))
        if (length(bad) > 0)
            warns$warn(gtxtf("%s must contain only yes/no values; found invalid entry: %s",
                              label, paste(bad, collapse=", ")), dostop=TRUE)
    }
    if (!is.null(factors))  check_yesno_values(factors,  gtxt("Factors"),  warns)
    if (!is.null(mixtures)) check_yesno_values(mixtures, gtxt("Mixtures"), warns)

    # Properly coerce factors and mixtures using coerce_yesno
    centers_was_null <- is.null(centers)
    factors  <- coerce_yesno(unlist(fixup(nvars, factors,  gtxt("factors"),            warns, numeric=FALSE)))
    lows     <- fixup(nvars, lows,    gtxt("low values"),                 warns)
    highs    <- fixup(nvars, highs,   gtxt("high values"),                warns)
    centers  <- fixup(nvars, centers, gtxt("center values"),              warns)
    nlevels  <- fixup(nvars, nlevels, gtxt("number of levels"),           warns)
    roundtos <- fixup(nvars, roundtos,gtxt("round values"),               warns)
    mixtures <- coerce_yesno(unlist(fixup(nvars, mixtures, gtxt("mixture spec"),       warns, numeric=FALSE)))

    # optdesmc()'s own argument defaults are lows=-Inf, highs=Inf (harmless when
    # VARNAMES/LOWS/HIGHS aren't used at all, e.g. analysis-only mode). But if a
    # non-factor, non-mixture variable is given a LOWS/HIGHS without an actual
    # numeric range supplied, those -Inf/Inf defaults flow straight through
    # fixup() unchanged (fixup() only fills in NULL, not -Inf/Inf), so the
    # auto-center calculation below computes (-Inf+Inf)/2 = NaN, and the later
    # range check `centers[[i]]<lows[[i]]` then evaluates to NA, crashing with
    # "missing value where TRUE/FALSE needed" instead of a clear message.
    # LOWS/HIGHS are legitimately irrelevant for factor and mixture variables
    # (AlgDesign ignores/resets them for those), so only require finite values
    # for ordinary continuous variables.
    for (i in seq_len(nvars)) {
        if (!isTRUE(factors[[i]]) && !isTRUE(mixtures[[i]]) &&
            (!is.finite(lows[[i]]) || !is.finite(highs[[i]]))) {
            warns$warn(gtxtf("LOWS and HIGHS must both be specified with numeric values for variable %s", varnames[[i]]), dostop=TRUE)
        }
    }

    # Auto-calculate centers if CENTERS was left blank entirely (fixup() defaults a
    # blank CENTERS to all-0, which is indistinguishable from a real 0 unless we
    # remember up front that the user never supplied it at all)
    for (i in seq_len(nvars)) {
        if (centers_was_null || is.na(centers[[i]])) {
            centers[[i]] <- (lows[[i]] + highs[[i]]) / 2
        }
    }

    # LEVELS=0 is a per-variable "skip / use default" sentinel (no SPSS-side minimum
    # is enforced anymore so partial lists like LEVELS=0 0 3 4 are allowed). 0 (or
    # blank, which fixup() also turns into 0) becomes a default level count; 1 or
    # negative values are not valid level counts and are rejected.
    #
    # The default is normally 2. But for DESIGNTYPE=OPTIMAL with a curvature model
    # (MODEL=quad/cubic/cubics), AlgDesign::optMonteCarlo samples each non-factor,
    # non-mixture variable from only `nLevels` distinct values. At 2 levels, x^2 is
    # an exact linear function of x, so the squared/curvature term is perfectly
    # collinear with the linear term and the resulting information matrix is always
    # singular ("Singular design.") -- regardless of TRIALS/NUMCAND/REPEATS. To avoid
    # that for users who didn't explicitly set LEVELS=, default to enough levels to
    # resolve the requested curvature instead of the flat screening default of 2.
    dtype <- tolower(if (is.null(designtype)) "" else as.character(designtype)[1])
    mdl   <- tolower(if (is.null(model))      "" else as.character(model)[1])
    curvature_default <- if (dtype=="optimal" && mdl %in% c("quad","cubic","cubics")) {
        if (mdl=="quad") 5L else 7L
    } else NA_integer_
    # A user FORMULA with squared/higher powers (I(A^2), A^2, quad(), cubic(),
    # poly()) needs the same: 2 levels make A^2 a copy of the constant/A and
    # the design matrix singular. Only the variables with curvature are raised.
    curv_vars <- character(0)
    if (dtype == "optimal" && !is.null(frml) && nzchar(paste(unlist(frml), collapse=""))) {
        ftxt <- paste(unlist(frml), collapse=" ")
        if (grepl("(quad|cubic|cubicS|poly)\\s*\\(", ftxt)) curv_vars <- as.character(unlist(varnames))
        else for (v in as.character(unlist(varnames)))
            if (grepl(paste0("(^|[^A-Za-z0-9._])", gsub("([.])", "\\\\\\1", v), "\\s*\\^\\s*[2-9]"), ftxt)) curv_vars <- c(curv_vars, v)
    }
    for (i in seq_len(nvars)) {
        if (is.na(curvature_default) && as.character(varnames[[i]]) %in% curv_vars &&
            !isTRUE(factors[[i]]) && !isTRUE(mixtures[[i]])) {
            if (is.null(nlevels[[i]]) || is.na(nlevels[[i]]) || nlevels[[i]] == 0) { nlevels[[i]] <- 5; next }
            if (nlevels[[i]] < 3) warns$warn(gtxtf("Variable %s appears squared (or higher) in FORMULA but has only %d level(s); at least 3 levels (5+ recommended) are needed, otherwise the design is singular.",
                                                  varnames[[i]], nlevels[[i]]), dostop=FALSE)
        }
        if (is.null(nlevels[[i]]) || is.na(nlevels[[i]]) || nlevels[[i]]==0) {
            if (!is.na(curvature_default) && !isTRUE(factors[[i]]) && !isTRUE(mixtures[[i]])) {
                nlevels[[i]] <- curvature_default
            } else {
                nlevels[[i]] <- 2
            }
        } else if (nlevels[[i]] < 2) {
            warns$warn(gtxtf("Number of levels for variable %s must be 0 (skip) or 2 or greater", varnames[[i]]), dostop=TRUE)
        } else if (!is.na(curvature_default) && !isTRUE(factors[[i]]) && !isTRUE(mixtures[[i]]) && nlevels[[i]] < 3) {
            warns$warn(gtxtf("Variable %s has only %d level(s) but MODEL=%s requires curvature; at least 3 levels (5+ recommended) are needed to avoid a singular design",
                              varnames[[i]], nlevels[[i]], toupper(mdl)), dostop=FALSE)
        }
    }

    # Validate ranges
    # (factor and mixture variables may legitimately have no numeric range:
    # skip the check for them instead of failing on NaN/NA comparisons)
    for (i in seq_len(nvars)) {
        if (!is.finite(lows[[i]]) || !is.finite(highs[[i]]) || !is.finite(centers[[i]])) {
            if (isTRUE(factors[[i]]) || isTRUE(mixtures[[i]])) next
        }
        if (isTRUE(lows[[i]]>=highs[[i]] || centers[[i]]<lows[[i]] || centers[[i]]>highs[[i]]) ||
            any(is.na(c(lows[[i]], highs[[i]], centers[[i]]))))
            warns$warn(gtxtf("Invalid low/high/center for variable %s", varnames[[i]]), dostop=TRUE)
    }
    data.frame(var=unlist(varnames), lows, highs, centers, nlevels, roundtos, factors, mixtures)
}

fixup <- function(nvars, item, label, warns, numeric=TRUE) {
    if (is.null(item)) item <- rep(0, nvars)  # Default to 0 if NULL
    if (length(item)==1) item <- rep(item, nvars)
    if (length(item)!=nvars)
        warns$warn(gtxtf("Input has wrong length: %s", label), dostop=TRUE)
    # BUGFIX: LOWS/HIGHS/CENTERS/LEVELS/ROUNDTOS previously had no numeric
    # check at all -- an alphabetic entry (e.g. a typo) silently became NA
    # once something downstream eventually called as.numeric() on it, then
    # crashed later with an opaque "non-numeric argument to binary operator"
    # or "missing value where TRUE/FALSE needed" instead of a clear message
    # naming the field and the bad value. numeric=FALSE (used for the
    # FACTORS/MIXTURES yes/no callers, which also route through fixup() for
    # its length-recycling logic) skips this check entirely.
    if (numeric && !is.numeric(item)) {
        coerced <- suppressWarnings(as.numeric(as.character(item)))
        bad <- is.na(coerced) & !(is.na(item) | as.character(item) %in% c("", "NA"))
        if (any(bad))
            warns$warn(gtxtf("%s must contain only numeric values; found non-numeric entry: %s",
                              label, paste(unique(as.character(item)[bad]), collapse=", ")), dostop=TRUE)
        item <- coerced
    }
    return(item)
}

genfrml <- function(variables, frml, model, constant, factorlist) {
    if (!is.null(frml)) {
        if (substr(frml,1,1)!="~") frml <- paste("~",frml)
        return(frml)
    }
    # MODEL is a free-form Keyword in the syntax (no EnumValue list, so SPSS
    # passes through whatever case the user typed -- e.g. "LINEAR", "Quad").
    # validate() already defensively lowercases its own local copy (see `mdl`)
    # for exactly this reason. This function did NOT, so MODEL=LINEAR (or any
    # non-lowercase variant) fell through to the else-branch below and built
    # an invalid term like "LINEAR(vet)" -- there is no LINEAR() function --
    # instead of the plain additive term every "linear" model should produce.
    model <- tolower(model)
    if (model=="linear") {
        part <- paste(variables, collapse="+")
    } else {
        fvars <- variables[factorlist]
        nfvars<- setdiff(variables, fvars)
        if (length(nfvars)>0) {
            if (model=="cubics") model <- "cubicS"
            part <- paste0(model,"(",paste(nfvars,collapse=","),")")
        } else { part <- NULL }
        if (length(fvars)>0) {
            f <- paste(fvars, collapse="+")
            part <- if (is.null(part)) f else paste(part,f,sep="+")
        }
    }
    if (!constant) part <- paste(part,"-1")
    paste("~", part)
}

buildfactorlist <- function(variables, factors, warns) {
    # Properly coerce factors
    factors <- coerce_yesno(factors)
    cvars <- c(); count <- 1
    for (i in seq_along(variables))
        if (isTRUE(factors[i])) { cvars[count] <- i; count <- count+1 }
    return(cvars)
}

fixtype <- function(alist, warns) {
    isvalid <- all(alist %in% list("yes","true","no","false"))
    if (!isvalid) warns$warn(gtxt("A yes/no keyword has an invalid value"), dostop=TRUE)
    lapply(alist, function(x) x=="yes"||x=="true")
}

setuplocalization <- function(domain) {
    fpath <- Find(file.exists, file.path(.libPaths(), paste0(domain,".R")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
}

StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1,2)>=19)
        spsspkg.StartProcedure(procname, omsid)
    else
        spsspkg.StartProcedure(omsid)
}

gtxt  <- function(...) gettext(..., domain="STATS_DOE_ANALYSIS")
gtxtf <- function(...) gettextf(..., domain="STATS_DOE_ANALYSIS")

Warn <- function(procname, omsid) {
    lcl <- list(procname=procname, omsid=omsid, msglist=list(), msgnum=0)
    lcl <- list2env(lcl)
    lcl$warn <- function(msg=NULL, dostop=FALSE, inproc=FALSE) {
        if (!is.null(msg)) {
            assign("msgnum", lcl$msgnum+1, envir=lcl)
            m <- lcl$msglist; m[[lcl$msgnum]] <- msg
            assign("msglist", m, envir=lcl)
        }
        if (is.null(msg)||dostop) {
            lcl$display(inproc)
            # BUGFIX: stop() previously always used the generic, unhelpful
            # "End of procedure" text, no matter what the actual warning
            # said. The intent was that the real message would already be
            # visible in the Warnings pivot table built by display() just
            # above -- but that table has proven unreliable (it can render
            # with no visible message text, a separate, not-yet-understood
            # rendering issue), while whatever surfaces this stop() call's
            # own text has consistently shown up in the Output Viewer.
            # Passing the real message here means the Viewer shows something
            # useful even when the pivot table itself doesn't.
            if (dostop) stop(if (!is.null(msg)) msg else gtxt("End of procedure"), call.=FALSE)
        }
    }
    lcl$display <- function(inproc=FALSE) {
        if (lcl$msgnum==0) { if (inproc) spsspkg.EndProcedure(); return() }
        if (!inproc) {
            # BUGFIX: clear any stray/still-open procedure state immediately
            # before this StartProcedure() call (not just once, earlier, at
            # the top of the calling function) -- StartProcedure() otherwise
            # silently fails on the first call of a fresh run and this
            # Warnings table falls back to plain print(), which only the
            # syntax/log pane shows, never the Output Viewer.
            tryCatch(spsspkg.EndProcedure(), error=function(e) NULL)
            procok <- tryCatch({ StartProcedure(lcl$procname, lcl$omsid); TRUE },
                               error=function(e) FALSE)
        } else procok <- TRUE
        if (procok) {
            # BUGFIX: the raw spss.BasePivotTable/BasePivotTable.SetCellValue
            # API used here previously produced a table with a title but no
            # visible message text (the "not-yet-understood" rendering issue
            # referenced in the stop() comment above). This is the ONLY place
            # in the whole file that used that raw API directly -- every other
            # table (30+ call sites) uses spsspivottable.Display(), which has
            # proven reliable. Swapped to the same proven helper here so
            # warning text actually renders instead of silently vanishing.
            msg_df <- data.frame(cbind(vapply(lcl$msglist, as.character, character(1))),
                                  row.names=as.character(seq_len(lcl$msgnum)))
            spsspivottable.Display(msg_df,
                title=gtxt("Warnings"),
                collabels=c(gtxt("Message")),
                templateName="DOEWARNINGS", outline=gtxt("Warnings"))
            spsspkg.EndProcedure()
        } else for (i in seq_len(lcl$msgnum)) print(lcl$msglist[[i]])
    }
    return(lcl)
}

# ════════════════════════════════════════════════════════════════════════════
# CODED-FACTOR ANALYSIS ENGINE (2-level factorial, Plackett-Burman,
# definitive screening and response-surface designs)
# ════════════════════════════════════════════════════════════════════════════
# Standard textbook least-squares analysis of designed experiments
# (Montgomery, "Design and Analysis of Experiments"; Myers, Montgomery &
# Anderson-Cook, "Response Surface Methodology"):
#   * numeric factors are coded to -1 (low) / +1 (high) so that effects are
#     comparable across factors and main effects stay interpretable when
#     interactions are present;
#   * a two-level text/categorical factor is coded -1 (first level) / +1;
#     categoricals with more levels use sum-to-zero (effect) coding;
#   * center points are detected and, for two-level designs, tested with a
#     single 0/1 curvature term;
#   * terms aliased with earlier terms are removed and reported;
#   * adjusted (partial) sums of squares, t-tests, VIFs, PRESS-based
#     predicted R-squared, lack-of-fit vs. pure error when replicates exist;
#   * optional hierarchical backward elimination by p-value;
#   * multi-response optimization by the Derringer & Suich (1980)
#     desirability approach.
# Every table, chart and piece of wording here is this extension's own.
# ════════════════════════════════════════════════════════════════════════════

.fa_meta_cols <- c("Reps","Proportion","StdOrder","RunOrder","Block","Blocks",
                   "CenterPt","PtType")

# Split "name=value name2=value2" (spaces or commas between pairs) into a named
# character vector; names are matched case-insensitively later.
.fa_parse_pairs <- function(x) {
    x <- parse_multi_values(x)
    if (length(x) == 0) return(character(0))
    x <- x[grepl("=", x, fixed=TRUE)]
    if (length(x) == 0) return(character(0))
    nm  <- trimws(sub("=.*$", "", x))
    val <- trimws(sub("^[^=]*=", "", x))
    setNames(val, nm)
}

.fa_match_name <- function(nm, choices) {
    i <- match(tolower(nm), tolower(choices))
    if (is.na(i)) NA_character_ else choices[i]
}

# ── Factor coding ───────────────────────────────────────────────────────────
# Returns list(info=list(per factor), ctpt=logical vector of center rows,
#              axial=logical vector, kind="factorial"|"rsm"|"dsd")
fa_build_coding <- function(data, factors, warns, lows=NULL, highs=NULL,
                            lowlevels=NULL, designtype=NULL, categorical=NULL,
                            designmodel=NULL) {
    catforce <- tolower(parse_multi_values(categorical))
    dt0 <- tolower(if (is.null(designtype)) "" else as.character(designtype)[1])
    n <- nrow(data)
    if (!is.null(data$StdOrder)) {
        so <- suppressWarnings(as.numeric(data$StdOrder))
        ord <- if (all(is.finite(so))) order(so) else seq_len(n)
    } else ord <- seq_len(n)
    lowlv <- .fa_parse_pairs(lowlevels)
    lows  <- suppressWarnings(as.numeric(unlist(lows)))
    highs <- suppressWarnings(as.numeric(unlist(highs)))

    # Explicit center/axial marker column (0 = center, 1 = cube, -1 = axial),
    # the convention used both by this extension's own generator and by the
    # most common commercial packages' worksheets.
    cp_col <- NULL
    for (cn in c("CenterPt","PtType")) {
        if (cn %in% names(data)) {
            v <- suppressWarnings(as.numeric(data[[cn]]))
            if (all(is.finite(v) | is.na(v)) && any(is.finite(v))) { cp_col <- v; break }
        }
    }

    info <- list()
    for (i in seq_along(factors)) {
        f <- factors[i]
        x <- data[[f]]
        if (is.null(x)) {
            stop(gtxtf("Factor variable '%s' was not found in the data.", f), call.=FALSE)
        }
        is_num <- is.numeric(x) && !is.factor(x)
        # numeric factors treated as categorical: listed in CATEGORICAL, or a
        # general full factorial with more than two levels
        num_as_cat <- is_num && (tolower(f) %in% catforce ||
            (dt0 == "fullfactorial" && length(unique(x[is.finite(x)])) > 2))
        if (!is_num || num_as_cat) {
            xc   <- trimws(as.character(x))
            xc[xc == ""] <- NA
            lv   <- unique(xc[ord][!is.na(xc[ord])])
            if (num_as_cat) lv <- as.character(sort(unique(x[is.finite(x)])))
            if (length(lv) < 2)
                stop(gtxtf("Factor '%s' has fewer than two distinct levels in the data.", f), call.=FALSE)
            ll <- lowlv[tolower(names(lowlv)) == tolower(f)]
            if (length(ll)) {
                m <- match(tolower(ll[1]), tolower(lv))
                if (is.na(m))
                    warns$warn(gtxtf("LOWLEVELS: level '%s' does not occur in factor '%s'; first-appearing level used instead.", ll[1], f), dostop=FALSE)
                else lv <- c(lv[m], lv[-m])
            }
            info[[f]] <- list(name=f, type="categorical", levels=lv, nlev=length(lv), numeric_levels=num_as_cat)
        } else {
            u <- sort(unique(x[is.finite(x)]))
            if (length(u) < 2)
                stop(gtxtf("Factor '%s' has fewer than two distinct levels in the data.", f), call.=FALSE)
            lo <- NA; hi <- NA
            if (length(lows) >= i && length(highs) >= i &&
                is.finite(lows[i]) && is.finite(highs[i]) && highs[i] != lows[i]) {
                lo <- lows[i]; hi <- highs[i]
            } else if (!is.null(cp_col)) {
                # the cube points define the -1/+1 levels
                cube <- x[is.finite(cp_col) & cp_col == 1 & is.finite(x)]
                if (length(unique(cube)) >= 2) { lo <- min(cube); hi <- max(cube) }
            }
            if (!is.finite(lo) || !is.finite(hi)) {
                if (length(u) == 5) { lo <- u[2]; hi <- u[4] }        # central composite
                else { lo <- u[1]; hi <- u[length(u)] }
            }
            info[[f]] <- list(name=f, type="numeric", low=lo, high=hi,
                              mid=(lo+hi)/2, half=(hi-lo)/2, nlev=length(u))
        }
    }

    # Center points: explicit marker, else all numeric factors at their mid.
    num_f <- Filter(function(z) z$type == "numeric", info)
    if (!is.null(cp_col)) {
        ctpt  <- is.finite(cp_col) & cp_col == 0
        axial <- is.finite(cp_col) & cp_col == -1
    } else if (length(num_f)) {
        ctpt <- rep(TRUE, n)
        for (z in num_f) {
            x <- data[[z$name]]
            ctpt <- ctpt & is.finite(x) & abs(x - z$mid) <= 1e-9 * max(1, abs(z$mid))
        }
        axial <- rep(FALSE, n)
        for (z in num_f) {
            cx <- (data[[z$name]] - z$mid) / z$half
            axial <- axial | (is.finite(cx) & abs(cx) > 1 + 1e-9)
        }
    } else {
        ctpt <- rep(FALSE, n); axial <- rep(FALSE, n)
    }

    # Design kind decides the default model.
    dt <- tolower(if (is.null(designtype)) "" else as.character(designtype))
    nonctr <- !ctpt & !axial
    two_level <- all(vapply(info, function(z) {
        if (z$type == "categorical") return(z$nlev == 2)
        x <- data[[z$name]][nonctr]
        length(unique(x[is.finite(x)])) <= 2
    }, logical(1)))
    multi_cat <- any(vapply(info, function(z) z$type == "categorical" && z$nlev > 2, logical(1)))
    dm <- tolower(if (is.null(designmodel)) "" else as.character(unlist(designmodel))[1])
    kind <- if (dt == "dsd") "dsd"
            else if (dt %in% c("ccd","rsm","boxbehnken")) "rsm"
            else if (dt %in% c("optimal","lhs") && dm %in% c("linear","item_313_a")) "linear"
            else if (two_level) "factorial"
            else if (multi_cat && !any(vapply(info, function(z) z$type == "numeric" && z$nlev >= 3, logical(1)))) "general"
            else "rsm"
    list(info=info, ctpt=ctpt, axial=axial, kind=kind)
}

# Build the coded design frame (one column per factor, internal names X1..Xk)
fa_code_data <- function(data, coding) {
    info <- coding$info
    out  <- data.frame(row.names=seq_len(nrow(data)))
    for (i in seq_along(info)) {
        z  <- info[[i]]
        nm <- paste0("X", i)
        x  <- data[[z$name]]
        if (z$type == "numeric") {
            out[[nm]] <- (as.numeric(x) - z$mid) / z$half
        } else {
            xc <- trimws(as.character(x)); xc[xc == ""] <- NA
            if (z$nlev == 2) {
                out[[nm]] <- ifelse(is.na(xc), NA_real_, ifelse(xc == z$levels[1], -1, 1))
            } else {
                fx <- factor(xc, levels=z$levels)
                contrasts(fx) <- contr.sum(z$nlev)
                out[[nm]] <- fx
            }
        }
    }
    out$CtPt <- as.numeric(coding$ctpt)
    out
}

# Letter codes A, B, C, ... (skipping I, the conventional identity symbol)
fa_letters <- function(k) {
    L <- setdiff(LETTERS, "I")
    if (k <= length(L)) L[seq_len(k)] else paste0("F", seq_len(k))
}

# ── Model terms ─────────────────────────────────────────────────────────────
# A term is an integer vector of factor indices (repeats = powers), or the
# special string "CTPT". Term keys: "1", "1:2", "1:1", "CTPT".
.fa_term_key   <- function(t) if (identical(t, "CTPT")) "CTPT" else paste(sort(t), collapse=":")
.fa_term_order <- function(t) if (identical(t, "CTPT")) 99L else length(t)
.fa_is_square  <- function(t) !identical(t, "CTPT") && length(t) == 2 && t[1] == t[2]

fa_default_terms <- function(coding, include_ctpt=TRUE) {
    k    <- length(coding$info)
    num  <- which(vapply(coding$info, function(z) z$type == "numeric", logical(1)))
    lin  <- as.list(seq_len(k))
    tw   <- if (k >= 2) combn(k, 2, simplify=FALSE) else list()
    sq   <- lapply(num[vapply(coding$info[num], function(z) z$nlev >= 3, logical(1))],
                   function(i) c(i, i))
    terms <- switch(coding$kind,
        factorial = c(lin, tw),
        general   = c(lin, tw),
        linear    = lin,
        dsd       = c(lin, sq),
        rsm       = c(lin, sq, tw),
        c(lin, sq, tw))
    if (coding$kind == "factorial" && include_ctpt && any(coding$ctpt) && length(num) > 0)
        terms <- c(terms, list("CTPT"))
    terms
}

# Parse the TERMS text. Tokens are whitespace separated; a token is one or
# more factor references joined by '*' (variable name or letter code), or a
# keyword: LINEAR, 2WAY, 3WAY, FULL, SQUARES, QUADRATIC, CTPT. A block may be
# prefixed "response:" and blocks are separated by ';'. Returns a named list
# of term lists ("" = applies to every response without its own block).
fa_parse_terms_spec <- function(spec, coding, warns) {
    if (is.null(spec)) return(list())
    spec <- paste(unlist(spec), collapse=" ")
    if (!nzchar(trimws(spec))) return(list())
    k      <- length(coding$info)
    fnames <- names(coding$info)
    lets   <- fa_letters(k)
    num    <- which(vapply(coding$info, function(z) z$type == "numeric", logical(1)))
    blocks <- strsplit(spec, ";", fixed=TRUE)[[1]]
    out    <- list()
    for (b in blocks) {
        b <- trimws(b); if (!nzchar(b)) next
        resp <- ".all"
        if (grepl(":", b, fixed=TRUE)) {
            resp <- trimws(sub(":.*$", "", b)); b <- trimws(sub("^[^:]*:", "", b))
        }
        toks  <- strsplit(b, "[[:space:],]+")[[1]]; toks <- toks[nzchar(toks)]
        terms <- list()
        for (tk in toks) {
            up <- toupper(tk)
            if (up %in% c("CTPT","CENTERPT","CENTER","CURVATURE")) { terms <- c(terms, list("CTPT")); next }
            if (up %in% c("LINEAR","MAIN","MAINEFFECTS")) { terms <- c(terms, as.list(seq_len(k))); next }
            if (up %in% c("2WAY","TWOWAY")) { if (k >= 2) terms <- c(terms, combn(k, 2, simplify=FALSE)); next }
            if (up %in% c("3WAY","THREEWAY")) { if (k >= 3) terms <- c(terms, combn(k, 3, simplify=FALSE)); next }
            if (up == "FULL") { for (o in seq_len(k)) terms <- c(terms, combn(k, o, simplify=FALSE)); next }
            if (up %in% c("SQUARES","SQUARE")) { terms <- c(terms, lapply(num, function(i) c(i,i))); next }
            if (up %in% c("QUADRATIC","FULLQUAD","QUAD")) {
                terms <- c(terms, as.list(seq_len(k)), lapply(num, function(i) c(i,i)))
                if (k >= 2) terms <- c(terms, combn(k, 2, simplify=FALSE)); next
            }
            parts <- strsplit(tk, "*", fixed=TRUE)[[1]]
            idx <- integer(0)
            for (p in parts) {
                j <- match(tolower(p), tolower(fnames))
                if (is.na(j)) j <- match(toupper(p), lets)
                if (is.na(j) && grepl("^[A-Za-z]{2,}$", p) && all(toupper(strsplit(p, "")[[1]]) %in% lets)) {
                    # compact letter form such as AB or AAB
                    j <- match(toupper(strsplit(p, "")[[1]]), lets)
                }
                if (any(is.na(j))) {
                    warns$warn(gtxtf("TERMS: '%s' is not a factor name or letter code and was ignored.", p), dostop=FALSE)
                    idx <- NULL; break
                }
                idx <- c(idx, j)
            }
            if (is.null(idx) || !length(idx)) next
            idx <- sort(idx)
            tab <- table(idx)
            if (any(tab > 2) || (any(tab == 2) && length(idx) > 2)) {
                warns$warn(gtxtf("TERMS: '%s' is not supported (only squares X*X and cross products of distinct factors).", tk), dostop=FALSE); next
            }
            if (any(tab == 2) && coding$info[[idx[1]]]$type != "numeric") {
                warns$warn(gtxtf("TERMS: a square term needs a numeric factor ('%s' ignored).", tk), dostop=FALSE); next
            }
            terms <- c(terms, list(idx))
        }
        # de-duplicate, keep first occurrence
        keys  <- vapply(terms, .fa_term_key, character(1))
        terms <- terms[!duplicated(keys)]
        out[[resp]] <- terms
    }
    out
}

# Order terms the conventional way: linear, squares, 2-way, 3-way, ..., CtPt;
# within a group by factor index (A, B, AB, AC, ...).
fa_sort_terms <- function(terms) {
    if (!length(terms)) return(terms)
    grp <- vapply(terms, function(t) {
        if (identical(t, "CTPT")) return(100)
        if (length(t) == 1) return(1)
        if (.fa_is_square(t)) return(2)
        length(t) + 1
    }, numeric(1))
    key <- vapply(terms, function(t) if (identical(t, "CTPT")) "" else
        paste(sprintf("%03d", sort(t)), collapse=""), character(1))
    terms[order(grp, key)]
}

fa_term_label <- function(t, coding, letters=FALSE) {
    if (identical(t, "CTPT")) return(gtxt("Center Point"))
    nm <- if (letters) fa_letters(length(coding$info)) else names(coding$info)
    paste(nm[t], collapse=if (letters) "" else "*")
}

# Hierarchy: a term's "parents" are the lower-order terms it contains.
.fa_contains <- function(big, small) {
    if (identical(big, "CTPT") || identical(small, "CTPT")) return(FALSE)
    if (length(small) >= length(big)) return(FALSE)
    tb <- table(big); ts <- table(small)
    all(names(ts) %in% names(tb)) && all(ts <= tb[names(ts)])
}


# ── Model matrix for a list of terms ─────────────────────────────────────────
# Returns list(X=matrix incl. intercept, assign=integer term index per column
# (0 = intercept, -1 = blocks), colterm=term keys)
fa_model_matrix <- function(cd, terms, blockcol=NULL) {
    n    <- nrow(cd)
    cols <- list(`(Intercept)`=rep(1, n))
    asg  <- 0L
    cnm  <- "(Intercept)"
    if (!is.null(blockcol)) {
        bf <- factor(blockcol)
        if (nlevels(bf) > 1) {
            cm <- contr.sum(nlevels(bf))
            B  <- cm[as.integer(bf), , drop=FALSE]
            for (j in seq_len(ncol(B))) {
                cols[[paste0("Blk", j)]] <- B[, j]; asg <- c(asg, -1L)
                cnm <- c(cnm, paste0("Blk", j))
            }
        }
    }
    for (ti in seq_along(terms)) {
        t <- terms[[ti]]
        if (identical(t, "CTPT")) {
            cols[["CtPt"]] <- cd$CtPt; asg <- c(asg, ti); cnm <- c(cnm, "CtPt"); next
        }
        # product of the (possibly multi-column) pieces
        piece <- matrix(1, n, 1); pnm <- ""
        for (f in t) {
            v <- cd[[paste0("X", f)]]
            if (is.factor(v)) {
                M <- contrasts(v)[as.integer(v), , drop=FALSE]
                M[is.na(v), ] <- NA
                newp <- NULL; newn <- NULL
                for (a in seq_len(ncol(piece))) for (b in seq_len(ncol(M))) {
                    newp <- cbind(newp, piece[, a] * M[, b])
                    newn <- c(newn, paste0(pnm[a], "X", f, "_", b))
                }
                piece <- newp; pnm <- newn
            } else {
                piece <- piece * v
                pnm   <- paste0(pnm, "X", f, ".")
            }
        }
        for (a in seq_len(ncol(piece))) {
            cn <- paste0("T", ti, "_", a)
            cols[[cn]] <- piece[, a]; asg <- c(asg, ti); cnm <- c(cnm, cn)
        }
    }
    X <- do.call(cbind, cols); colnames(X) <- cnm
    list(X=X, assign=asg)
}

.fa_sse <- function(X, y) {
    if (ncol(X) == 0) return(sum(y^2))
    f <- lm.fit(X, y)
    sum(f$residuals^2)
}

# Fit one response. Removes terms that are aliased (linearly dependent on
# earlier terms), reporting them. Returns an "fa" object.
fa_fit <- function(data, cd, coding, resp, terms, warns, alpha=0.05,
                   conf=0.95, blockcol=NULL, report_alias=TRUE, trans=NULL) {
    y   <- suppressWarnings(as.numeric(data[[resp]]))
    lambda <- if (!is.null(trans)) trans$lambda else NA_real_
    y_orig <- y
    if (is.finite(lambda)) {
        if (any(y[is.finite(y)] <= 0)) stop(gtxtf("%s: a Box-Cox transformation needs every response value > 0.", resp), call.=FALSE)
        y <- if (abs(lambda) < 1e-12) log(y) else y^lambda
    }
    fac <- cd[, paste0("X", seq_along(coding$info)), drop=FALSE]
    ok  <- is.finite(y) & stats::complete.cases(fac)
    if (!is.null(blockcol)) ok <- ok & !is.na(blockcol)
    if (sum(!ok) > 0)
        warns$warn(gtxtf("%s: %d row(s) with a missing response or factor value were excluded.", resp, sum(!ok)), dostop=FALSE)
    cdo <- cd[ok, , drop=FALSE]; yo <- y[ok]
    blk <- if (is.null(blockcol)) NULL else blockcol[ok]
    terms <- fa_sort_terms(terms)
    # drop CtPt if it cannot be estimated (no center rows, or all rows centers)
    if (any(vapply(terms, identical, logical(1), "CTPT"))) {
        s <- sum(cdo$CtPt)
        if (s == 0 || s == nrow(cdo)) terms <- Filter(function(t) !identical(t, "CTPT"), terms)
    }
    removed <- character(0)
    repeat {
        mm <- fa_model_matrix(cdo, terms, blk)
        qrX <- qr(mm$X, tol=1e-7)
        if (qrX$rank == ncol(mm$X)) break
        # first column (in model order) that is dependent on the ones before it
        bad <- NA
        for (j in 2:ncol(mm$X)) {
            if (qr(mm$X[, 1:j, drop=FALSE], tol=1e-7)$rank < j) { bad <- j; break }
        }
        ti <- mm$assign[bad]
        if (ti <= 0) { warns$warn(gtxtf("%s: the block variable is confounded with the design and was dropped.", resp), dostop=FALSE); blk <- NULL; next }
        removed <- c(removed, fa_term_label(terms[[ti]], coding))
        terms <- terms[-ti]
    }
    if (length(removed) && report_alias)
        warns$warn(gtxtf("%s: these terms cannot be estimated separately (aliased with terms already in the model) and were removed: %s",
                         resp, paste(removed, collapse=", ")), dostop=FALSE)
    n  <- nrow(mm$X); p <- ncol(mm$X)
    fit <- lm.fit(mm$X, yo)
    b   <- fit$coefficients
    res <- fit$residuals
    dfe <- n - p
    sse <- sum(res^2)
    sst <- sum((yo - mean(yo))^2)
    mse <- if (dfe > 0) sse / dfe else NA_real_
    XtXi <- chol2inv(qr.R(qr(mm$X)))
    se   <- if (dfe > 0) sqrt(diag(XtXi) * mse) else rep(NA_real_, p)
    tval <- b / se
    pval <- if (dfe > 0) 2 * pt(-abs(tval), dfe) else rep(NA_real_, p)
    h    <- rowSums((mm$X %*% XtXi) * mm$X)
    press <- if (all(h < 1 - 1e-10)) sum((res / (1 - h))^2) else NA_real_
    # VIF per column: diagonal of the inverse correlation matrix of predictors
    vif <- rep(NA_real_, p)
    if (p > 2) {
        Z <- mm$X[, -1, drop=FALSE]
        R <- suppressWarnings(cor(Z))
        vi <- tryCatch(diag(solve(R)), error=function(e) rep(NA_real_, ncol(Z)))
        vif[-1] <- vi
    } else if (p == 2) vif[2] <- 1
    obj <- list(resp=resp, coding=coding, terms=terms, removed=removed,
                X=mm$X, assign=mm$assign, y=yo, rows=which(ok), cd=cdo, blk=blk,
                coef=b, se=se, t=tval, p=pval, df_error=dfe, sse=sse, sst=sst,
                mse=mse, s=sqrt(mse), fitted=as.vector(mm$X %*% b), resid=res,
                hat=h, XtXi=XtXi, press=press, vif=vif, n=n, alpha=alpha, conf=conf,
                lambda=lambda, y_orig=y_orig[ok])
    class(obj) <- "fa"
    obj
}

# Back-transform from the Box-Cox scale to the original response scale
fa_untransform <- function(fo, v) {
    if (is.null(fo$lambda) || !is.finite(fo$lambda)) return(v)
    if (abs(fo$lambda) < 1e-12) exp(v) else sign(v) * abs(v)^(1 / fo$lambda)
}
fa_predict_orig <- function(fo, newcd) fa_untransform(fo, fa_predict(fo, newcd))

# Box-Cox profile likelihood for the current model matrix
fa_boxcox <- function(y, X) {
    if (any(y <= 0)) return(NULL)
    gm <- exp(mean(log(y)))
    lam <- seq(-2, 2, by=0.01)
    ll <- vapply(lam, function(l) {
        w <- if (abs(l) < 1e-12) gm * log(y) else (y^l - 1) / (l * gm^(l - 1))
        -length(y) / 2 * log(.fa_sse(X, w) / length(y))
    }, numeric(1))
    best <- lam[which.max(ll)]
    ci <- range(lam[ll >= max(ll) - qchisq(0.95, 1) / 2])
    list(lambda=best + 0, rounded=round(best * 2) / 2 + 0, lower=ci[1], upper=ci[2])
}

# Prediction at coded settings (data.frame with X1..Xk and CtPt)
fa_predict <- function(fo, newcd, se=FALSE) {
    mm <- fa_model_matrix(newcd, fo$terms, NULL)
    X  <- mm$X
    if (!is.null(fo$blk) && any(fo$assign == -1)) {
        # predictions average over blocks (sum-to-zero coding -> block cols 0)
        nb <- sum(fo$assign == -1)
        X  <- cbind(X[, 1, drop=FALSE], matrix(0, nrow(X), nb), X[, -1, drop=FALSE])
    }
    fit <- as.vector(X %*% fo$coef)
    if (!se) return(fit)
    sef <- sqrt(pmax(0, rowSums((X %*% fo$XtXi) * X))) * fo$s
    list(fit=fit, se=sef)
}

# Term selection by p-value: "backward", "forward" or "stepwise" (forward
# with a removal check after every entry). Hierarchy is kept: a term is only
# removed when no remaining interaction contains it, and entering an
# interaction also enters any missing lower-order terms. The center-point and
# block terms are never removed.
fa_select <- function(data, cd, coding, resp, terms, warns, method="backward",
                      alpha_enter=0.15, alpha_remove=0.15, alpha=0.05, conf=0.95,
                      blockcol=NULL, hierarchical=TRUE, report_alias=FALSE, trans=NULL) {
    method <- tolower(method)
    steps <- data.frame(Step=integer(0), Action=character(0), Term=character(0), PValue=numeric(0),
                        stringsAsFactors=FALSE)
    lab <- function(t) fa_term_label(t, coding)
    keyv <- function(tt) vapply(tt, .fa_term_key, character(1))
    parents_of <- function(t) {
        if (identical(t, "CTPT") || length(t) < 2) return(list())
        out <- list()
        for (o in seq_len(length(t) - 1)) {
            cmb <- unique(lapply(combn(length(t), o, simplify=FALSE), function(ix) sort(t[ix])))
            out <- c(out, cmb)
        }
        out
    }
    fitm <- function(tt, rep=FALSE) fa_fit(data, cd, coding, resp, tt, warns, alpha, conf, blockcol,
                                           report_alias=rep, trans=trans)
    pool <- fa_sort_terms(terms)
    forced <- Filter(function(t) identical(t, "CTPT"), pool)
    if (method == "backward") {
        fo <- fitm(pool, report_alias)
    } else {
        fo <- fitm(forced, FALSE)
    }
    step <- 0; guard <- 0
    try_remove <- function(fo) {
        if (fo$df_error < 1) return(NULL)
        tt <- fo$terms
        cand <- which(vapply(seq_along(tt), function(i) {
            t <- tt[[i]]
            if (identical(t, "CTPT")) return(FALSE)
            if (hierarchical && any(vapply(tt[-i], .fa_contains, logical(1), small=t))) return(FALSE)
            TRUE
        }, logical(1)))
        if (!length(cand)) return(NULL)
        pv <- vapply(cand, function(i) fa_term_test(fo, i)$p, numeric(1))
        if (all(is.na(pv)) || max(pv, na.rm=TRUE) <= alpha_remove) return(NULL)
        w <- cand[which.max(pv)]
        list(i=w, p=max(pv, na.rm=TRUE))
    }
    if (method == "backward" && fo$df_error < 1)
        warns$warn(gtxtf("%s: term selection needs at least one error degree of freedom; the starting model is saturated, so backward elimination could not start. Use forward or stepwise selection, or a smaller starting model.", resp), dostop=FALSE)
    repeat {
        guard <- guard + 1; if (guard > 200) break
        if (method == "backward") {
            r <- try_remove(fo); if (is.null(r)) break
            step <- step + 1
            steps[step, ] <- list(step, gtxt("Removed"), lab(fo$terms[[r$i]]), r$p)
            fo <- fitm(fo$terms[-r$i])
            next
        }
        # forward entry
        inmod <- keyv(fo$terms)
        best <- NULL
        for (t in pool) {
            if (identical(t, "CTPT") || .fa_term_key(t) %in% inmod) next
            add <- c(list(t), if (hierarchical) Filter(function(q) !(.fa_term_key(q) %in% inmod), parents_of(t)) else list())
            ft <- tryCatch(suppressWarnings(fitm(c(fo$terms, add))), error=function(e) NULL)
            if (is.null(ft) || ft$df_error < 1) next
            j <- which(keyv(ft$terms) == .fa_term_key(t))
            if (!length(j)) next
            pv <- fa_term_test(ft, j)$p
            if (is.finite(pv) && pv < alpha_enter && (is.null(best) || pv < best$p)) best <- list(t=t, add=add, p=pv)
        }
        if (is.null(best)) break
        step <- step + 1
        steps[step, ] <- list(step, gtxt("Entered"), lab(best$t), best$p)
        fo <- fitm(c(fo$terms, best$add))
        if (method == "stepwise") {
            r <- try_remove(fo)
            if (!is.null(r) && .fa_term_key(fo$terms[[r$i]]) != .fa_term_key(best$t)) {
                step <- step + 1
                steps[step, ] <- list(step, gtxt("Removed"), lab(fo$terms[[r$i]]), r$p)
                fo <- fitm(fo$terms[-r$i])
            }
        }
    }
    fo$steps <- steps
    fo$selection <- method
    fo
}

fa_backward <- function(data, cd, coding, resp, terms, warns, alpha_remove=0.10,
                        alpha=0.05, conf=0.95, blockcol=NULL, hierarchical=TRUE,
                        force=character(0), report_alias=FALSE) {
    fa_select(data, cd, coding, resp, terms, warns, method="backward", alpha_remove=alpha_remove,
              alpha=alpha, conf=conf, blockcol=blockcol, hierarchical=hierarchical, report_alias=report_alias)
}

# Partial F test for a set of term indices (adjusted SS).
fa_term_test <- function(fo, term_idx) {
    cols <- which(fo$assign %in% term_idx)
    df   <- length(cols)
    Xr   <- fo$X[, -cols, drop=FALSE]
    ss   <- .fa_sse(Xr, fo$y) - fo$sse
    if (fo$df_error > 0 && df > 0) {
        f <- (ss / df) / fo$mse
        p <- pf(f, df, fo$df_error, lower.tail=FALSE)
    } else { f <- NA_real_; p <- NA_real_ }
    list(df=df, ss=ss, ms=if (df > 0) ss / df else NA_real_, f=f, p=p)
}

# Pure error from replicated settings (identical coded factor settings and
# block); returns list(df, ss) or NULL.
fa_pure_error <- function(fo) {
    fx  <- fo$cd[, paste0("X", seq_along(fo$coding$info)), drop=FALSE]
    fx  <- as.data.frame(lapply(fx, function(v) if (is.factor(v)) as.integer(v) else round(v, 8)))
    fx$CtPt <- fo$cd$CtPt
    key <- apply(fx, 1, paste, collapse="|")
    if (!is.null(fo$blk)) key <- paste(key, fo$blk)
    g  <- split(fo$y, key)
    df <- sum(vapply(g, function(v) length(v) - 1, numeric(1)))
    if (df <= 0) return(NULL)
    ss <- sum(vapply(g, function(v) sum((v - mean(v))^2), numeric(1)))
    list(df=df, ss=ss)
}

# ── Tables ───────────────────────────────────────────────────────────────────
.fa_r <- function(x, d=4) ifelse(is.na(x), NA, signif(x, max(d, 1) + 2))
.fa_fmt <- function(x, digits=4) {
    out <- ifelse(is.na(x), "", formatC(x, digits=digits, format="f"))
    out
}
.fa_pfmt <- function(p) ifelse(is.na(p), "", ifelse(p < 0.0005, "0.000", formatC(p, digits=3, format="f")))

fa_anova_df <- function(fo) {
    tt <- fo$terms; coding <- fo$coding
    rows <- list()
    add <- function(src, df, ss, f=NA, p=NA) {
        rows[[length(rows) + 1]] <<- data.frame(Source=src, DF=df, AdjSS=ss,
            AdjMS=if (df > 0) ss / df else NA, F=f, P=p, stringsAsFactors=FALSE)
    }
    ssm <- fo$sst - fo$sse
    dfm <- ncol(fo$X) - 1
    fm  <- if (fo$df_error > 0 && dfm > 0) (ssm / dfm) / fo$mse else NA
    add(gtxt("Model"), dfm, ssm, fm, if (!is.na(fm)) pf(fm, dfm, fo$df_error, lower.tail=FALSE) else NA)
    if (any(fo$assign == -1)) {
        cols <- which(fo$assign == -1)
        ss <- .fa_sse(fo$X[, -cols, drop=FALSE], fo$y) - fo$sse; df <- length(cols)
        f  <- if (fo$df_error > 0) (ss/df)/fo$mse else NA
        add(gtxt("  Blocks"), df, ss, f, if (!is.na(f)) pf(f, df, fo$df_error, lower.tail=FALSE) else NA)
    }
    grp_of <- function(t) {
        if (identical(t, "CTPT")) return("ctpt")
        if (length(t) == 1) return("lin")
        if (.fa_is_square(t)) return("sq")
        paste0("w", length(t))
    }
    groups <- vapply(tt, grp_of, character(1))
    glabel <- function(g) switch(g, lin=gtxt("  Linear"), sq=gtxt("  Square"),
        ctpt=gtxt("  Curvature"),
        gtxtf("  %d-Way Interactions", as.integer(sub("w", "", g))))
    for (g in unique(groups[order(match(groups, c("lin","sq", paste0("w", 2:30), "ctpt")))])) {
        idx <- which(groups == g)
        if (g != "ctpt") {
            tst <- fa_term_test(fo, idx)
            add(glabel(g), tst$df, tst$ss, tst$f, tst$p)
            for (i in idx) {
                ti <- fa_term_test(fo, i)
                add(paste0("    ", fa_term_label(tt[[i]], coding)), ti$df, ti$ss, ti$f, ti$p)
            }
        } else {
            ti <- fa_term_test(fo, idx)
            add(glabel(g), ti$df, ti$ss, ti$f, ti$p)
        }
    }
    add(gtxt("Error"), fo$df_error, fo$sse)
    pe <- fa_pure_error(fo)
    if (!is.null(pe) && fo$df_error - pe$df > 0) {
        lof_df <- fo$df_error - pe$df; lof_ss <- fo$sse - pe$ss
        f <- (lof_ss / lof_df) / (pe$ss / pe$df)
        add(gtxt("  Lack-of-Fit"), lof_df, lof_ss, f,
            if (pe$ss > 0) pf(f, lof_df, pe$df, lower.tail=FALSE) else NA)
        add(gtxt("  Pure Error"), pe$df, pe$ss)
    }
    add(gtxt("Total"), fo$n - 1, fo$sst)
    out <- do.call(rbind, rows)
    out$AdjMS[out$Source == gtxt("Total")] <- NA
    out
}

fa_coef_df <- function(fo, factorial_effects=TRUE) {
    coding <- fo$coding
    lab <- character(length(fo$coef)); eff <- rep(NA_real_, length(fo$coef))
    bl <- 0
    for (j in seq_along(fo$coef)) {
        a <- fo$assign[j]
        if (a == 0) { lab[j] <- gtxt("Constant"); next }
        if (a == -1) { bl <- bl + 1; lab[j] <- gtxtf("Block %d", bl); next }
        t <- fo$terms[[a]]
        base <- fa_term_label(t, coding)
        ncol_t <- sum(fo$assign == a)
        if (ncol_t > 1) {
            k <- sum(fo$assign[seq_len(j)] == a)
            base <- paste0(base, " (", k, ")")
        }
        lab[j] <- base
        if (factorial_effects && !identical(t, "CTPT") && !.fa_is_square(t) && ncol_t == 1)
            eff[j] <- 2 * fo$coef[j]
    }
    data.frame(Term=lab, Effect=eff, Coef=fo$coef, SECoef=fo$se, T=fo$t, P=fo$p,
               VIF=fo$vif, stringsAsFactors=FALSE, row.names=NULL)
}

fa_summary_df <- function(fo) {
    r2   <- if (fo$sst > 0) 1 - fo$sse / fo$sst else NA
    p    <- ncol(fo$X)
    adj  <- if (fo$df_error > 0 && fo$sst > 0) 1 - (fo$sse / fo$df_error) / (fo$sst / (fo$n - 1)) else NA
    pred <- if (!is.na(fo$press) && fo$sst > 0) max(0, 1 - fo$press / fo$sst) else NA
    data.frame(S=fo$s, R2=r2, R2adj=adj, R2pred=pred, PRESS=fo$press)
}

# Uncoded (natural-unit) coefficients. Numeric coded factor x_c = (X - m)/h
# is expanded symbolically; categorical factors are fixed at each level
# combination, giving one equation per combination.
fa_uncoded <- function(fo) {
    coding <- fo$coding; info <- coding$info
    cat_idx <- which(vapply(info, function(z) z$type == "categorical", logical(1)))
    combos <- if (length(cat_idx)) expand.grid(lapply(info[cat_idx], function(z) z$levels),
                                               stringsAsFactors=FALSE) else data.frame(dummy=1)
    # polynomial = named numeric vector; names are monomials "Var1#Var2" ("1" = constant)
    padd <- function(P, key, val) { if (is.na(P[key])) P[key] <- 0; P[key] <- P[key] + val; P }
    res <- list()
    for (ci in seq_len(nrow(combos))) {
        P <- c(`1`=0)
        for (j in seq_along(fo$coef)) {
            a <- fo$assign[j]; b <- unname(fo$coef[j])
            if (a == -1) next
            if (a == 0) { P <- padd(P, "1", b); next }
            t <- fo$terms[[a]]
            if (identical(t, "CTPT")) { P <- padd(P, "CTPT", b); next }
            cur <- c(`1`=b)
            colk <- sum(fo$assign[seq_len(j)] == a)
            for (f in t) {
                z <- info[[f]]
                if (z$type == "numeric") {
                    nxt <- numeric(0)
                    for (k in names(cur)) {
                        parts <- if (k == "1") character(0) else strsplit(k, "#", fixed=TRUE)[[1]]
                        m1 <- paste(sort(c(parts, z$name)), collapse="#")
                        nxt <- padd(nxt, m1, cur[[k]] / z$half)
                        nxt <- padd(nxt, k,  -cur[[k]] * z$mid / z$half)
                    }
                    cur <- nxt
                } else {
                    lvl <- combos[ci, match(f, cat_idx)]
                    v <- if (z$nlev == 2) { if (lvl == z$levels[1]) -1 else 1 }
                         else contr.sum(z$nlev)[match(lvl, z$levels), colk]
                    cur <- cur * v
                }
            }
            for (k in names(cur)) P <- padd(P, k, cur[[k]])
        }
        res[[ci]] <- P
    }
    keys <- unique(unlist(lapply(res, names)))
    deg  <- vapply(keys, function(k) if (k == "1") 0 else if (k == "CTPT") 99 else
        length(strsplit(k, "#", fixed=TRUE)[[1]]), numeric(1))
    keys <- keys[order(deg, keys)]
    lab <- vapply(keys, function(k) {
        if (k == "1") return(gtxt("Constant"))
        if (k == "CTPT") return(gtxt("Center Point"))
        parts <- strsplit(k, "#", fixed=TRUE)[[1]]
        tb <- table(factor(parts, levels=unique(parts)))
        paste(ifelse(tb > 1, paste0(names(tb), "^", tb), names(tb)), collapse="*")
    }, character(1))
    out <- data.frame(Term=unname(lab), stringsAsFactors=FALSE)
    for (ci in seq_len(nrow(combos))) {
        cn <- if (length(cat_idx)) paste(paste0(names(info)[cat_idx], "=", unlist(combos[ci, ])), collapse=", ")
              else gtxt("Coefficient")
        out[[cn]] <- vapply(keys, function(k) { v <- res[[ci]][k]; if (is.na(v)) 0 else unname(v) }, numeric(1))
    }
    rownames(out) <- NULL
    out
}

# Alias structure for a two-level (fractional) design: for each model term,
# the other interactions (up to maxorder) whose +-1 columns are identical or
# exact negatives on the non-center runs.
fa_alias <- function(fo, maxorder=4) {
    coding <- fo$coding; k <- length(coding$info)
    if (coding$kind != "factorial") return(NULL)
    if (any(vapply(coding$info, function(z) z$type == "categorical" && z$nlev > 2, logical(1)))) return(NULL)
    cube <- fo$cd$CtPt == 0
    F <- as.matrix(fo$cd[cube, paste0("X", seq_len(k)), drop=FALSE])
    if (nrow(F) >= 2^k) return(NULL)   # full factorial: no aliasing
    lets <- fa_letters(k)
    allterms <- list()
    for (o in seq_len(min(k, maxorder))) allterms <- c(allterms, combn(k, o, simplify=FALSE))
    colv <- lapply(allterms, function(t) apply(F[, t, drop=FALSE], 1, prod))
    labs <- vapply(allterms, function(t) paste(lets[t], collapse=""), character(1))
    ident <- rep(1, nrow(F))
    defining <- labs[vapply(colv, function(v) isTRUE(all.equal(v, ident)) || isTRUE(all.equal(v, -ident)), logical(1))]
    out <- list()
    for (t in fo$terms) {
        if (identical(t, "CTPT") || .fa_is_square(t)) next
        v <- apply(F[, t, drop=FALSE], 1, prod)
        lab <- paste(lets[t], collapse="")
        al <- character(0)
        for (i in seq_along(allterms)) {
            if (labs[i] == lab) next
            if (isTRUE(all.equal(colv[[i]], v))) al <- c(al, paste0("+ ", labs[i]))
            else if (isTRUE(all.equal(colv[[i]], -v))) al <- c(al, paste0("- ", labs[i]))
        }
        out[[length(out) + 1]] <- data.frame(Term=lab, Aliases=if (length(al)) paste(al, collapse=" ") else "",
                                              stringsAsFactors=FALSE)
    }
    list(table=do.call(rbind, out), defining=defining,
         legend=data.frame(Letter=lets, Factor=names(coding$info), stringsAsFactors=FALSE))
}


# ── Display helpers ──────────────────────────────────────────────────────────
.fa_num <- function(x, sig=5) {
    x <- as.numeric(x)
    out <- rep("", length(x))
    ok  <- is.finite(x)
    out[ok] <- trimws(formatC(signif(x[ok], sig), digits=sig, format="fg"))
    out
}
.fa_fix <- function(x, d=2) { x <- as.numeric(x); ifelse(is.finite(x), formatC(x, digits=d, format="f"), "") }
.fa_pv  <- function(p) { p <- as.numeric(p); ifelse(is.finite(p), formatC(p, digits=3, format="f"), "") }
.fa_pct <- function(x) ifelse(is.finite(x), paste0(formatC(100 * x, digits=2, format="f"), "%"), "")

.fa_show <- function(df, title, template, outline=NULL, caption=NULL, rowlabels=NULL) {
    df <- as.data.frame(df, stringsAsFactors=FALSE, check.names=FALSE)
    if (is.null(rowlabels)) rowlabels <- as.character(seq_len(nrow(df)))
    args <- list(df, title=title, templateName=template, rowlabels=rowlabels,
                 hiderowdimtitle=TRUE, hidecoldimtitle=TRUE)
    if (!is.null(outline)) args$outline <- outline
    if (!is.null(caption)) args$caption <- caption
    .fa_capture_add(list(type="table", title=title, df=df, rowlabels=rowlabels, caption=caption))
    do.call(spsspivottable.Display, args)
}

# All tables for one fitted response. Must be called inside a procedure.
fa_display_tables <- function(fo, opts) {
    resp  <- fo$resp
    coding <- fo$coding
    alpha <- fo$alpha
    kindlab <- switch(coding$kind, factorial=gtxt("two-level factorial"),
                      dsd=gtxt("definitive screening"), gtxt("response surface"))
    # Factor information
    if (isTRUE(opts$factorinfo)) {
        fi <- do.call(rbind, lapply(seq_along(coding$info), function(i) {
            z <- coding$info[[i]]
            data.frame(Code=fa_letters(length(coding$info))[i], Factor=z$name,
                       Type=if (z$type == "numeric") gtxt("Numeric") else gtxt("Text/categorical"),
                       Low=if (z$type == "numeric") .fa_num(z$low) else z$levels[1],
                       High=if (z$type == "numeric") .fa_num(z$high) else paste(z$levels[-1], collapse=", "),
                       stringsAsFactors=FALSE)
        }))
        names(fi) <- c(gtxt("Code"), gtxt("Factor"), gtxt("Type"), gtxt("Coded -1"), gtxt("Coded +1"))
        .fa_show(fi, gtxtf("Factor Coding: %s", resp), "DOEFACTORINFO", outline=gtxt("Factor Coding"),
                 caption=gtxtf("Design treated as %s. Runs used: %d (center points: %d%s).", kindlab, fo$n,
                               sum(fo$cd$CtPt == 1),
                               if (any(coding$axial)) gtxtf(", axial points: %d", sum(coding$axial[fo$rows])) else ""))
    }
    # Backward elimination steps
    if (!is.null(fo$boxcox)) {
        bc <- fo$boxcox
        bt <- data.frame(.fa_fix(bc$lambda, 2), .fa_fix(bc$rounded, 1),
                         paste0("(", .fa_fix(bc$lower, 2), ", ", .fa_fix(bc$upper, 2), ")"),
                         .fa_fix(fo$lambda, 2), stringsAsFactors=FALSE)
        names(bt) <- c(gtxt("Best lambda"), gtxt("Rounded lambda"), gtxt("95% CI"), gtxt("Lambda used"))
        .fa_show(bt, gtxtf("Box-Cox Transformation: %s", resp), "DOEBOXCOX", outline=gtxt("Box-Cox"),
                 rowlabels=resp,
                 caption=gtxt("The model is fitted to Y^lambda (natural log when lambda = 0); tables and effect charts are on that scale, the optimizer reports original units. Lambda = 1 means no transformation is needed."))
    }
    if (!is.null(fo$steps) && nrow(fo$steps) > 0) {
        st <- data.frame(fo$steps$Step, fo$steps$Action, fo$steps$Term, .fa_pv(fo$steps$PValue), stringsAsFactors=FALSE)
        names(st) <- c(gtxt("Step"), gtxt("Action"), gtxt("Term"), gtxt("P-Value"))
        meth <- switch(fo$selection, backward=gtxt("Backward elimination"), forward=gtxt("Forward selection"),
                       gtxt("Stepwise selection"))
        .fa_show(st[, -1, drop=FALSE], gtxtf("Term Selection: %s", resp), "DOESELECTION", outline=gtxt("Term Selection"),
                 rowlabels=as.character(st[[1]]),
                 caption=gtxtf("%s. Alpha to enter = %s, alpha to remove = %s; hierarchy kept (an interaction always keeps its lower-order terms).",
                               meth, format(opts$alphaenter), format(opts$alpharemove)))
    } else if (!is.null(fo$selection) && fo$selection != "none") {
        .fa_show(data.frame(Result=gtxt("No terms were entered or removed."), stringsAsFactors=FALSE),
                 gtxtf("Term Selection: %s", resp), "DOESELECTION", outline=gtxt("Term Selection"), rowlabels=resp)
    }
    # Coefficients
    cf <- fa_coef_df(fo, factorial_effects=(coding$kind == "factorial"))
    show_eff <- coding$kind == "factorial"
    ct <- data.frame(Term=cf$Term, stringsAsFactors=FALSE)
    if (show_eff) ct$Effect <- .fa_num(cf$Effect)
    ct$Coef <- .fa_num(cf$Coef); ct$SE <- .fa_num(cf$SECoef)
    ct$T <- .fa_fix(cf$T, 2); ct$P <- .fa_pv(cf$P); ct$VIF <- .fa_fix(cf$VIF, 2)
    ct$VIF[1] <- ""
    names(ct) <- c(gtxt("Term"), if (show_eff) gtxt("Effect"), gtxt("Coefficient"), gtxt("SE Coef."),
                   gtxt("t"), gtxt("Sig."), gtxt("VIF"))
    cap <- if (fo$df_error > 0)
        gtxtf("Coded units. Effect = 2 x coefficient. Error df = %d; terms with Sig. < %s are significant at alpha = %s.",
              fo$df_error, format(alpha), format(alpha))
    else gtxt("Coded units. No error degrees of freedom: standard errors and tests are unavailable; judge effects with the Pareto or normal plot of effects (Lenth's method).")
    .fa_show(ct[, -1, drop=FALSE], gtxtf("Coded Coefficients: %s", resp), "DOECODEDCOEF",
             outline=gtxt("Coded Coefficients"), caption=cap, rowlabels=ct[[1]])
    # Model summary
    sm <- fa_summary_df(fo)
    sd <- data.frame(.fa_num(sm$S), .fa_pct(sm$R2), .fa_pct(sm$R2adj), .fa_pct(sm$R2pred), .fa_num(sm$PRESS),
                     stringsAsFactors=FALSE)
    names(sd) <- c(gtxt("S"), gtxt("R-squared"), gtxt("Adj. R-squared"), gtxt("Pred. R-squared"), gtxt("PRESS"))
    .fa_show(sd, gtxtf("Model Summary: %s", resp), "DOEMODELSUMMARY", outline=gtxt("Model Summary"),
             caption=gtxt("S = residual standard deviation. Predicted R-squared uses leave-one-out (PRESS) residuals; shown as 0 when negative."),
             rowlabels=resp)
    # ANOVA
    an <- fa_anova_df(fo)
    ad <- data.frame(an$DF, .fa_num(an$AdjSS), .fa_num(an$AdjMS), .fa_fix(an$F, 2), .fa_pv(an$P),
                     stringsAsFactors=FALSE)
    names(ad) <- c(gtxt("df"), gtxt("Adj. SS"), gtxt("Adj. MS"), gtxt("F"), gtxt("Sig."))
    .fa_show(ad, gtxtf("Analysis of Variance: %s", resp), "DOECODEDANOVA", outline=gtxt("ANOVA"),
             caption=gtxt("Adjusted (partial) sums of squares: each term is tested given all other terms in the model."),
             rowlabels=an$Source)
    # Uncoded equation(s)
    if (isTRUE(opts$equation)) {
        uc <- fa_uncoded(fo)
        ud <- uc[, -1, drop=FALSE]
        for (j in seq_len(ncol(ud))) ud[[j]] <- .fa_num(ud[[j]], 8)
        .fa_show(ud, gtxtf("Regression Equation in Natural Units: %s", resp), "DOEUNCODED",
                 outline=gtxt("Natural-Unit Equation"),
                 caption=gtxtf("%s = sum of coefficient x term, with factors in their original units%s.", resp,
                               if (any(vapply(coding$info, function(z) z$type == "categorical", logical(1))))
                                   gtxt("; one column per level of the text factor(s)") else ""),
                 rowlabels=uc$Term)
    }
    # Alias structure
    if (isTRUE(opts$alias)) {
        al <- tryCatch(fa_alias(fo), error=function(e) NULL)
        if (!is.null(al) && !is.null(al$table)) {
            at <- al$table; names(at) <- c(gtxt("Term"), gtxt("Aliased With"))
            lg <- paste(paste0(al$legend$Letter, " = ", al$legend$Factor), collapse="; ")
            .fa_show(at[, 2, drop=FALSE], gtxtf("Alias Structure: %s", resp), "DOEALIAS", outline=gtxt("Alias Structure"),
                     caption=paste0(if (length(al$defining)) gtxtf("Defining relation: I = %s. ", paste(al$defining, collapse=" = ")) else "",
                                    if (length(fo$removed)) gtxtf("Not estimable separately (removed): %s. ", paste(fo$removed, collapse=", ")) else "", lg),
                     rowlabels=at[[1]])
        }
    }
    # Unusual observations
    if (isTRUE(opts$diagnostics) && fo$df_error > 0) {
        sres <- fo$resid / (fo$s * sqrt(pmax(1e-12, 1 - fo$hat)))
        p    <- ncol(fo$X)
        flagR <- abs(sres) > 2
        flagX <- fo$hat > 3 * p / fo$n
        u <- which(flagR | flagX)
        if (length(u)) {
            ud <- data.frame(.fa_num(fo$y[u]), .fa_num(fo$fitted[u]), .fa_num(fo$resid[u]), .fa_fix(sres[u], 2),
                             ifelse(flagR[u] & flagX[u], "R X", ifelse(flagR[u], "R", "X")), stringsAsFactors=FALSE)
            names(ud) <- c(resp, gtxt("Fit"), gtxt("Residual"), gtxt("Std. Residual"), gtxt("Flag"))
            .fa_show(ud, gtxtf("Unusual Observations: %s", resp), "DOEUNUSUAL", outline=gtxt("Unusual Observations"),
                     caption=gtxt("R = |standardized residual| > 2;  X = leverage > 3p/n. Row = case number in the active dataset."),
                     rowlabels=as.character(fo$rows[u]))
        }
    }
    invisible(NULL)
}

# Standardized effects for the charts: t-values when error df > 0, otherwise
# Lenth pseudo-t (effect / PSE) with m/3 df.
fa_std_effects <- function(fo) {
    keep <- which(fo$assign > 0)
    keep <- keep[vapply(keep, function(j) !identical(fo$terms[[fo$assign[j]]], "CTPT"), logical(1))]
    lab  <- fa_coef_df(fo)$Term[keep]
    code <- vapply(keep, function(j) {
        t <- fo$terms[[fo$assign[j]]]
        s <- fa_term_label(t, fo$coding, letters=TRUE)
        if (sum(fo$assign == fo$assign[j]) > 1) paste0(s, "(", sum(fo$assign[seq_len(j)] == fo$assign[j]), ")") else s
    }, character(1))
    if (fo$df_error > 0) {
        tv <- fo$t[keep]; crit <- qt(1 - fo$alpha / 2, fo$df_error); method <- "t"; df <- fo$df_error
    } else {
        eff <- 2 * fo$coef[keep]
        m   <- length(eff)
        if (m < 3) return(NULL)
        s0  <- 1.5 * median(abs(eff))
        pse <- 1.5 * median(abs(eff)[abs(eff) < 2.5 * s0])
        if (!is.finite(pse) || pse <= 0) return(NULL)
        tv <- eff / pse; df <- m / 3; crit <- qt(1 - fo$alpha / 2, df); method <- "lenth"
    }
    list(t=tv, label=lab, code=code, crit=crit, df=df, method=method)
}


# ════════════════════════════════════════════════════════════════════════════
# INTERACTIVE CHARTS FOR THE HTML REPORT (plotly.js figures built as JSON)
# Each fa_plot_* function still draws its static chart for the SPSS Viewer;
# when the HTML report is being captured it also registers an interactive
# plotly.js version of the same chart (built directly as JSON, so only the
# 'jsonlite' package is needed). The static PNG is then left out of the HTML.
# Charts without an interactive version fall back to the embedded PNG.
# ════════════════════════════════════════════════════════════════════════════
.fa_ip_on <- function() isTRUE(.fa_capture$on) && requireNamespace("jsonlite", quietly=TRUE)
.fa_A <- function(x) I(as.vector(x))                       # always a JSON array
.fa_M <- function(z) { z[!is.finite(z)] <- NA; I(unname(z)) } # matrix -> array of rows
.fa_ch <- function(data, layout, csv=NULL, name="chart", height=400)
    list(data=data, layout=layout, csv=csv, name=name, height=height)
.fa_lay <- function(title, xt="", yt="", ...) {
    l <- list(title=list(text=title, font=list(size=14), x=0.02, xanchor="left"),
              xaxis=list(title=list(text=xt), zeroline=FALSE, gridcolor="#e6e6e6"),
              yaxis=list(title=list(text=yt), zeroline=FALSE, gridcolor="#e6e6e6"),
              margin=list(l=60, r=20, t=50, b=55), hovermode="closest",
              font=list(family="Segoe UI, Helvetica, Arial, sans-serif", size=12),
              paper_bgcolor="#ffffff", plot_bgcolor="#ffffff", showlegend=FALSE)
    modifyList(l, list(...))
}
.fa_hline <- function(y, color="#999999", dash="dot")
    list(type="line", xref="paper", x0=0, x1=1, yref="y", y0=y, y1=y, line=list(color=color, dash=dash, width=1.5))
.fa_vline <- function(x, color="#999999", dash="dash")
    list(type="line", yref="paper", y0=0, y1=1, xref="x", x0=x, x1=x, line=list(color=color, dash=dash, width=2))
.fa_iplot <- function(heading, charts, cols=1, note=NULL) {
    charts <- Filter(Negate(is.null), charts)
    if (!length(charts)) return(invisible(FALSE))
    .fa_capture_add(list(type="plotly", heading=heading, charts=charts, cols=cols, note=note))
    .fa_capture$skip_img <- TRUE
    invisible(TRUE)
}
.fa_try_ip <- function(expr) if (.fa_ip_on()) tryCatch(expr, error=function(e) NULL)

.fa_ip_pareto <- function(fo, se) {
    o <- order(abs(se$t)); v <- abs(se$t)[o]; cd <- se$code[o]; lab <- se$label[o]
    sig <- v > se$crit
    xt <- if (se$method == "t") gtxt("Standardized effect |t|") else gtxt("Standardized effect |effect / PSE|")
    k <- length(fo$coding$info)
    legend_txt <- paste0("<b>", gtxt("Code"), "</b><br>", paste(fa_letters(k), names(fo$coding$info), sep="  ", collapse="<br>"))
    sub <- if (se$method == "t") gtxtf("alpha = %s, error df = %d", format(fo$alpha), fo$df_error)
           else gtxtf("alpha = %s, Lenth PSE (no error df)", format(fo$alpha))
    tr <- list(type="bar", orientation="h", x=.fa_A(v), y=.fa_A(cd), customdata=.fa_A(lab),
               marker=list(color=.fa_A(ifelse(sig, .fa_pal$sig, .fa_pal$bar))),
               hovertemplate=paste0("%{y} = %{customdata}<br>", xt, ": %{x:.4f}<extra></extra>"))
    lay <- .fa_lay(gtxtf("Pareto of Standardized Effects: %s", fo$resp), xt, "",
                   yaxis=list(type="category", automargin=TRUE, title=list(text="")),
                   margin=list(l=70, r=190, t=70, b=55),
                   shapes=list(.fa_vline(se$crit, .fa_pal$ref)),
                   annotations=list(
                       list(x=se$crit, y=1.02, xref="x", yref="paper", xanchor="left", yanchor="bottom", xshift=4, text=.fa_num(se$crit, 4), showarrow=FALSE,
                            font=list(color=.fa_pal$ref)),
                       list(x=0, y=1.09, xref="paper", yref="paper", xanchor="left", text=sub, showarrow=FALSE, font=list(size=11)),
                       list(x=1.02, y=1, xref="paper", yref="paper", xanchor="left", yanchor="top", align="left",
                            text=legend_txt, showarrow=FALSE, font=list(size=11))))
    csv <- data.frame(Code=se$code, Term=se$label, StdEffect=se$t, AbsStdEffect=abs(se$t),
                      Critical=se$crit, Significant=abs(se$t) > se$crit, stringsAsFactors=FALSE)
    .fa_iplot(gtxtf("Pareto Chart: %s", fo$resp),
              list(.fa_ch(list(tr), lay, csv, paste0("pareto_", fo$resp), max(380, 110 + 30 * length(v)))))
}

.fa_ip_normal <- function(fo, se) {
    tv <- se$t; m <- length(tv); o <- order(tv); q <- qnorm((seq_len(m) - 0.5) / m)
    sig <- abs(tv[o]) > se$crit
    mk <- function(idx, nm, col, fill, txt) list(type="scatter", mode=if (txt) "markers+text" else "markers",
        name=nm, x=.fa_A(tv[o][idx]), y=.fa_A(q[idx]), text=.fa_A(se$code[o][idx]), textposition="middle right",
        customdata=.fa_A(se$label[o][idx]),
        marker=list(size=10, color=fill, line=list(color=col, width=1.5)),
        hovertemplate="%{text} = %{customdata}<br>effect %{x:.4f}<br>score %{y:.3f}<extra></extra>")
    trs <- list()
    if (any(sig))  trs[[length(trs) + 1]] <- mk(which(sig), gtxtf("significant (alpha = %s)", format(fo$alpha)), .fa_pal$sig, .fa_pal$sig, TRUE)
    if (any(!sig)) trs[[length(trs) + 1]] <- mk(which(!sig), gtxt("not significant"), "#555555", "#ffffff", FALSE)
    sref <- if (sum(!sig) >= 2) sd(tv[o][!sig]) else NA
    if (is.finite(sref) && sref > 0) {
        xr <- range(tv); trs[[length(trs) + 1]] <- list(type="scatter", mode="lines", name=gtxt("reference"),
            x=.fa_A(xr), y=.fa_A(xr / sref), line=list(color="#888888", dash="dot"), hoverinfo="skip")
    }
    lay <- .fa_lay(gtxtf("Normal Plot of Standardized Effects: %s", fo$resp), gtxt("Standardized effect"), gtxt("Normal score"),
                   showlegend=TRUE, legend=list(x=0.01, y=0.99))
    csv <- data.frame(Code=se$code[o], Term=se$label[o], StdEffect=tv[o], NormalScore=q, Significant=sig, stringsAsFactors=FALSE)
    .fa_iplot(gtxtf("Normal Plot of Effects: %s", fo$resp), list(.fa_ch(trs, lay, csv, paste0("normal_effects_", fo$resp), 460)))
}

.fa_ip_main <- function(fo, fx, vals, ctr, xl, yr, gm, ctp) {
    nm <- names(fo$coding$info)
    charts <- lapply(seq_along(fx), function(j) {
        v <- vals[[j]]; lab <- as.character(xl[[j]])
        trs <- list(list(type="scatter", mode="lines+markers", x=.fa_A(lab), y=.fa_A(v), name=gtxt("fitted mean"),
                         line=list(color=.fa_pal$line1, width=2.5), marker=list(size=9),
                         hovertemplate=paste0(nm[fx[j]], " = %{x}<br>", gtxt("fitted mean"), " %{y:.4f}<extra></extra>")))
        cc <- ctr[[j]]
        if (!all(is.na(cc))) {
            cx <- if (length(cc) == 1) gtxt("center") else lab
            trs[[2]] <- list(type="scatter", mode="markers", x=.fa_A(cx), y=.fa_A(cc), name=gtxt("center point"),
                             marker=list(symbol="square", size=11, color=.fa_pal$center),
                             hovertemplate=paste0(gtxt("center point"), " %{y:.4f}<extra></extra>"))
        }
        cats <- if (length(cc) == 1 && !is.na(cc)) c(lab[1], gtxt("center"), lab[-1]) else lab
        lay <- .fa_lay(nm[fx[j]], nm[fx[j]], gtxtf("Fitted mean of %s", fo$resp),
                       xaxis=list(type="category", categoryorder="array", categoryarray=.fa_A(cats), title=list(text=nm[fx[j]])),
                       yaxis=list(range=.fa_A(yr), title=list(text=gtxtf("Fitted mean of %s", fo$resp)), gridcolor="#e6e6e6"),
                       shapes=list(.fa_hline(gm)))
        csv <- data.frame(Factor=nm[fx[j]], Level=lab, FittedMean=v, stringsAsFactors=FALSE)
        if (!all(is.na(cc))) csv$CenterPoint <- if (length(cc) == 1) cc else cc
        .fa_ch(trs, lay, csv, paste0("main_effect_", nm[fx[j]]), 330)
    })
    .fa_iplot(gtxtf("Main Effects (Fitted Means): %s", fo$resp), charts, cols=min(3, length(charts)),
              note=if (ctp) gtxt("Square = fitted center point; dotted line = overall mean.") else gtxt("Dotted line = overall mean."))
}

.fa_ip_interactions <- function(fo, pairs) {
    coding <- fo$coding; nm <- names(coding$info)
    lv <- function(i) { z <- coding$info[[i]]; if (z$type == "numeric") c(-1, 1) else z$levels }
    ll <- function(i) { z <- coding$info[[i]]; if (z$type == "numeric") .fa_num(z$mid + c(-1, 1) * z$half, 4) else z$levels }
    ctp <- .fa_has_ctpt(fo)
    num_idx <- which(vapply(coding$info, function(z) z$type == "numeric", logical(1)))
    cols <- c(.fa_pal$line1, .fa_pal$line2, .fa_pal$line3, "#666666", "#B279A2", "#F58518")
    charts <- lapply(pairs, function(pr) {
        a <- pr[1]; b <- pr[2]; la <- lv(a); lb <- lv(b)
        M <- sapply(lb, function(vb) vapply(la, function(va)
            .fa_marginal(fo, setNames(list(va, vb), as.character(c(a, b)))), numeric(1)))
        M <- matrix(M, nrow=length(la))
        xa <- as.character(ll(a)); lbl <- as.character(ll(b))
        trs <- lapply(seq_len(ncol(M)), function(k) list(type="scatter", mode="lines+markers",
            name=paste(nm[b], "=", lbl[k]), x=.fa_A(xa), y=.fa_A(M[, k]),
            line=list(color=cols[(k - 1) %% 6 + 1], width=2.5), marker=list(size=9),
            hovertemplate=paste0(nm[a], " = %{x}<br>", nm[b], " = ", lbl[k], "<br>%{y:.4f}<extra></extra>")))
        if (ctp && length(num_idx)) {
            f <- setNames(as.list(rep(0, length(num_idx))), as.character(num_idx)); f$CtPt <- 1
            cc <- .fa_marginal(fo, f)
            trs[[length(trs) + 1]] <- list(type="scatter", mode="markers", name=gtxt("center point"),
                x=.fa_A(gtxt("center")), y=.fa_A(cc), marker=list(symbol="square", size=11, color=.fa_pal$center))
        }
        cats <- if (ctp && length(num_idx)) c(xa[1], gtxt("center"), xa[-1]) else xa
        lay <- .fa_lay(paste(nm[a], "x", nm[b]), nm[a], gtxtf("Fitted mean of %s", fo$resp), showlegend=TRUE,
                       legend=list(orientation="h", y=-0.25),
                       xaxis=list(type="category", categoryorder="array", categoryarray=.fa_A(cats), title=list(text=nm[a])))
        csv <- data.frame(expand.grid(A=xa, B=lbl, stringsAsFactors=FALSE), FittedMean=as.vector(M))
        names(csv)[1:2] <- nm[c(a, b)]
        .fa_ch(trs, lay, csv, paste0("interaction_", nm[a], "_", nm[b]), 400)
    })
    .fa_iplot(gtxtf("Interactions (Fitted Means): %s", fo$resp), charts, cols=min(2, length(charts)))
}

.fa_ip_residuals <- function(fo, sres) {
    qq <- qqnorm(sres, plot.it=FALSE)
    ord <- if (!is.null(fo$runorder)) order(fo$runorder) else seq_along(sres)
    pt <- list(size=8, color=.fa_pal$line1)
    q1 <- quantile(sres, c(0.25, 0.75)); qn <- qnorm(c(0.25, 0.75)); sl <- diff(q1) / diff(qn); ic <- q1[1] - sl * qn[1]
    xr <- range(qq$x)
    c1 <- .fa_ch(list(list(type="scatter", mode="markers", x=.fa_A(qq$x), y=.fa_A(qq$y), marker=pt,
                           hovertemplate="%{x:.3f}, %{y:.3f}<extra></extra>"),
                      list(type="scatter", mode="lines", x=.fa_A(xr), y=.fa_A(ic + sl * xr), line=list(color=.fa_pal$line2), hoverinfo="skip")),
                 .fa_lay(gtxt("Normal probability"), gtxt("Theoretical quantile"), gtxt("Standardized residual")),
                 data.frame(Theoretical=qq$x, StdResidual=qq$y), "resid_normal", 340)
    c2 <- .fa_ch(list(list(type="scatter", mode="markers", x=.fa_A(fo$fitted), y=.fa_A(sres), marker=pt,
                           hovertemplate="fit %{x:.4f}<br>resid %{y:.3f}<extra></extra>")),
                 .fa_lay(gtxt("Versus fits"), gtxt("Fitted value"), gtxt("Standardized residual"), shapes=list(.fa_hline(0, "#666666", "dash"))),
                 data.frame(Fitted=fo$fitted, StdResidual=sres), "resid_vs_fits", 340)
    c3 <- .fa_ch(list(list(type="histogram", x=.fa_A(sres), marker=list(color=.fa_pal$bar, line=list(color="white", width=1)))),
                 .fa_lay(gtxt("Histogram"), gtxt("Standardized residual"), gtxt("Frequency")),
                 data.frame(StdResidual=sres), "resid_histogram", 340)
    c4 <- .fa_ch(list(list(type="scatter", mode="lines+markers", x=.fa_A(seq_along(sres)), y=.fa_A(sres[ord]), marker=pt,
                           line=list(color=.fa_pal$line1), hovertemplate="obs %{x}<br>resid %{y:.3f}<extra></extra>")),
                 .fa_lay(gtxt("Versus order"), gtxt("Observation order"), gtxt("Standardized residual"), shapes=list(.fa_hline(0, "#666666", "dash"))),
                 data.frame(Order=seq_along(sres), StdResidual=sres[ord]), "resid_vs_order", 340)
    .fa_iplot(gtxtf("Residual Diagnostics: %s", fo$resp), list(c1, c2, c3, c4), cols=2)
}

.fa_ip_cube <- function(fo, fx) {
    coding <- fo$coding; nm <- names(coding$info)
    lv <- function(i) { z <- coding$info[[i]]; if (z$type == "numeric") c(-1, 1) else z$levels }
    ll <- function(i) { z <- coding$info[[i]]; if (z$type == "numeric") .fa_num(z$mid + c(-1, 1) * z$half, 4) else z$levels }
    zs <- if (length(fx) == 3) 0:1 else 0
    cn <- expand.grid(x=0:1, y=0:1, z=zs)
    val <- vapply(seq_len(nrow(cn)), function(e) {
        fix <- list()
        fix[[as.character(fx[1])]] <- lv(fx[1])[cn$x[e] + 1]
        fix[[as.character(fx[2])]] <- lv(fx[2])[cn$y[e] + 1]
        if (length(fx) == 3) fix[[as.character(fx[3])]] <- lv(fx[3])[cn$z[e] + 1]
        .fa_marginal(fo, fix)
    }, numeric(1))
    ex <- ey <- ez <- c()
    for (e in seq_len(nrow(cn))) for (f in seq_len(nrow(cn)))
        if (e < f && sum(abs(unlist(cn[e, ]) - unlist(cn[f, ]))) == 1) {
            ex <- c(ex, cn$x[e], cn$x[f], NA); ey <- c(ey, cn$y[e], cn$y[f], NA); ez <- c(ez, cn$z[e], cn$z[f], NA) }
    txt <- .fa_num(val, 4)
    ax <- function(i) list(title=list(text=nm[fx[i]]), tickvals=.fa_A(c(0, 1)), ticktext=.fa_A(as.character(ll(fx[i]))),
                           range=.fa_A(c(-0.15, 1.15)), showgrid=FALSE, zeroline=FALSE, showbackground=FALSE, showspikes=FALSE)
    csv <- data.frame(A=as.character(ll(fx[1]))[cn$x + 1], B=as.character(ll(fx[2]))[cn$y + 1], FittedMean=val, stringsAsFactors=FALSE)
    names(csv)[1:2] <- nm[fx[1:2]]
    if (length(fx) == 3) {
        csv <- cbind(csv[, 1:2], C=as.character(ll(fx[3]))[cn$z + 1], FittedMean=val); names(csv)[3] <- nm[fx[3]]
        trs <- list(list(type="scatter3d", mode="lines", x=.fa_A(ex), y=.fa_A(ey), z=.fa_A(ez), line=list(color="#888888", width=4), hoverinfo="skip"),
                    list(type="scatter3d", mode="markers+text", x=.fa_A(cn$x), y=.fa_A(cn$y), z=.fa_A(cn$z), text=.fa_A(txt),
                         textposition="top center", marker=list(size=6, color=.fa_pal$line1),
                         hovertemplate=paste0(gtxt("fitted mean"), " %{text}<extra></extra>")))
        lay <- .fa_lay(gtxtf("Cube of Fitted Means: %s", fo$resp),
                       scene=list(xaxis=ax(1), yaxis=ax(2), zaxis=ax(3), camera=list(eye=list(x=1.6, y=-1.6, z=1.1))),
                       margin=list(l=0, r=0, t=50, b=0))
    } else {
        trs <- list(list(type="scatter", mode="lines", x=.fa_A(ex), y=.fa_A(ey), line=list(color="#888888"), hoverinfo="skip"),
                    list(type="scatter", mode="markers+text", x=.fa_A(cn$x), y=.fa_A(cn$y), text=.fa_A(txt),
                         textposition=.fa_A(ifelse(cn$y == 1, "top center", "bottom center")),
                         marker=list(size=11, color=.fa_pal$line1), hovertemplate="%{text}<extra></extra>"))
        lay <- .fa_lay(gtxtf("Square of Fitted Means: %s", fo$resp),
                       xaxis=modifyList(ax(1), list(range=.fa_A(c(-0.25, 1.25)), showbackground=NULL, showspikes=NULL)),
                       yaxis=modifyList(ax(2), list(range=.fa_A(c(-0.25, 1.25)), showbackground=NULL, showspikes=NULL)))
    }
    .fa_iplot(gtxtf("Cube Plot: %s", fo$resp), list(.fa_ch(trs, lay, csv, paste0("cube_", fo$resp), 560)))
}

.fa_grid_pred <- function(fo, a, b, n, rng, base=NULL) {
    coding <- fo$coding
    ga <- seq(rng(a)[1], rng(a)[2], length.out=n); gb <- seq(rng(b)[1], rng(b)[2], length.out=n)
    g <- expand.grid(ga, gb)
    nd <- (if (is.null(base)) .fa_base_row(coding) else base)[rep(1, nrow(g)), , drop=FALSE]
    nd[[paste0("X", a)]] <- g[[1]]; nd[[paste0("X", b)]] <- g[[2]]
    za <- coding$info[[a]]; zb <- coding$info[[b]]
    list(xa=za$mid + ga * za$half, xb=zb$mid + gb * zb$half, nd=nd, za=za, zb=zb, g=g)
}

.fa_ip_contours <- function(fo, pairs, surface=FALSE) {
    rng <- function(i) { r <- range(fo$cd[[paste0("X", i)]], na.rm=TRUE); c(min(-1, r[1]), max(1, r[2])) }
    n <- if (surface) 31 else 41
    charts <- lapply(pairs, function(pr) {
        a <- pr[1]; b <- pr[2]; G <- .fa_grid_pred(fo, a, b, n, rng)
        z <- matrix(fa_predict(fo, G$nd), n, n)
        pts_x <- G$za$mid + fo$cd[[paste0("X", a)]] * G$za$half; pts_y <- G$zb$mid + fo$cd[[paste0("X", b)]] * G$zb$half
        if (surface) {
            trs <- list(list(type="surface", x=.fa_A(G$xa), y=.fa_A(G$xb), z=.fa_M(t(z)), colorscale="Blues", reversescale=TRUE,
                             colorbar=list(title=list(text=fo$resp)),
                             contours=list(z=list(show=TRUE, usecolormap=TRUE, project=list(z=TRUE))),
                             hovertemplate=paste0(G$za$name, " %{x:.4g}<br>", G$zb$name, " %{y:.4g}<br>", fo$resp, " %{z:.4f}<extra></extra>")))
            lay <- .fa_lay(paste(G$za$name, "x", G$zb$name),
                           scene=list(xaxis=list(title=list(text=G$za$name)), yaxis=list(title=list(text=G$zb$name)),
                                      zaxis=list(title=list(text=fo$resp)), camera=list(eye=list(x=1.6, y=-1.5, z=0.9))),
                           margin=list(l=0, r=0, t=50, b=0))
        } else {
            trs <- list(list(type="contour", x=.fa_A(G$xa), y=.fa_A(G$xb), z=.fa_M(t(z)), colorscale="Blues", reversescale=TRUE,
                             contours=list(showlabels=TRUE, labelfont=list(size=10, color="#222222")),
                             colorbar=list(title=list(text=fo$resp)),
                             hovertemplate=paste0(G$za$name, " %{x:.4g}<br>", G$zb$name, " %{y:.4g}<br>", fo$resp, " %{z:.4f}<extra></extra>")),
                        list(type="scatter", mode="markers", name=gtxt("design points"), x=.fa_A(pts_x), y=.fa_A(pts_y),
                             marker=list(size=7, color="#E45756", line=list(color="white", width=1)),
                             hovertemplate=gtxt("design point")))
            lay <- .fa_lay(paste(G$za$name, "x", G$zb$name), G$za$name, G$zb$name)
        }
        csv <- data.frame(G$za$mid + G$g[[1]] * G$za$half, G$zb$mid + G$g[[2]] * G$zb$half, as.vector(z))
        names(csv) <- c(G$za$name, G$zb$name, paste0("Fitted_", fo$resp))
        .fa_ch(trs, lay, csv, paste0(if (surface) "surface_" else "contour_", G$za$name, "_", G$zb$name), if (surface) 480 else 420)
    })
    .fa_iplot(if (surface) gtxtf("Response Surfaces of Fitted %s", fo$resp) else gtxtf("Contour Plots of Fitted %s", fo$resp),
              charts, cols=min(2, length(charts)), note=gtxt("Other factors held at their center / first level."))
}

.fa_ip_optimizer <- function(fos, pars, opt) {
    best <- opt$solutions[[1]]; coding <- fos[[1]]$coding; info <- coding$info
    charts <- list()
    for (row in 0:length(fos)) for (i in seq_along(info)) {
        z <- info[[i]]
        if (z$type == "numeric") {
            r <- range(unlist(lapply(fos, function(f) f$cd[[paste0("X", i)]])), na.rm=TRUE)
            xs <- if (opt$uses_ctpt) c(-1, 1) else seq(min(-1, r[1]), max(1, r[2]), length.out=41)
        } else xs <- z$levels
        vals <- vapply(seq_along(xs), function(j) {
            nd <- best$nd
            nd <- if (z$type == "numeric") { nd[[paste0("X", i)]] <- xs[j]; nd } else .fa_set(nd, i, coding, xs[j])
            if (opt$uses_ctpt) nd$CtPt <- 0
            pr <- vapply(fos, function(f) fa_predict_orig(f, nd), numeric(1))
            if (row == 0) .fa_composite(pr, pars)$D else pr[row]
        }, numeric(1))
        xv <- if (z$type == "numeric") z$mid + xs * z$half else as.character(xs)
        cur <- if (z$type == "numeric") z$mid + best$xnum[match(i, opt$num)] * z$half else best$cat[[match(i, opt$cat)]]
        ylab <- if (row == 0) gtxtf("Composite D = %s", .fa_fix(best$D, 4)) else paste0(pars[[row]]$resp, " (d = ", .fa_fix(best$d[row], 4), ")")
        shp <- list(.fa_vline(cur, .fa_pal$center))
        if (row > 0) { p <- pars[[row]]; for (h in na.omit(c(p$L, p$T, p$U))) shp[[length(shp) + 1]] <- .fa_hline(h, "#bbbbbb") }
        tr <- list(type="scatter", mode=if (length(xs) > 2) "lines" else "lines+markers", x=.fa_A(xv), y=.fa_A(vals),
                   line=list(color=if (row == 0) .fa_pal$sig else .fa_pal$line1, width=2.5),
                   hovertemplate=paste0(z$name, " %{x}<br>%{y:.4f}<extra></extra>"))
        lay <- .fa_lay(paste0(if (row == 0) gtxt("Composite D") else pars[[row]]$resp, "  vs  ", z$name), z$name, ylab,
                       shapes=shp, margin=list(l=60, r=15, t=40, b=45))
        if (row == 0) lay$yaxis$range <- .fa_A(c(0, 1.02))
        if (z$type != "numeric") lay$xaxis$type <- "category"
        csv <- data.frame(Setting=xv, Value=vals, stringsAsFactors=FALSE)
        names(csv) <- c(z$name, if (row == 0) "CompositeD" else pars[[row]]$resp)
        charts[[length(charts) + 1]] <- .fa_ch(list(tr), lay, csv, paste0("optimizer_", if (row == 0) "D" else pars[[row]]$resp, "_", z$name), 260)
    }
    .fa_iplot(gtxt("Optimization Profile"), charts, cols=min(4, length(info)),
              note=gtxt("Each row varies one factor while the others stay at the optimum; the dashed line marks the optimal setting and dotted lines the response limits."))
}

.fa_ip_overlay <- function(fos, pars, opt, pairs) {
    coding <- fos[[1]]$coding; best <- opt$solutions[[1]]
    rng <- function(i) { r <- range(unlist(lapply(fos, function(f) f$cd[[paste0("X", i)]])), na.rm=TRUE); c(min(-1, r[1]), max(1, r[2])) }
    cols <- c("#4C78A8", "#E45756", "#54A24B", "#B279A2", "#F58518", "#72B7B2", "#9D755D", "#BAB0AC")
    n <- 81
    charts <- lapply(pairs, function(pr) {
        a <- pr[1]; b <- pr[2]; G <- .fa_grid_pred(fos[[1]], a, b, n, rng, base=best$nd)
        if (opt$uses_ctpt) G$nd$CtPt <- 0
        ok <- rep(TRUE, nrow(G$g)); trs <- list()
        Z <- lapply(seq_along(fos), function(k) {
            y <- fa_predict_orig(fos[[k]], G$nd); p <- pars[[k]]
            lo <- if (p$goal == "minimize") -Inf else p$L; hi <- if (p$goal == "maximize") Inf else p$U
            ok <<- ok & y >= lo & y <= hi
            matrix(y, n, n)
        })
        trs[[1]] <- list(type="heatmap", x=.fa_A(G$xa), y=.fa_A(G$xb), z=.fa_M(t(matrix(as.numeric(ok), n, n))),
                         colorscale=list(list(0, "#d9d9d9"), list(1, "#ffffff")), zmin=0, zmax=1, showscale=FALSE, hoverinfo="skip")
        for (k in seq_along(fos)) {
            p <- pars[[k]]; lvls <- na.omit(c(if (p$goal != "minimize") p$L, if (p$goal != "maximize") p$U))
            for (h in lvls) trs[[length(trs) + 1]] <- list(type="contour", x=.fa_A(G$xa), y=.fa_A(G$xb), z=.fa_M(t(Z[[k]])),
                name=paste0(fos[[k]]$resp, " = ", .fa_num(h, 4)), showscale=FALSE, autocontour=FALSE,
                contours=list(start=h, end=h, size=1, coloring="lines", showlabels=TRUE),
                line=list(color=cols[(k - 1) %% 8 + 1], width=2.5),
                colorscale=list(list(0, cols[(k - 1) %% 8 + 1]), list(1, cols[(k - 1) %% 8 + 1])),
                hovertemplate=paste0(fos[[k]]$resp, " %{z:.4f}<extra></extra>"), showlegend=TRUE)
        }
        trs[[length(trs) + 1]] <- list(type="scatter", mode="markers", name=gtxt("optimal setting"),
            x=.fa_A(G$za$mid + best$xnum[match(a, opt$num)] * G$za$half), y=.fa_A(G$zb$mid + best$xnum[match(b, opt$num)] * G$zb$half),
            marker=list(symbol="x", size=14, color="#000000"))
        lay <- .fa_lay(paste(G$za$name, "x", G$zb$name), G$za$name, G$zb$name, showlegend=TRUE, legend=list(orientation="h", y=-0.2))
        csv <- data.frame(G$za$mid + G$g[[1]] * G$za$half, G$zb$mid + G$g[[2]] * G$zb$half, AllWithinLimits=ok)
        names(csv)[1:2] <- c(G$za$name, G$zb$name)
        for (k in seq_along(fos)) csv[[fos[[k]]$resp]] <- as.vector(Z[[k]])
        .fa_ch(trs, lay, csv, paste0("overlay_", G$za$name, "_", G$zb$name), 480)
    })
    .fa_iplot(gtxt("Overlaid Contour Plot"), charts, cols=min(2, length(charts)),
              note=gtxt("White = every response within its limits; x = optimal setting (other factors at the optimum)."))
}

.fa_ip_taguchi <- function(factors, lv, tab, what, gmv, snlab) {
    all <- unlist(tab$means); yr <- range(all, na.rm=TRUE); yr <- yr + c(-0.08, 0.08) * diff(yr)
    yl <- if (what == "sn") gtxt("Mean of S/N ratios") else gtxt("Mean of means")
    charts <- lapply(seq_along(factors), function(j) {
        m <- tab$means[[j]]
        tr <- list(type="scatter", mode="lines+markers", x=.fa_A(as.character(lv[[j]])), y=.fa_A(m),
                   line=list(color=if (what == "sn") .fa_pal$sig else .fa_pal$line1, width=2.5), marker=list(size=9),
                   hovertemplate=paste0(factors[j], " = %{x}<br>%{y:.4f}<extra></extra>"))
        lay <- .fa_lay(factors[j], factors[j], yl, xaxis=list(type="category", title=list(text=factors[j])),
                       yaxis=list(range=.fa_A(yr), title=list(text=yl), gridcolor="#e6e6e6"), shapes=list(.fa_hline(gmv)))
        csv <- data.frame(Factor=factors[j], Level=as.character(lv[[j]]), Mean=as.numeric(m), stringsAsFactors=FALSE)
        .fa_ch(list(tr), lay, csv, paste0("taguchi_", what, "_", factors[j]), 320)
    })
    .fa_iplot(if (what == "sn") gtxtf("Main Effects for S/N Ratios (%s)", snlab) else gtxt("Main Effects for Means"),
              charts, cols=min(3, length(charts)), note=gtxt("Dotted line = overall mean."))
}

.fa_ip_mix_trace <- function(mo, curves) {
    cols <- c("#4C78A8", "#E45756", "#54A24B", "#B279A2", "#F58518", "#72B7B2", "#9D755D", "#BAB0AC", "#EECA3B", "#FF9DA6", "#9ECAE9", "#6F4E7C")
    trs <- lapply(seq_along(curves), function(i) list(type="scatter", mode="lines", name=mo$comps[i],
        x=.fa_A(curves[[i]]$x), y=.fa_A(curves[[i]]$y), line=list(color=cols[(i - 1) %% 12 + 1], width=2.5),
        hovertemplate=paste0(mo$comps[i], "<br>", gtxt("deviation"), " %{x:.4f}<br>%{y:.4f}<extra></extra>")))
    lay <- .fa_lay(gtxtf("Response Trace: %s", mo$resp), gtxt("Deviation from reference blend (component proportion, Cox direction)"),
                   gtxtf("Fitted %s", mo$resp), showlegend=TRUE, shapes=list(.fa_vline(0, "#999999", "dot")))
    csv <- do.call(rbind, lapply(seq_along(curves), function(i)
        data.frame(Component=mo$comps[i], Deviation=curves[[i]]$x, Fitted=curves[[i]]$y, stringsAsFactors=FALSE)))
    .fa_iplot(gtxtf("Response Trace: %s", mo$resp), list(.fa_ch(trs, lay, csv, paste0("trace_", mo$resp), 460)))
}

.fa_ip_mix_ternary <- function(mo, gx, gy, Z, labs, dp) {
    tri <- list(type="scatter", mode="lines", x=.fa_A(c(0, 1, 0.5, 0)), y=.fa_A(c(0, 0, sqrt(3) / 2, 0)),
                line=list(color="#000000", width=2), hoverinfo="skip")
    # recover the three proportions for hover from the triangle coordinates
    b3 <- outer(rep(1, length(gx)), gy / (sqrt(3) / 2)); b2 <- outer(gx, rep(1, length(gy))) - b3 / 2; b1 <- 1 - b2 - b3
    cd <- array(c(b1, b2, b3), c(length(gx), length(gy), 3)); cd <- aperm(cd, c(2, 1, 3))
    trs <- list(list(type="contour", x=.fa_A(gx), y=.fa_A(gy), z=.fa_M(t(Z)), colorscale="Blues", reversescale=TRUE,
                     connectgaps=FALSE, contours=list(showlabels=TRUE), colorbar=list(title=list(text=mo$resp)),
                     customdata=I(cd),
                     hovertemplate=paste0(labs[1], " %{customdata[0]:.3f}<br>", labs[2], " %{customdata[1]:.3f}<br>", labs[3],
                                          " %{customdata[2]:.3f}<br>", mo$resp, " %{z:.4f}<extra></extra>")),
                tri,
                list(type="scatter", mode="markers", x=.fa_A(dp[, 2] + dp[, 3] / 2), y=.fa_A(dp[, 3] * sqrt(3) / 2),
                     marker=list(size=8, color="#E45756"), hovertemplate=gtxt("design point")))
    lay <- .fa_lay(gtxtf("Mixture Contours: %s", mo$resp),
                   xaxis=list(visible=FALSE, range=.fa_A(c(-0.08, 1.08))),
                   yaxis=list(visible=FALSE, scaleanchor="x", range=.fa_A(c(-0.1, 0.95))),
                   annotations=list(list(x=0, y=-0.05, text=labs[1], showarrow=FALSE, xref="x", yref="y"),
                                    list(x=1, y=-0.05, text=labs[2], showarrow=FALSE, xref="x", yref="y"),
                                    list(x=0.5, y=sqrt(3) / 2 + 0.05, text=labs[3], showarrow=FALSE, xref="x", yref="y")))
    g <- expand.grid(x=gx, y=gy); keep <- is.finite(as.vector(Z))
    csv <- data.frame(b1=as.vector(b1)[keep], b2=as.vector(b2)[keep], b3=as.vector(b3)[keep], Fitted=as.vector(Z)[keep])
    names(csv)[1:3] <- labs
    .fa_iplot(gtxtf("Mixture Contour Plot: %s", mo$resp), list(.fa_ch(trs, lay, csv, paste0("ternary_", mo$resp), 560)))
}

# ── Writes the interactive chart report ──────────────────────────────────────
.fa_plotly_js <- function() {
    d <- tryCatch(system.file("htmlwidgets", "lib", "plotlyjs", package="plotly"), error=function(e) "")
    f <- if (nzchar(d)) list.files(d, pattern="^plotly.*min\\.js$", full.names=TRUE) else character(0)
    if (length(f)) {
        js <- tryCatch(paste(readLines(f[1], warn=FALSE, encoding="UTF-8"), collapse="\n"), error=function(e) NULL)
        if (!is.null(js)) return(paste0("<script>", gsub("</script", "<\\/script", js, fixed=TRUE), "</script>"))
    }
    "<script src=\"https://cdn.plot.ly/plotly-2.27.0.min.js\"></script>"
}
.fa_json <- function(x) gsub("</", "<\\/", as.character(jsonlite::toJSON(x, auto_unbox=TRUE, digits=NA, null="null", na="null", force=TRUE, rownames=FALSE)), fixed=TRUE)

.fa_report_js <- '
function doeCsv(rows){ if(!rows||!rows.length) return ""; var k=Object.keys(rows[0]);
  var esc=function(v){ if(v===null||v===undefined) return ""; var s=String(v); return /[",\\n]/.test(s)?"\\""+s.replace(/"/g,"\\"\\"")+"\\"":s; };
  return [k.map(esc).join(",")].concat(rows.map(function(r){return k.map(function(c){return esc(r[c]);}).join(",");})).join("\\n"); }
function doeSave(text,name,type){ var b=new Blob([text],{type:type}); var a=document.createElement("a");
  a.href=URL.createObjectURL(b); a.download=name; document.body.appendChild(a); a.click(); setTimeout(function(){URL.revokeObjectURL(a.href); a.remove();},500); }
function doeFlip(gd){ if(gd.layout.scene||gd.layout.xaxis2) return;
  var d=gd.data.map(function(t){ var n=Object.assign({},t);
    if(n.type==="contour"||n.type==="heatmap"||n.type==="surface") return t;
    if(n.type==="histogram"){ if(n.x!==undefined){n.y=n.x; delete n.x;} else {n.x=n.y; delete n.y;} return n; }
    var tx=n.x; n.x=n.y; n.y=tx; if(n.type==="bar") n.orientation=(n.orientation==="h")?"v":"h"; return n; });
  var L=Object.assign({},gd.layout), xa=L.xaxis||{}, ya=L.yaxis||{}; L.xaxis=Object.assign({},ya); L.yaxis=Object.assign({},xa);
  (L.shapes||[]).forEach(function(s){ var t; t=s.xref; s.xref=s.yref; s.yref=t; t=s.x0; s.x0=s.y0; s.y0=t; t=s.x1; s.x1=s.y1; s.y1=t; });
  Plotly.react(gd,d,L); }
var DOE_THEMES={ light:{paper_bgcolor:"#ffffff",plot_bgcolor:"#ffffff",font:{color:"#222222"},grid:"#e6e6e6"},
  dark:{paper_bgcolor:"#1e2127",plot_bgcolor:"#1e2127",font:{color:"#e6e6e6"},grid:"#3a3f48"},
  print:{paper_bgcolor:"#ffffff",plot_bgcolor:"#f7f7f7",font:{color:"#000000"},grid:"#cccccc"} };
function doeTheme(name){ var t=DOE_THEMES[name]; document.body.className="t-"+name;
  document.querySelectorAll(".doe-chart").forEach(function(gd){ if(!gd.layout) return;
    var u={paper_bgcolor:t.paper_bgcolor,plot_bgcolor:t.plot_bgcolor,"font.color":t.font.color,"xaxis.gridcolor":t.grid,"yaxis.gridcolor":t.grid};
    if(gd.layout.scene){ u["scene.xaxis.gridcolor"]=t.grid; u["scene.yaxis.gridcolor"]=t.grid; u["scene.zaxis.gridcolor"]=t.grid; }
    Plotly.relayout(gd,u); }); }
function doeFont(delta){ document.querySelectorAll(".doe-chart").forEach(function(gd){ if(!gd.layout) return;
  var s=((gd.layout.font&&gd.layout.font.size)||12)+delta; if(s<8) s=8; Plotly.relayout(gd,{"font.size":s}); }); }
function doePlot(id,data,layout,csv,name){
  var cfg={editable:true,responsive:true,displaylogo:false,scrollZoom:false,
    edits:{titleText:true,axisTitleText:true,legendText:true,legendPosition:true,annotationText:true,annotationPosition:true,colorbarTitleText:true,colorbarPosition:true,shapePosition:false},
    toImageButtonOptions:{format:"png",filename:name,scale:2},
    modeBarButtonsToAdd:[
      {name:"svg",title:"Download as SVG (vector, for papers/slides)",icon:Plotly.Icons.disk,click:function(gd){Plotly.downloadImage(gd,{format:"svg",filename:name});}},
      {name:"csv",title:"Download the chart data (CSV)",icon:{width:24,height:24,path:"M2 2h20v5H2z M2 9h6v5H2z M9 9h6v5H9z M16 9h6v5h-6z M2 16h6v6H2z M9 16h6v6H9z M16 16h6v6h-6z"},click:function(){doeSave(doeCsv(csv),name+".csv","text/csv");}},
      {name:"flip",title:"Flip orientation (swap X and Y)",icon:Plotly.Icons.autoscale,click:function(gd){doeFlip(gd);}}]};
  Plotly.newPlot(id,data,layout,cfg).then(function(gd){ gd.classList.add("doe-chart"); var cur=document.body.className.replace("t-","");
    if(cur&&cur!=="light") doeTheme(cur);
    var gp=document.getElementById("doe-global-pal"); if(gp&&gp.value&&gp.value!=="standard") doePalette(gd,gp.value); }); }
var DOE_PAL={
  standard:{label:"Standard",c:["#4C78A8","#E45756","#B279A2","#F58518","#54A24B","#72B7B2","#9D755D","#BAB0AC","#EECA3B","#FF9DA6","#9ECAE9","#6F4E7C"],scale:"Blues",rev:true},
  colorblind:{label:"Colour-blind safe",c:["#0072B2","#D55E00","#CC79A7","#E69F00","#009E73","#56B4E9","#F0E442","#999999","#000000","#E69F00","#56B4E9","#CC79A7"],scale:"Cividis",rev:false},
  vivid:{label:"Vivid",c:["#1F77B4","#D62728","#9467BD","#FF7F0E","#2CA02C","#17BECF","#8C564B","#7F7F7F","#BCBD22","#E377C2","#AEC7E8","#98DF8A"],scale:"Viridis",rev:false},
  soft:{label:"Soft / pastel",c:["#7EA6D8","#F08A8A","#C3A3D6","#F7BE7A","#94CC8E","#8FD0CB","#C7AE9A","#CFCAC4","#EFD77E","#FFB8C4","#B9D7EF","#B7A5C9"],scale:"YlGnBu",rev:true},
  earth:{label:"Earth",c:["#33658A","#A23B2A","#86608E","#D98E04","#5B8C5A","#2F8F9D","#7A5C3E","#A9A18C","#C9A227","#C97B84","#86BBD8","#5D4E6D"],scale:"Earth",rev:false},
  warm:{label:"Warm",c:["#B2182B","#2166AC","#E08214","#D6604D","#8C510A","#F4A582","#762A83","#BF812D","#FDB863","#DE77AE","#92C5DE","#5AAE61"],scale:"YlOrRd",rev:true},
  mono:{label:"Grayscale (print)",c:["#8C8C8C","#000000","#595959","#404040","#B3B3B3","#262626","#737373","#A6A6A6","#4D4D4D","#1A1A1A","#C4C4C4","#6B6B6B"],scale:"Greys",rev:true}};
var DOE_SCALES=["Blues","Cividis","Viridis","YlGnBu","Earth","YlOrRd","Greys"];
function doeWalk(o,map){ if(Array.isArray(o)) return o.map(function(v){return doeWalk(v,map);});
  if(o&&typeof o==="object"){ var n={}; for(var k in o){ if(k==="x"||k==="y"||k==="z"||k==="customdata"||k==="text"||k==="dimensions") n[k]=o[k]; else n[k]=doeWalk(o[k],map);} return n; }
  if(typeof o==="string"){ var u=o.toUpperCase(); if(map[u]) return map[u]; } return o; }
function doePalette(gd,name){ if(!gd||!gd.data) return; var from=DOE_PAL[gd.__pal||"standard"], to=DOE_PAL[name]; if(!to) return;
  var map={}; from.c.forEach(function(c,i){ if(!map[c.toUpperCase()]) map[c.toUpperCase()]=to.c[i]; });
  var d=doeWalk(gd.data,map); d.forEach(function(t){ if(typeof t.colorscale==="string"&&DOE_SCALES.indexOf(t.colorscale)>=0){ t.colorscale=to.scale; t.reversescale=to.rev; } });
  var L=Object.assign({},gd.layout); if(L.shapes) L.shapes=doeWalk(L.shapes,map); if(L.annotations) L.annotations=doeWalk(L.annotations,map);
  gd.__pal=name; Plotly.react(gd,d,L); var s=document.querySelector("select[data-for=\\""+gd.id+"\\"]"); if(s) s.value=name; }
function doePaletteAll(name){ document.querySelectorAll(".doe-chart").forEach(function(gd){ doePalette(gd,name); }); }
function doePalOptions(){ var h=""; for(var k in DOE_PAL) h+="<option value=\\""+k+"\\">"+DOE_PAL[k].label+"</option>"; return h; }
document.addEventListener("DOMContentLoaded",function(){ document.querySelectorAll("select.doe-pal").forEach(function(s){ s.innerHTML=doePalOptions(); }); });
window.addEventListener("load",function(){ document.querySelectorAll(".doe-chart").forEach(function(gd){ try{Plotly.Plots.resize(gd);}catch(e){} }); });
'


# ════════════════════════════════════════════════════════════════════════════
# RESIDUAL CHECKS, PREDICTION AT NEW SETTINGS, PLAIN-LANGUAGE SUMMARIES
# ════════════════════════════════════════════════════════════════════════════

# Anderson-Darling normality test (estimated mean and SD; Stephens /
# D'Agostino small-sample correction and p-value approximation).
fa_ad_test <- function(x) {
    x <- sort(x[is.finite(x)]); n <- length(x)
    if (n < 8 || sd(x) <= 0) return(NULL)
    z <- pnorm((x - mean(x)) / sd(x))
    z <- pmin(pmax(z, 1e-15), 1 - 1e-15)
    i <- seq_len(n)
    A <- -n - mean((2 * i - 1) * (log(z) + log(1 - rev(z))))
    AA <- A * (1 + 0.75 / n + 2.25 / n^2)
    p <- if (AA < 0.2) 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
         else if (AA < 0.34) 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
         else if (AA < 0.6) exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
         else exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
    list(A=A, p=min(1, max(0, p)))
}

# Residual checks for a fitted coded model: normality, independence in run
# order (Durbin-Watson) and influence (Cook's distance).
fa_resid_checks <- function(fo) {
    if (is.null(fo$df_error) || fo$df_error < 1 || !is.finite(fo$s) || fo$s <= 0) return(NULL)
    sres <- fo$resid / (fo$s * sqrt(pmax(1e-12, 1 - fo$hat)))
    p <- ncol(fo$X)
    cook <- sres^2 / p * fo$hat / pmax(1e-12, 1 - fo$hat)
    ord <- if (!is.null(fo$runorder) && all(is.finite(fo$runorder))) order(fo$runorder) else seq_along(fo$resid)
    e <- fo$resid[ord]
    dw <- if (length(e) >= 4 && sum(e^2) > 0) sum(diff(e)^2) / sum(e^2) else NA_real_
    list(ad=fa_ad_test(fo$resid), dw=dw, dw_runorder=!is.null(fo$runorder),
         cook=cook, cook_max=max(cook), cook_row=fo$rows[which.max(cook)],
         n_big=sum(abs(sres) > 2), sres=sres, n_cook=sum(cook > 4 / fo$n & cook > 0.5))
}

fa_display_residchecks <- function(fo, rc) {
    if (is.null(rc)) return(invisible(NULL))
    rows <- list(); labs <- character(0)
    add <- function(lab, stat, crit, concl) {
        labs <<- c(labs, lab)
        rows[[length(rows) + 1]] <<- data.frame(stat, crit, concl, stringsAsFactors=FALSE)
    }
    if (!is.null(rc$ad))
        add(gtxt("Normality (Anderson-Darling)"), .fa_fix(rc$ad$A, 3), gtxtf("Sig. = %s", .fa_pv(rc$ad$p)),
            if (rc$ad$p < 0.05) gtxt("Residuals are not normal (Sig. < 0.05): p-values may be inaccurate; consider Box-Cox.")
            else gtxt("No evidence against normal residuals."))
    if (is.finite(rc$dw))
        add(gtxt("Independence (Durbin-Watson)"), .fa_fix(rc$dw, 3), gtxt("about 2 = independent"),
            if (rc$dw < 1.4) gtxt("Neighbouring runs are positively correlated: look for a drift or time trend.")
            else if (rc$dw > 2.6) gtxt("Neighbouring runs alternate (negative correlation).")
            else gtxt("No sign of correlation between neighbouring runs."))
    add(gtxt("Influence (Cook's distance)"), .fa_fix(rc$cook_max, 3), gtxt("above 1 = influential"),
        if (rc$cook_max > 1) gtxtf("Run in row %d strongly changes the fit; check it for errors.", rc$cook_row)
        else if (rc$cook_max > 0.5) gtxtf("Run in row %d has a noticeable pull on the fit.", rc$cook_row)
        else gtxt("No single run dominates the fit."))
    df <- do.call(rbind, rows)
    names(df) <- c(gtxt("Statistic"), gtxt("Reference"), gtxt("Conclusion"))
    .fa_show(df, gtxtf("Residual Checks: %s", fo$resp), "DOERESIDCHECK", outline=gtxt("Residual Checks"),
             rowlabels=labs,
             caption=paste0(gtxt("Checks on the residuals (observed minus fitted). "),
                            if (isTRUE(rc$dw_runorder)) gtxt("Durbin-Watson uses the RunOrder column.")
                            else gtxt("Durbin-Watson uses the row order of the data (no RunOrder column).")))
}

# ── Prediction at user-specified settings ────────────────────────────────────
# spec: "Bentonite=AC BentRatio=10.5 ...; Bentonite=WYO ..." (names or letters;
# omitted numeric factors -> center, omitted text factors -> first level)
fa_parse_predict <- function(spec, coding, warns) {
    s <- paste(as.character(unlist(spec)), collapse=" ")
    s <- trimws(s); if (!nzchar(s)) return(list())
    pts <- trimws(unlist(strsplit(s, ";", fixed=TRUE))); pts <- pts[nzchar(pts)]
    info <- coding$info; nm <- names(info); lt <- fa_letters(length(info))
    lapply(pts, function(pt) {
        pr <- .fa_parse_pairs(pt)
        vals <- vector("list", length(info))
        for (k in names(pr)) {
            i <- match(tolower(k), tolower(nm)); if (is.na(i)) i <- match(toupper(k), lt)
            if (is.na(i)) { warns$warn(gtxtf("PREDICT: '%s' is not a factor name or letter code; ignored.", k), dostop=FALSE); next }
            z <- info[[i]]
            if (z$type == "numeric") {
                v <- suppressWarnings(as.numeric(pr[[k]]))
                if (!is.finite(v)) { warns$warn(gtxtf("PREDICT: value for '%s' is not numeric; its center was used.", k), dostop=FALSE); next }
                vals[[i]] <- v
            } else {
                m <- match(tolower(pr[[k]]), tolower(z$levels))
                if (is.na(m)) { warns$warn(gtxtf("PREDICT: '%s' is not a level of %s; the first level was used.", pr[[k]], z$name), dostop=FALSE); next }
                vals[[i]] <- z$levels[m]
            }
        }
        for (i in seq_along(info)) if (is.null(vals[[i]]))
            vals[[i]] <- if (info[[i]]$type == "numeric") info[[i]]$mid else info[[i]]$levels[1]
        vals
    })
}

fa_display_predict <- function(fo, pts, conf=0.95) {
    if (!length(pts)) return(invisible(NULL))
    coding <- fo$coding; info <- coding$info
    has_ct <- .fa_has_ctpt(fo)
    rows <- lapply(pts, function(vals) {
        nd <- .fa_base_row(coding)
        outside <- FALSE; atcenter <- TRUE; corner <- TRUE
        for (i in seq_along(info)) {
            z <- info[[i]]
            if (z$type == "numeric") {
                x <- (vals[[i]] - z$mid) / z$half
                r <- range(fo$cd[[paste0("X", i)]], na.rm=TRUE)
                if (x < min(-1, r[1]) - 1e-9 || x > max(1, r[2]) + 1e-9) outside <- TRUE
                if (abs(x) > 1e-9) atcenter <- FALSE
                if (abs(abs(x) - 1) > 1e-9) corner <- FALSE
                nd[[paste0("X", i)]] <- x
            } else nd <- .fa_set(nd, i, coding, vals[[i]])
        }
        nd$CtPt <- if (has_ct && atcenter && any(vapply(info, function(z) z$type == "numeric", logical(1)))) 1 else 0
        ps <- fa_predict(fo, nd, se=TRUE)
        fit <- ps$fit
        if (fo$df_error > 0) {
            tq <- qt(1 - (1 - conf) / 2, fo$df_error)
            ci <- fit + c(-1, 1) * tq * ps$se
            pi <- fit + c(-1, 1) * tq * sqrt(ps$se^2 + fo$mse)
        } else { ci <- pi <- c(NA, NA) }
        u <- function(v) sort(fa_untransform(fo, v))
        note <- c(if (outside) gtxt("outside the tested range (extrapolation)"),
                  if (has_ct && !atcenter && !corner) gtxt("curvature term assumed 0 away from the center"))
        data.frame(Settings=paste(vapply(seq_along(info), function(i)
                       paste0(info[[i]]$name, "=", if (is.numeric(vals[[i]])) .fa_num(vals[[i]], 6) else vals[[i]]), character(1)), collapse=", "),
                   Fit=.fa_num(fa_untransform(fo, fit), 5),
                   SE=if (is.finite(fo$lambda)) "" else .fa_num(ps$se, 4),
                   CI=if (all(is.finite(ci))) paste0("(", paste(.fa_num(u(ci), 5), collapse=", "), ")") else "",
                   PI=if (all(is.finite(pi))) paste0("(", paste(.fa_num(u(pi), 5), collapse=", "), ")") else "",
                   Note=paste(note, collapse="; "), stringsAsFactors=FALSE)
    })
    df <- do.call(rbind, rows)
    pc <- format(100 * conf)
    names(df) <- c(gtxt("Settings"), gtxt("Fit"), gtxt("SE Fit"), gtxtf("%s%% CI (mean)", pc), gtxtf("%s%% PI (single run)", pc), gtxt("Note"))
    .fa_show(df[, -1, drop=FALSE], gtxtf("Prediction: %s", fo$resp), "DOEPREDICT", outline=gtxt("Prediction"),
             rowlabels=df[[1]],
             caption=gtxt("CI = interval for the average response at these settings; PI = interval for one new run. Factors not listed are at their center (numeric) or first level (text)."))
}

# ── Plain-language summary ──────────────────────────────────────────────────
.fa_pctx <- function(x) if (is.na(x)) "-" else paste0(formatC(100 * x, format="f", digits=1), "%")
.fa_listx <- function(v, max=6) {
    if (!length(v)) return("")
    if (length(v) > max) v <- c(v[seq_len(max)], gtxtf("and %d more", length(v) - max))
    paste(v, collapse=", ")
}

fa_interpret <- function(fo, rc=NULL) {
    lines <- list()
    add <- function(k, txt) lines[[length(lines) + 1]] <<- c(k, txt)
    sm <- fa_summary_df(fo); a <- fo$alpha; coding <- fo$coding
    tr <- if (is.finite(fo$lambda)) gtxtf(" (on the Box-Cox scale, lambda = %s)", format(fo$lambda)) else ""
    # overall model
    an <- fa_anova_df(fo)
    pm <- an$P[1]
    if (fo$df_error > 0 && is.finite(pm))
        add(gtxt("Overall"), if (pm < a) gtxtf("The model is statistically significant (Sig. %s): the factors explain real changes in %s%s.", .fa_pv(pm), fo$resp, tr)
                             else gtxtf("The model is not statistically significant (Sig. %s): the factors studied do not clearly change %s at alpha = %s.", .fa_pv(pm), fo$resp, format(a)))
    # fit quality
    fitq <- gtxtf("The model explains %s of the variation in %s (adjusted %s).", .fa_pctx(sm$R2), fo$resp, .fa_pctx(sm$R2adj))
    if (!is.na(sm$R2pred)) {
        fitq <- paste(fitq, if (sm$R2pred >= 0.7) gtxtf("Predicted R-squared %s: it should predict new runs well.", .fa_pctx(sm$R2pred))
                            else if (sm$R2pred >= 0.4) gtxtf("Predicted R-squared %s: predictions of new runs are only moderately reliable.", .fa_pctx(sm$R2pred))
                            else gtxtf("Predicted R-squared %s: the model predicts new runs poorly; treat predictions with caution.", .fa_pctx(sm$R2pred)))
        if (is.finite(sm$R2) && sm$R2 - sm$R2pred > 0.25)
            fitq <- paste(fitq, gtxt("The large gap to R-squared suggests too many terms for the data (over-fitting)."))
    }
    add(gtxt("Model fit"), fitq)
    # significant terms
    idx <- which(!vapply(fo$terms, identical, logical(1), "CTPT"))
    if (length(idx)) {
        labs <- vapply(fo$terms[idx], fa_term_label, character(1), coding=coding)
        if (fo$df_error > 0) {
            pv <- vapply(idx, function(i) fa_term_test(fo, i)$p, numeric(1))
            sig <- which(pv < a); o <- sig[order(pv[sig])]
            ns <- setdiff(seq_along(idx), sig)
            txt <- gtxtf("%d of %d terms are significant at alpha = %s", length(sig), length(idx), format(a))
            txt <- paste0(txt, if (length(sig)) paste0(": ", .fa_listx(labs[o]), gtxt(" (strongest first).")) else ".")
            if (length(ns)) txt <- paste(txt, gtxtf("Not significant: %s.", .fa_listx(labs[ns])))
            add(gtxt("Significant terms"), txt)
        } else {
            se <- fa_std_effects(fo)
            if (!is.null(se)) {
                s <- which(abs(se$t) > se$crit); o <- s[order(-abs(se$t[s]))]
                add(gtxt("Significant terms"), gtxtf("No error degrees of freedom, so terms were judged with Lenth's method: %d of %d stand out%s.",
                    length(s), length(se$t), if (length(s)) paste0(": ", .fa_listx(se$label[o])) else ""))
            }
        }
    }
    # direction of the largest main effects (two-level coding)
    if (coding$kind == "factorial") {
        mains <- which(vapply(fo$terms, function(t) !identical(t, "CTPT") && length(t) == 1, logical(1)))
        ii <- which(vapply(fo$terms, function(t) !identical(t, "CTPT") && length(t) >= 2 && !.fa_is_square(t), logical(1)))
        if (fo$df_error > 0) ii <- ii[vapply(ii, function(k) isTRUE(fa_term_test(fo, k)$p < a), logical(1))]
        ints <- fo$terms[ii]
        ef <- list()
        for (ti in mains) {
            j <- which(fo$assign == ti); if (length(j) != 1) next
            i <- fo$terms[[ti]]; z <- coding$info[[i]]
            sigok <- if (fo$df_error > 0) fa_term_test(fo, ti)$p < a else TRUE
            if (!sigok) next
            lohi <- if (z$type == "numeric") c(.fa_num(z$low, 4), .fa_num(z$high, 4)) else z$levels[1:2]
            dep <- unique(unlist(lapply(Filter(function(t) i %in% t, ints), function(t) setdiff(t, i))))
            dep <- if (length(dep)) gtxtf(" (the size depends on %s: see the interaction plot)", paste(names(coding$info)[dep], collapse=", ")) else ""
            ef[[length(ef) + 1]] <- list(v=2 * fo$coef[j], txt=gtxtf("%s from %s to %s %s %s by %s%s", z$name, lohi[1], lohi[2],
                gtxt("changes"), fo$resp, .fa_num(2 * fo$coef[j], 4), dep))
        }
        if (length(ef)) {
            o <- order(-abs(vapply(ef, `[[`, numeric(1), "v")))[seq_len(min(3, length(ef)))]
            add(gtxt("Largest effects"), paste0(gtxt("Averaged over the other factors: "),
                paste(vapply(ef[o], `[[`, character(1), "txt"), collapse="; "), "."))
        }
    }
    # curvature
    if (.fa_has_ctpt(fo) && fo$df_error > 0) {
        ct <- which(vapply(fo$terms, identical, logical(1), "CTPT"))
        pc <- fa_term_test(fo, ct)$p
        add(gtxt("Curvature"), if (is.finite(pc) && pc < a)
            gtxtf("The center points differ from the corners (Sig. %s): the response curves inside the region. A response surface design (e.g. augment to a central composite) is needed to model it.", .fa_pv(pc))
            else gtxtf("No significant curvature (Sig. %s): a straight-line (first-order) model is adequate in this region.", .fa_pv(pc)))
    }
    # lack of fit
    lof <- an[trimws(an$Source) == trimws(gtxt("  Lack-of-Fit")), , drop=FALSE]
    if (nrow(lof) && is.finite(lof$P[1]))
        add(gtxt("Lack of fit"), if (lof$P[1] < a) gtxtf("Significant lack of fit (Sig. %s): the model misses something real; consider adding terms or a transformation.", .fa_pv(lof$P[1]))
                                  else gtxtf("No evidence of lack of fit (Sig. %s).", .fa_pv(lof$P[1])))
    # residuals
    if (!is.null(rc)) {
        bits <- character(0)
        if (!is.null(rc$ad)) bits <- c(bits, if (rc$ad$p < 0.05) gtxtf("not normal (Anderson-Darling Sig. %s)", .fa_pv(rc$ad$p))
                                              else gtxtf("consistent with a normal distribution (Anderson-Darling Sig. %s)", .fa_pv(rc$ad$p)))
        if (rc$n_big > 0) bits <- c(bits, gtxtf("%d run(s) with a large standardized residual (> 2)", rc$n_big))
        if (is.finite(rc$dw) && (rc$dw < 1.4 || rc$dw > 2.6)) bits <- c(bits, gtxtf("possible run-order pattern (Durbin-Watson %s)", .fa_fix(rc$dw, 2)))
        if (rc$cook_max > 1) bits <- c(bits, gtxtf("row %d is highly influential (Cook's D %s)", rc$cook_row, .fa_fix(rc$cook_max, 2)))
        if (length(bits)) add(gtxt("Residuals"), paste0(gtxt("Residuals are "), paste(bits, collapse="; "), "."))
    }
    # cautions
    cau <- character(0)
    if (fo$df_error > 0 && fo$df_error < 3) cau <- c(cau, gtxtf("Only %d error degree(s) of freedom: tests have little power; small real effects can look non-significant.", fo$df_error))
    if (any(is.finite(fo$vif) & fo$vif > 5)) cau <- c(cau, gtxt("Some terms are correlated (VIF > 5): their coefficients are less precise."))
    if (length(fo$removed)) cau <- c(cau, gtxtf("Aliased terms were removed: %s.", paste(fo$removed, collapse=", ")))
    if (length(cau)) add(gtxt("Caution"), paste(cau, collapse=" "))
    lines
}

.fa_show_interpret <- function(lines, title, omsid="DOEINTERPRET") {
    if (!length(lines)) return(invisible(NULL))
    df <- data.frame(vapply(lines, `[`, character(1), 2), stringsAsFactors=FALSE)
    names(df) <- gtxt("Finding")
    .fa_show(df, title, omsid, outline=gtxt("Summary of Results"),
             rowlabels=vapply(lines, `[`, character(1), 1),
             caption=gtxt("Plain-language reading of the tables below. Decisions use the alpha level shown; check the charts before acting."))
}

fa_interpret_mixture <- function(mo, alpha=0.05) {
    lines <- list(); add <- function(k, txt) lines[[length(lines) + 1]] <<- c(k, txt)
    r2 <- 1 - mo$sse / mo$sst
    pr <- if (is.finite(mo$press)) max(0, 1 - mo$press / mo$sst) else NA
    add(gtxt("Model fit"), gtxtf("The blend model explains %s of the variation in %s%s.", .fa_pctx(r2), mo$resp,
        if (!is.na(pr)) gtxtf("; predicted R-squared %s", .fa_pctx(pr)) else ""))
    an <- fa_mix_anova(mo)
    lin <- an[trimws(an$Source) == trimws(gtxt("  Linear")), , drop=FALSE]
    if (nrow(lin) && is.finite(lin$P[1]))
        add(gtxt("Components"), if (lin$P[1] < alpha) gtxtf("The pure components give different responses (Linear Sig. %s).", .fa_pv(lin$P[1]))
                                else gtxtf("The pure components do not clearly differ (Linear Sig. %s).", .fa_pv(lin$P[1])))
    blend <- an[grepl("^    ", an$Source) & is.finite(an$P), , drop=FALSE]
    if (nrow(blend)) {
        s <- blend[blend$P < alpha, , drop=FALSE]
        add(gtxt("Blending"), if (nrow(s)) gtxtf("Significant non-linear blending: %s (the mix behaves differently from a simple average of its parts).", .fa_listx(trimws(s$Source)))
                              else gtxt("No significant non-linear blending terms: the response changes roughly in proportion to the amounts."))
    }
    lof <- an[trimws(an$Source) == trimws(gtxt("  Lack-of-Fit")), , drop=FALSE]
    if (nrow(lof) && is.finite(lof$P[1]))
        add(gtxt("Lack of fit"), if (lof$P[1] < alpha) gtxtf("Significant lack of fit (Sig. %s): try a higher-order mixture model.", .fa_pv(lof$P[1]))
                                  else gtxtf("No evidence of lack of fit (Sig. %s).", .fa_pv(lof$P[1])))
    if (mo$df_error > 0) { ad <- fa_ad_test(mo$resid)
        if (!is.null(ad)) add(gtxt("Residuals"), if (ad$p < 0.05) gtxtf("Residuals are not normal (Anderson-Darling Sig. %s).", .fa_pv(ad$p))
                                                 else gtxtf("Residuals are consistent with a normal distribution (Anderson-Darling Sig. %s).", .fa_pv(ad$p))) }
    lines
}

fa_interpret_optimizer <- function(fos, pars, opt) {
    if (!length(opt$solutions)) return(list())
    b <- opt$solutions[[1]]; info <- fos[[1]]$coding$info
    set <- vapply(seq_along(info), function(i) { z <- info[[i]]
        paste0(z$name, " = ", if (z$type == "numeric") .fa_num(z$mid + b$xnum[match(i, opt$num)] * z$half, 5) else as.character(b$cat[[match(i, opt$cat)]])) }, character(1))
    lines <- list(c(gtxt("Best settings"), paste0(paste(set, collapse=", "), ".")))
    lines[[2]] <- c(gtxt("Desirability"), if (b$D <= 0) gtxt("Composite desirability is 0: no setting meets every response's acceptable range. Relax a lower/upper bound.")
        else gtxtf("Composite desirability D = %s (1 = every goal fully met, 0 = at least one response unacceptable). %s", .fa_fix(b$D, 3),
                   if (b$D >= 0.8) gtxt("The goals are met well.") else if (b$D >= 0.5) gtxt("The goals are met reasonably; some compromise is needed.") else gtxt("The goals conflict; a clear compromise was needed.")))
    if (length(fos) > 1 && !is.null(b$d)) {
        o <- order(b$d); w <- o[1]
        lines[[3]] <- c(gtxt("Trade-off"), gtxtf("Weakest goal: %s (d = %s); best met: %s (d = %s).", pars[[w]]$resp, .fa_fix(b$d[w], 3),
                                                   pars[[o[length(o)]]]$resp, .fa_fix(b$d[o[length(o)]], 3)))
    }
    lines[[length(lines) + 1]] <- c(gtxt("Next step"), gtxt("Confirm with a few runs at these settings; the prediction intervals below show the range to expect."))
    lines
}

# ── Charts (drawn with base R, submitted through .doe_submit_plot) ─────────
.fa_pal <- list(bar="#4C78A8", sig="#E45756", ref="#B279A2", center="#F58518",
                line1="#4C78A8", line2="#E45756", line3="#54A24B", grid="grey90")

fa_plot_pareto <- function(fo) {
    se <- fa_std_effects(fo); if (is.null(se)) return(invisible(NULL))
    o  <- order(abs(se$t))
    v  <- abs(se$t)[o]; cd <- se$code[o]
    .fa_try_ip(.fa_ip_pareto(fo, se))
    .doe_submit_plot({
        op <- par(mar=c(5, 6, 4.5, 11), family="sans"); on.exit(par(op))
        xmax <- max(c(v, se$crit), na.rm=TRUE) * 1.12
        bp <- barplot(v, horiz=TRUE, names.arg=cd, las=1, xlim=c(0, xmax),
                      col=ifelse(v > se$crit, .fa_pal$sig, .fa_pal$bar), border=NA,
                      xlab=if (se$method == "t") gtxt("Standardized effect |t|") else gtxt("Standardized effect |effect / PSE|"),
                      main=gtxtf("Pareto of Standardized Effects: %s", fo$resp), cex.main=1)
        abline(v=se$crit, lty=2, col=.fa_pal$ref, lwd=2)
        mtext(.fa_num(se$crit, 4), side=3, at=se$crit, col=.fa_pal$ref, cex=0.8)
        mtext(if (se$method == "t") gtxtf("alpha = %s, error df = %d", format(fo$alpha), fo$df_error)
              else gtxtf("alpha = %s, Lenth PSE (no error df)", format(fo$alpha)),
              side=3, line=0.2, cex=0.75, adj=0)
        par(xpd=TRUE)
        k <- length(fo$coding$info)
        legend(par("usr")[2] * 1.02, par("usr")[4], bty="n", cex=0.75, title=gtxt("Code"),
               legend=paste(fa_letters(k), names(fo$coding$info), sep="  "))
    }, width=960, height=max(420, 90 + 34 * length(v)))
}

fa_plot_effects_normal <- function(fo) {
    se <- fa_std_effects(fo); if (is.null(se) || length(se$t) < 3) return(invisible(NULL))
    tv <- se$t; m <- length(tv)
    o <- order(tv); q <- qnorm((seq_len(m) - 0.5) / m)
    sig <- abs(tv[o]) > se$crit
    .fa_try_ip(.fa_ip_normal(fo, se))
    .doe_submit_plot({
        op <- par(mar=c(5, 5, 4.5, 2), family="sans"); on.exit(par(op))
        plot(tv[o], q, pch=21, bg=ifelse(sig, .fa_pal$sig, "white"), col=ifelse(sig, .fa_pal$sig, "grey30"),
             xlab=gtxt("Standardized effect"), ylab=gtxt("Normal score"),
             main=gtxtf("Normal Plot of Standardized Effects: %s", fo$resp), cex.main=1)
        grid(col=.fa_pal$grid)
        sref <- if (sum(!sig) >= 2) sd(tv[!sig]) else NA
        if (is.finite(sref) && sref > 0) abline(a=0, b=1 / sref, col="grey50", lty=3)
        if (any(sig)) text(tv[o][sig], q[sig], se$code[o][sig], pos=4, cex=0.8)
        legend("topleft", bty="n", cex=0.8, pch=21, pt.bg=c(.fa_pal$sig, "white"),
               legend=c(gtxtf("significant (alpha = %s)", format(fo$alpha)), gtxt("not significant")))
    })
}

# settings frame helpers ------------------------------------------------------
.fa_base_row <- function(coding) {
    r <- data.frame(row.names=1)
    for (i in seq_along(coding$info)) {
        z <- coding$info[[i]]
        if (z$type == "numeric" || z$nlev == 2) r[[paste0("X", i)]] <- 0
        else {
            f <- factor(z$levels[1], levels=z$levels); contrasts(f) <- contr.sum(z$nlev)
            r[[paste0("X", i)]] <- f
        }
    }
    r$CtPt <- 0
    r
}
.fa_set <- function(df, i, coding, value) {
    z <- coding$info[[i]]
    if (z$type == "categorical" && z$nlev > 2) {
        f <- factor(if (length(value) == 1) rep(value, nrow(df)) else value, levels=z$levels)
        contrasts(f) <- contr.sum(z$nlev)
        df[[paste0("X", i)]] <- f
    } else if (z$type == "categorical") {
        df[[paste0("X", i)]] <- ifelse(value == z$levels[1], -1, 1)
    } else df[[paste0("X", i)]] <- value
    df
}
.fa_factors_in_model <- function(fo) sort(unique(unlist(Filter(function(t) !identical(t, "CTPT"), fo$terms))))
.fa_has_ctpt <- function(fo) any(vapply(fo$terms, identical, logical(1), "CTPT"))

# Fitted mean for factor i at a level, averaging over all level combinations
# of the other factors (cube corners for 2-level factors).
.fa_marginal <- function(fo, fix) {
    coding <- fo$coding; k <- length(coding$info)
    grid <- lapply(seq_len(k), function(i) {
        if (!is.null(fix[[as.character(i)]])) return(fix[[as.character(i)]])
        z <- coding$info[[i]]
        if (z$type == "numeric") c(-1, 1) else z$levels
    })
    g <- expand.grid(grid, stringsAsFactors=FALSE)
    nd <- .fa_base_row(coding)[rep(1, nrow(g)), , drop=FALSE]
    for (i in seq_len(k)) {
        z <- coding$info[[i]]
        if (z$type == "categorical" && z$nlev > 2) nd <- .fa_set(nd, i, coding, g[[i]])
        else if (z$type == "categorical") nd[[paste0("X", i)]] <- ifelse(g[[i]] == z$levels[1], -1, 1)
        else nd[[paste0("X", i)]] <- as.numeric(g[[i]])
    }
    if (!is.null(fix$CtPt)) nd$CtPt <- fix$CtPt
    mean(fa_predict(fo, nd))
}

fa_plot_main_effects <- function(fo) {
    coding <- fo$coding; fx <- .fa_factors_in_model(fo)
    if (!length(fx)) return(invisible(NULL))
    ctp <- .fa_has_ctpt(fo)
    num_idx <- which(vapply(coding$info, function(z) z$type == "numeric", logical(1)))
    vals <- list(); ctr <- list(); xl <- list()
    for (i in fx) {
        z <- coding$info[[i]]
        if (z$type == "numeric") {
            xs <- if (coding$kind == "factorial") c(-1, 1) else c(-1, 0, 1)
            fixes <- lapply(xs, function(x) setNames(list(x), as.character(i)))
            vals[[length(vals) + 1]] <- vapply(fixes, function(f) .fa_marginal(fo, f), numeric(1))
            xl[[length(xl) + 1]] <- .fa_num(z$mid + xs * z$half, 4)
            # center: every numeric factor at 0 and curvature term on
            ctr[[length(ctr) + 1]] <- if (ctp) {
                f <- setNames(as.list(rep(0, length(num_idx))), as.character(num_idx)); f$CtPt <- 1
                .fa_marginal(fo, f) } else NA
        } else {
            vals[[length(vals) + 1]] <- vapply(z$levels, function(l) .fa_marginal(fo, setNames(list(l), as.character(i))), numeric(1))
            xl[[length(xl) + 1]] <- z$levels
            ctr[[length(ctr) + 1]] <- if (ctp && length(num_idx)) vapply(z$levels, function(l) {
                f <- setNames(as.list(rep(0, length(num_idx))), as.character(num_idx))
                f[[as.character(i)]] <- l; f$CtPt <- 1; .fa_marginal(fo, f) }, numeric(1)) else NA
        }
    }
    yr <- range(c(unlist(vals), unlist(ctr)), na.rm=TRUE); yr <- yr + c(-1, 1) * 0.08 * diff(yr)
    gm <- mean(fo$y)
    nplot <- length(fx); nc <- min(nplot, 4); nr <- ceiling(nplot / nc)
    .fa_try_ip(.fa_ip_main(fo, fx, vals, ctr, xl, yr, gm, ctp))
    .doe_submit_plot({
        op <- par(mfrow=c(nr, nc), mar=c(4.5, 4.2, 2.5, 0.8), oma=c(0, 0, 3, 0), family="sans"); on.exit(par(op))
        for (j in seq_along(fx)) {
            v <- vals[[j]]; xs <- seq_along(v)
            plot(xs, v, type="b", pch=19, col=.fa_pal$line1, lwd=2, ylim=yr, xlim=c(0.7, length(v) + 0.3),
                 xaxt="n", xlab="", ylab=if (j %% nc == 1 || nc == 1) gtxtf("Fitted mean of %s", fo$resp) else "",
                 main=names(coding$info)[fx[j]], cex.main=0.95)
            axis(1, at=xs, labels=xl[[j]], cex.axis=0.85)
            grid(col=.fa_pal$grid); abline(h=gm, lty=3, col="grey55")
            cc <- ctr[[j]]
            if (!all(is.na(cc))) {
                if (length(cc) == 1) points(mean(xs), cc, pch=15, col=.fa_pal$center, cex=1.3)
                else points(xs, cc, pch=15, col=.fa_pal$center, cex=1.3)
            }
        }
        mtext(gtxtf("Main Effects (Fitted Means): %s", fo$resp), outer=TRUE, line=1.2, font=2)
        if (ctp) mtext(gtxt("square = fitted center point;  dotted line = overall mean"), outer=TRUE, line=0.1, cex=0.75)
    }, width=min(1400, 300 * nc + 120), height=300 * nr + 110)
}

fa_plot_interactions <- function(fo, allpairs=FALSE) {
    coding <- fo$coding
    pairs <- Filter(function(t) !identical(t, "CTPT") && length(t) == 2 && t[1] != t[2], fo$terms)
    if (allpairs || !length(pairs)) {
        fx <- .fa_factors_in_model(fo)
        pairs <- if (length(fx) >= 2) combn(fx, 2, simplify=FALSE) else list()
    }
    if (!length(pairs)) return(invisible(NULL))
    lv <- function(i) { z <- coding$info[[i]]; if (z$type == "numeric") c(-1, 1) else z$levels }
    ll <- function(i) { z <- coding$info[[i]]; if (z$type == "numeric") .fa_num(z$mid + c(-1, 1) * z$half, 4) else z$levels }
    ctp <- .fa_has_ctpt(fo)
    num_idx <- which(vapply(coding$info, function(z) z$type == "numeric", logical(1)))
    np <- length(pairs); nc <- min(np, 3); nr <- ceiling(np / nc)
    .fa_try_ip(.fa_ip_interactions(fo, pairs))
    .doe_submit_plot({
        op <- par(mfrow=c(nr, nc), mar=c(4.5, 4.2, 2.8, 0.8), oma=c(0, 0, 2.5, 0), family="sans"); on.exit(par(op))
        for (pr in pairs) {
            a <- pr[1]; b <- pr[2]
            la <- lv(a); lb <- lv(b)
            M <- sapply(lb, function(vb) vapply(la, function(va)
                .fa_marginal(fo, setNames(list(va, vb), as.character(c(a, b)))), numeric(1)))
            M <- matrix(M, nrow=length(la))
            cc <- if (ctp && length(num_idx)) {
                f <- setNames(as.list(rep(0, length(num_idx))), as.character(num_idx)); f$CtPt <- 1
                .fa_marginal(fo, f) } else NA
            yr <- range(c(M, cc), na.rm=TRUE); yr <- yr + c(-1, 1) * 0.1 * diff(yr)
            cols <- c(.fa_pal$line1, .fa_pal$line2, .fa_pal$line3, "grey40")
            matplot(seq_along(la), M, type="b", pch=c(19, 17, 15, 18), lty=1, lwd=2, col=cols[seq_len(ncol(M))],
                    xaxt="n", xlab=names(coding$info)[a], ylab=gtxtf("Fitted mean of %s", fo$resp),
                    ylim=yr, xlim=c(0.7, length(la) + 0.3),
                    main=paste(names(coding$info)[a], "x", names(coding$info)[b]), cex.main=0.95)
            axis(1, at=seq_along(la), labels=ll(a)); grid(col=.fa_pal$grid)
            if (!is.na(cc)) points(mean(seq_along(la)), cc, pch=15, col=.fa_pal$center, cex=1.3)
            legend("topleft", bty="n", cex=0.75, lwd=2, col=cols[seq_len(ncol(M))],
                   legend=paste(names(coding$info)[b], "=", ll(b)))
        }
        mtext(gtxtf("Interactions (Fitted Means): %s", fo$resp), outer=TRUE, line=0.8, font=2)
    }, width=max(760, min(1400, 400 * nc + 60)), height=max(560, 340 * nr + 80))
}

fa_plot_residuals <- function(fo) {
    if (fo$df_error < 1) return(invisible(NULL))
    sres <- fo$resid / (fo$s * sqrt(pmax(1e-12, 1 - fo$hat)))
    .fa_try_ip(.fa_ip_residuals(fo, sres))
    .doe_submit_plot({
        op <- par(mfrow=c(2, 2), mar=c(4.5, 4.5, 2.5, 1), oma=c(0, 0, 2.5, 0), family="sans"); on.exit(par(op))
        qq <- qqnorm(sres, plot.it=FALSE)
        plot(qq$x, qq$y, pch=19, col=.fa_pal$line1, xlab=gtxt("Theoretical quantile"),
             ylab=gtxt("Standardized residual"), main=gtxt("Normal probability"))
        qqline(sres, col=.fa_pal$line2); grid(col=.fa_pal$grid)
        plot(fo$fitted, sres, pch=19, col=.fa_pal$line1, xlab=gtxt("Fitted value"),
             ylab=gtxt("Standardized residual"), main=gtxt("Versus fits")); abline(h=0, lty=2); grid(col=.fa_pal$grid)
        hist(sres, col=.fa_pal$bar, border="white", xlab=gtxt("Standardized residual"), main=gtxt("Histogram"))
        ord <- if (!is.null(fo$runorder)) order(fo$runorder) else seq_along(sres)
        plot(seq_along(sres), sres[ord], type="b", pch=19, col=.fa_pal$line1, xlab=gtxt("Observation order"),
             ylab=gtxt("Standardized residual"), main=gtxt("Versus order")); abline(h=0, lty=2)
        mtext(gtxtf("Residual Diagnostics: %s", fo$resp), outer=TRUE, line=0.8, font=2)
    })
}

fa_plot_cube <- function(fo) {
    coding <- fo$coding
    fx <- .fa_factors_in_model(fo)
    fx <- fx[vapply(coding$info[fx], function(z) z$type == "numeric" || z$nlev == 2, logical(1))]
    if (length(fx) < 2) return(invisible(NULL))
    fx <- fx[seq_len(min(3, length(fx)))]
    lv <- function(i) { z <- coding$info[[i]]; if (z$type == "numeric") c(-1, 1) else z$levels }
    ll <- function(i) { z <- coding$info[[i]]; if (z$type == "numeric") .fa_num(z$mid + c(-1, 1) * z$half, 4) else z$levels }
    .fa_try_ip(.fa_ip_cube(fo, fx))
    .doe_submit_plot({
        op <- par(mar=c(4, 4, 4, 2), family="sans"); on.exit(par(op))
        plot(0, 0, type="n", xlim=c(-0.4, 1.9), ylim=c(-0.35, 1.75), axes=FALSE, xlab="", ylab="",
             main=gtxtf("Cube of Fitted Means: %s", fo$resp), cex.main=1)
        P <- function(x, y, z) c(x + 0.45 * z, y + 0.4 * z)
        zs <- if (length(fx) == 3) 0:1 else 0
        corners <- expand.grid(x=0:1, y=0:1, z=zs)
        for (e in seq_len(nrow(corners))) for (f in seq_len(nrow(corners))) {
            if (sum(abs(unlist(corners[e, ]) - unlist(corners[f, ]))) == 1 && e < f) {
                a <- P(corners$x[e], corners$y[e], corners$z[e]); b <- P(corners$x[f], corners$y[f], corners$z[f])
                segments(a[1], a[2], b[1], b[2], col="grey50")
            }
        }
        for (e in seq_len(nrow(corners))) {
            fix <- list()
            fix[[as.character(fx[1])]] <- lv(fx[1])[corners$x[e] + 1]
            fix[[as.character(fx[2])]] <- lv(fx[2])[corners$y[e] + 1]
            if (length(fx) == 3) fix[[as.character(fx[3])]] <- lv(fx[3])[corners$z[e] + 1]
            v <- .fa_marginal(fo, fix)
            p <- P(corners$x[e], corners$y[e], corners$z[e])
            points(p[1], p[2], pch=19, col=.fa_pal$line1)
            text(p[1], p[2], .fa_num(v, 4), pos=if (corners$y[e] == 1) 3 else 1, cex=0.85)
        }
        nm <- names(coding$info)
        text(0.5, -0.3, paste0(nm[fx[1]], ":  ", ll(fx[1])[1], "  ->  ", ll(fx[1])[2]), cex=0.85)
        text(-0.35, 0.5, paste0(nm[fx[2]], ":  ", ll(fx[2])[1], " -> ", ll(fx[2])[2]), srt=90, cex=0.85)
        if (length(fx) == 3) text(1.55, 0.05, paste0(nm[fx[3]], ":  ", ll(fx[3])[1], " -> ", ll(fx[3])[2]), srt=42, cex=0.85)
    }, width=820, height=700)
}

fa_plot_contours <- function(fo, maxpairs=6) {
    coding <- fo$coding
    fx <- .fa_factors_in_model(fo)
    fx <- fx[vapply(coding$info[fx], function(z) z$type == "numeric", logical(1))]
    if (length(fx) < 2) return(invisible(NULL))
    pairs <- combn(fx, 2, simplify=FALSE)[seq_len(min(maxpairs, choose(length(fx), 2)))]
    rng <- function(i) { r <- range(fo$cd[[paste0("X", i)]], na.rm=TRUE); c(min(-1, r[1]), max(1, r[2])) }
    np <- length(pairs); nc <- min(np, 3); nr <- ceiling(np / nc)
    .fa_try_ip(.fa_ip_contours(fo, pairs))
    .doe_submit_plot({
        op <- par(mfrow=c(nr, nc), mar=c(4.5, 4.5, 2.8, 1), oma=c(0, 0, 2.5, 0), family="sans"); on.exit(par(op))
        for (pr in pairs) {
            a <- pr[1]; b <- pr[2]
            ga <- seq(rng(a)[1], rng(a)[2], length.out=41); gb <- seq(rng(b)[1], rng(b)[2], length.out=41)
            g  <- expand.grid(ga, gb)
            nd <- .fa_base_row(coding)[rep(1, nrow(g)), , drop=FALSE]
            nd[[paste0("X", a)]] <- g[[1]]; nd[[paste0("X", b)]] <- g[[2]]
            z <- matrix(fa_predict(fo, nd), 41, 41)
            za <- coding$info[[a]]; zb <- coding$info[[b]]
            filled <- grDevices::hcl.colors(12, "Blues 3", rev=TRUE)
            image(za$mid + ga * za$half, zb$mid + gb * zb$half, z, col=filled,
                  xlab=za$name, ylab=zb$name, main=paste(za$name, "x", zb$name), cex.main=0.95)
            contour(za$mid + ga * za$half, zb$mid + gb * zb$half, z, add=TRUE, labcex=0.7, col="grey20")
        }
        mtext(gtxtf("Contours of Fitted %s (other factors at center / first level)", fo$resp),
              outer=TRUE, line=0.8, font=2)
    }, width=min(1400, 420 * nc + 60), height=380 * nr + 80)
}


# ── Multi-response optimization (Derringer & Suich desirability) ───────────
# goal: "maximize" | "minimize" | "target"; lower/target/upper per response.
fa_desirability <- function(y, goal, L, T, U, w=1) {
    d <- rep(NA_real_, length(y))
    if (goal == "maximize") {
        d <- ifelse(y <= L, 0, ifelse(y >= T, 1, ((y - L) / (T - L))^w))
    } else if (goal == "minimize") {
        d <- ifelse(y <= T, 1, ifelse(y >= U, 0, ((U - y) / (U - T))^w))
    } else {
        d <- ifelse(y < L | y > U, 0,
             ifelse(y <= T, ((y - L) / (T - L))^w, ((U - y) / (U - T))^w))
        d[y == T] <- 1
    }
    d
}

# Resolve and validate optimization parameters per response.
fa_opt_params <- function(fos, goals, lowers, targets, uppers, weights, importance, warns) {
    r <- length(fos)
    rep_to <- function(x, default) {
        x <- suppressWarnings(as.numeric(unlist(x)))
        if (!length(x)) return(rep(default, r))
        if (length(x) < r) x <- c(x, rep(default, r - length(x)))
        x[seq_len(r)]
    }
    L <- rep_to(lowers, NA); Tg <- rep_to(targets, NA); U <- rep_to(uppers, NA)
    W <- rep_to(weights, 1); I <- rep_to(importance, 1)
    goals <- tolower(rep(parse_multi_values(goals), length.out=r))
    goals[goals %in% c("max")] <- "maximize"; goals[goals %in% c("min")] <- "minimize"
    goals[goals %in% c("tgt")] <- "target"
    out <- list()
    for (k in seq_len(r)) {
        g <- goals[k]; y <- if (!is.null(fos[[k]]$y_orig)) fos[[k]]$y_orig else fos[[k]]$y; nm <- fos[[k]]$resp
        if (!g %in% c("maximize","minimize","target")) {
            warns$warn(gtxtf("Optimizer: unknown goal '%s' for %s; MAXIMIZE used.", g, nm), dostop=FALSE); g <- "maximize"
        }
        lo <- L[k]; tg <- Tg[k]; up <- U[k]
        if (g == "maximize") {
            if (!is.finite(lo)) lo <- min(y); if (!is.finite(tg)) tg <- max(y)
            if (!(tg > lo)) stop(gtxtf("Optimizer: %s (maximize) needs lower < target.", nm), call.=FALSE)
            up <- NA
        } else if (g == "minimize") {
            if (!is.finite(tg)) tg <- min(y); if (!is.finite(up)) up <- max(y)
            if (!(up > tg)) stop(gtxtf("Optimizer: %s (minimize) needs target < upper.", nm), call.=FALSE)
            lo <- NA
        } else {
            if (!is.finite(lo)) lo <- min(y); if (!is.finite(up)) up <- max(y)
            if (!is.finite(tg)) tg <- (lo + up) / 2
            if (!(lo < tg && tg < up)) stop(gtxtf("Optimizer: %s (target) needs lower < target < upper.", nm), call.=FALSE)
        }
        w <- W[k]; if (!is.finite(w) || w < 0.1 || w > 10) {
            warns$warn(gtxtf("Optimizer: weight for %s must be between 0.1 and 10; 1 used.", nm), dostop=FALSE); w <- 1 }
        im <- I[k]; if (!is.finite(im) || im < 0.1 || im > 10) {
            warns$warn(gtxtf("Optimizer: importance for %s must be between 0.1 and 10; 1 used.", nm), dostop=FALSE); im <- 1 }
        out[[k]] <- list(resp=nm, goal=g, L=lo, T=tg, U=up, w=w, imp=im)
    }
    out
}

.fa_composite <- function(preds, pars) {
    d <- vapply(seq_along(pars), function(k) {
        p <- pars[[k]]; fa_desirability(preds[k], p$goal, p$L, p$T, p$U, p$w)
    }, numeric(1))
    imp <- vapply(pars, function(p) p$imp, numeric(1))
    D <- if (any(d <= 0)) 0 else exp(sum(imp * log(d)) / sum(imp))
    # tie-breaker (only matters when D is flat, e.g. several settings all
    # reach D = 1): how far each response goes beyond its lower/upper bound,
    # uncapped for maximize/minimize, so "maximize" really prefers the
    # largest prediction among otherwise equal settings.
    tb <- mean(vapply(seq_along(pars), function(k) {
        p <- pars[[k]]; y <- preds[k]
        if (p$goal == "maximize") (y - p$L) / (p$T - p$L)
        else if (p$goal == "minimize") (p$U - y) / (p$U - p$T)
        else d[k]
    }, numeric(1)))
    list(d=d, D=D, tb=tb)
}

# holds: named character vector factor=value (natural units / level)
fa_optimize <- function(fos, pars, holds, warns, nsolutions=1) {
    coding <- fos[[1]]$coding; info <- coding$info; k <- length(info)
    hold_idx <- list()
    for (h in names(holds)) {
        i <- match(tolower(h), tolower(names(info)))
        if (is.na(i)) { warns$warn(gtxtf("Optimizer: HOLD factor '%s' is not a design factor; ignored.", h), dostop=FALSE); next }
        z <- info[[i]]
        if (z$type == "numeric") {
            v <- suppressWarnings(as.numeric(holds[[h]]))
            if (!is.finite(v)) { warns$warn(gtxtf("Optimizer: HOLD value for '%s' is not numeric; ignored.", h), dostop=FALSE); next }
            hold_idx[[as.character(i)]] <- (v - z$mid) / z$half
        } else {
            m <- match(tolower(holds[[h]]), tolower(z$levels))
            if (is.na(m)) { warns$warn(gtxtf("Optimizer: '%s' is not a level of %s; HOLD ignored.", holds[[h]], h), dostop=FALSE); next }
            hold_idx[[as.character(i)]] <- z$levels[m]
        }
    }
    num <- which(vapply(info, function(z) z$type == "numeric", logical(1)))
    cat_ <- setdiff(seq_len(k), num)
    free_num <- setdiff(num, as.integer(names(hold_idx)))
    cat_levels <- lapply(cat_, function(i) if (!is.null(hold_idx[[as.character(i)]])) hold_idx[[as.character(i)]] else info[[i]]$levels)
    catgrid <- if (length(cat_)) expand.grid(cat_levels, stringsAsFactors=FALSE) else data.frame(dummy=1)
    # coded range of each numeric factor = region spanned by the design
    rngs <- lapply(num, function(i) {
        r <- range(unlist(lapply(fos, function(f) f$cd[[paste0("X", i)]])), na.rm=TRUE)
        c(min(-1, r[1]), max(1, r[2]))
    }); names(rngs) <- as.character(num)
    uses_ctpt <- any(vapply(fos, .fa_has_ctpt, logical(1)))

    make_nd <- function(xnum, catrow, ctpt=0) {
        nd <- .fa_base_row(coding)
        for (j in seq_along(num)) nd[[paste0("X", num[j])]] <- xnum[j]
        for (j in seq_along(cat_)) nd <- .fa_set(nd, cat_[j], coding, catrow[[j]])
        nd$CtPt <- ctpt
        nd
    }
    evalD <- function(nd) {
        pr <- vapply(fos, function(f) fa_predict_orig(f, nd), numeric(1))
        c(list(pred=pr), .fa_composite(pr, pars))
    }
    cands <- list()
    add_cand <- function(xnum, catrow, ctpt) {
        nd <- make_nd(xnum, catrow, ctpt); e <- evalD(nd)
        cands[[length(cands) + 1]] <<- list(xnum=xnum, cat=catrow, ctpt=ctpt, nd=nd, pred=e$pred, d=e$d, D=e$D, tb=e$tb)
    }
    fixed_num <- function() {
        x <- rep(0, length(num))
        for (j in seq_along(num)) if (!is.null(hold_idx[[as.character(num[j])]])) x[j] <- hold_idx[[as.character(num[j])]]
        x
    }
    for (ci in seq_len(nrow(catgrid))) {
        catrow <- if (length(cat_)) as.list(catgrid[ci, , drop=TRUE]) else list()
        if (uses_ctpt) {
            # a model with a curvature term only predicts at cube corners and
            # at the center point, so only those settings are searched
            corners <- if (length(free_num)) as.matrix(expand.grid(rep(list(c(-1, 1)), length(free_num)))) else matrix(0, 1, 0)
            for (r in seq_len(nrow(corners))) {
                x <- fixed_num(); x[match(free_num, num)] <- corners[r, ]
                add_cand(x, catrow, 0)
            }
            add_cand(fixed_num(), catrow, if (length(num)) 1 else 0)
        } else {
            nf <- length(free_num)
            m  <- if (nf == 0) 1 else max(3, min(11, floor(4000^(1 / nf))))
            grid <- if (nf) as.matrix(expand.grid(lapply(free_num, function(i) seq(rngs[[as.character(i)]][1], rngs[[as.character(i)]][2], length.out=m))))
                    else matrix(0, 1, 0)
            base <- fixed_num()
            G <- matrix(base, nrow=nrow(grid), ncol=length(num), byrow=TRUE)
            if (nf) G[, match(free_num, num)] <- grid
            ndg <- .fa_base_row(coding)[rep(1, nrow(G)), , drop=FALSE]
            for (j in seq_along(num)) ndg[[paste0("X", num[j])]] <- G[, j]
            for (j in seq_along(cat_)) ndg <- .fa_set(ndg, cat_[j], coding, catrow[[j]])
            ndg$CtPt <- 0
            PR <- sapply(fos, function(f) fa_predict_orig(f, ndg)); PR <- matrix(PR, nrow=nrow(G))
            Ds <- apply(PR, 1, function(pr) { e <- .fa_composite(pr, pars); e$D + 1e-6 * e$tb })
            starts <- order(-Ds)[seq_len(min(8, length(Ds)))]
            for (s in starts) {
                x0 <- base; if (nf) x0[match(free_num, num)] <- grid[s, ]
                if (nf) {
                    lo <- vapply(free_num, function(i) rngs[[as.character(i)]][1], numeric(1))
                    hi <- vapply(free_num, function(i) rngs[[as.character(i)]][2], numeric(1))
                    obj <- function(z) {
                        x <- base; x[match(free_num, num)] <- pmin(hi, pmax(lo, z))
                        e <- evalD(make_nd(x, catrow))
                        # smooth tie-breaker so the search can move off plateaus
                        -(e$D + 1e-6 * e$tb)
                    }
                    o <- tryCatch(optim(grid[s, ], obj, method="L-BFGS-B", lower=lo, upper=hi,
                                        control=list(maxit=300)), error=function(e) NULL)
                    if (!is.null(o)) x0[match(free_num, num)] <- pmin(hi, pmax(lo, o$par))
                }
                add_cand(x0, catrow, 0)
            }
        }
    }
    Dv <- vapply(cands, function(c) c$D + 1e-6 * c$tb, numeric(1))
    # rank; remove near-duplicate solutions
    o <- order(-Dv); keep <- list()
    for (i in o) {
        c <- cands[[i]]
        dup <- any(vapply(keep, function(q) isTRUE(all.equal(q$xnum, c$xnum, tolerance=1e-3)) &&
                           identical(q$cat, c$cat) && q$ctpt == c$ctpt, logical(1)))
        if (!dup) keep[[length(keep) + 1]] <- c
        if (length(keep) >= nsolutions) break
    }
    list(solutions=keep, uses_ctpt=uses_ctpt, num=num, cat=cat_, holds=hold_idx)
}

fa_display_optimizer <- function(fos, pars, opt, conf=0.95) {
    coding <- fos[[1]]$coding; info <- coding$info
    gl <- c(maximize=gtxt("Maximize"), minimize=gtxt("Minimize"), target=gtxt("Target"))
    pt <- data.frame(vapply(pars, function(p) gl[[p$goal]], character(1)),
                     .fa_num(vapply(pars, function(p) p$L, numeric(1))),
                     .fa_num(vapply(pars, function(p) p$T, numeric(1))),
                     .fa_num(vapply(pars, function(p) p$U, numeric(1))),
                     .fa_num(vapply(pars, function(p) p$w, numeric(1))),
                     .fa_num(vapply(pars, function(p) p$imp, numeric(1))), stringsAsFactors=FALSE)
    names(pt) <- c(gtxt("Goal"), gtxt("Lower"), gtxt("Target"), gtxt("Upper"), gtxt("Weight"), gtxt("Importance"))
    .fa_show(pt, gtxt("Optimization Criteria"), "DOEOPTCRIT", outline=gtxt("Optimization Criteria"),
             rowlabels=vapply(pars, function(p) p$resp, character(1)),
             caption=gtxt("Individual desirability d rises from 0 at the lower (or upper) bound to 1 at the target; weight shapes the curve; composite D is the importance-weighted geometric mean of all d."))
    # factor ranges / holds
    fr <- do.call(rbind, lapply(seq_along(info), function(i) {
        z <- info[[i]]; h <- opt$holds[[as.character(i)]]
        rngtxt <- if (!is.null(h)) {
            if (z$type == "numeric") gtxtf("held at %s", .fa_num(z$mid + h * z$half)) else gtxtf("held at %s", h)
        } else if (z$type == "numeric") {
            r <- range(unlist(lapply(fos, function(f) f$cd[[paste0("X", i)]])), na.rm=TRUE)
            r <- c(min(-1, r[1]), max(1, r[2]))
            paste0("[", .fa_num(z$mid + r[1] * z$half), ", ", .fa_num(z$mid + r[2] * z$half), "]")
        } else paste(z$levels, collapse=", ")
        data.frame(Setting=rngtxt, stringsAsFactors=FALSE)
    }))
    names(fr) <- gtxt("Search Region")
    .fa_show(fr, gtxt("Factor Search Region"), "DOEOPTRANGE", outline=gtxt("Factor Search Region"),
             rowlabels=names(info),
             caption=if (opt$uses_ctpt) gtxt("At least one model contains the center-point (curvature) term, which is only defined at the cube corners and the center point; only those settings were evaluated.") else NULL)
    # solutions
    sols <- opt$solutions
    st <- do.call(rbind, lapply(seq_along(sols), function(s) {
        c <- sols[[s]]
        row <- list()
        for (i in seq_along(info)) {
            z <- info[[i]]
            row[[z$name]] <- if (z$type == "numeric") .fa_num(z$mid + c$xnum[match(i, opt$num)] * z$half, 5)
                             else as.character(c$cat[[match(i, opt$cat)]])
        }
        for (k in seq_along(fos)) row[[paste0(fos[[k]]$resp, " ", gtxt("fit"))]] <- .fa_num(c$pred[k], 5)
        row[[gtxt("Composite D")]] <- .fa_fix(c$D, 4)
        as.data.frame(row, stringsAsFactors=FALSE, check.names=FALSE)
    }))
    .fa_show(st, gtxt("Optimal Settings"), "DOEOPTSOL", outline=gtxt("Optimal Settings"),
             rowlabels=as.character(seq_along(sols)),
             caption=if (length(sols) && sols[[1]]$D == 0) gtxt("No setting satisfies every response's acceptable range (composite D = 0). Consider relaxing lower/upper bounds.") else NULL)
    # predictions at the best solution
    if (length(sols)) {
        c <- sols[[1]]
        pr <- do.call(rbind, lapply(seq_along(fos), function(k) {
            f <- fos[[k]]; ps <- fa_predict(f, c$nd, se=TRUE)
            if (f$df_error > 0) {
                tq <- qt(1 - (1 - conf) / 2, f$df_error)
                ci <- ps$fit + c(-1, 1) * tq * ps$se
                pi <- ps$fit + c(-1, 1) * tq * sqrt(ps$se^2 + f$mse)
            } else { ci <- c(NA, NA); pi <- c(NA, NA) }
            tr <- is.finite(f$lambda %||% NA)
            fitv <- fa_untransform(f, ps$fit)
            ci <- sort(fa_untransform(f, ci)); pi <- sort(fa_untransform(f, pi))
            data.frame(.fa_num(fitv, 5), if (tr) "" else .fa_num(ps$se, 4),
                       paste0("(", .fa_num(ci[1], 5), ", ", .fa_num(ci[2], 5), ")"),
                       paste0("(", .fa_num(pi[1], 5), ", ", .fa_num(pi[2], 5), ")"),
                       .fa_fix(c$d[k], 4), stringsAsFactors=FALSE)
        }))
        names(pr) <- c(gtxt("Fit"), gtxt("SE Fit"), gtxtf("%s%% CI", format(100 * conf)),
                       gtxtf("%s%% PI", format(100 * conf)), gtxt("Desirability d"))
        .fa_show(pr, gtxt("Predicted Responses at the Optimal Settings"), "DOEOPTPRED",
                 outline=gtxt("Predictions at Optimum"),
                 rowlabels=vapply(fos, function(f) f$resp, character(1)),
                 caption=gtxtf("CI = confidence interval for the mean response; PI = prediction interval for a single new run. Composite D = %s.", .fa_fix(c$D, 4)))
    }
    invisible(NULL)
}

# Profile chart: each response (and D) versus each factor, others at optimum.
fa_plot_optimizer <- function(fos, pars, opt) {
    if (!length(opt$solutions)) return(invisible(NULL))
    best <- opt$solutions[[1]]; coding <- fos[[1]]$coding; info <- coding$info
    free <- seq_along(info)
    if (opt$uses_ctpt) {
        # a curvature model is only defined at corners/center: show the
        # composite D of every evaluated setting instead of smooth profiles
    }
    nr <- length(fos) + 1; nc <- length(free)
    .fa_try_ip(.fa_ip_optimizer(fos, pars, opt))
    .doe_submit_plot({
        op <- par(mfrow=c(nr, nc), mar=c(2.2, 3.2, 1.6, 0.6), oma=c(3, 9, 3, 0), family="sans"); on.exit(par(op))
        for (row in 0:length(fos)) for (i in free) {
            z <- info[[i]]
            if (z$type == "numeric") {
                r <- range(unlist(lapply(fos, function(f) f$cd[[paste0("X", i)]])), na.rm=TRUE)
                xs <- if (opt$uses_ctpt) c(-1, 1) else seq(min(-1, r[1]), max(1, r[2]), length.out=41)
            } else xs <- z$levels
            vals <- vapply(seq_along(xs), function(j) {
                nd <- best$nd
                nd <- if (z$type == "numeric") { nd[[paste0("X", i)]] <- xs[j]; nd } else .fa_set(nd, i, coding, xs[j])
                if (opt$uses_ctpt) nd$CtPt <- 0
                pr <- vapply(fos, function(f) fa_predict_orig(f, nd), numeric(1))
                if (row == 0) .fa_composite(pr, pars)$D else pr[row]
            }, numeric(1))
            xnat <- if (z$type == "numeric") z$mid + (if (is.numeric(xs)) xs else 0) * z$half else seq_along(xs)
            cur  <- if (z$type == "numeric") z$mid + best$xnum[match(i, opt$num)] * z$half else match(best$cat[[match(i, opt$cat)]], xs)
            plot(xnat, vals, type=if (length(xs) > 2) "l" else "b", lwd=2, pch=19,
                 col=if (row == 0) .fa_pal$sig else .fa_pal$line1, xaxt=if (z$type == "numeric") "s" else "n",
                 xlab="", ylab="", main=if (row == 0) z$name else "", cex.main=0.95,
                 ylim=if (row == 0) c(0, 1) else NULL)
            if (z$type != "numeric") axis(1, at=seq_along(xs), labels=xs)
            abline(v=cur, col=.fa_pal$center, lty=2, lwd=1.5)
            if (row > 0) { p <- pars[[row]]; abline(h=na.omit(c(p$L, p$T, p$U)), col="grey70", lty=3) }
            if (i == 1) mtext(if (row == 0) gtxtf("Composite D\n%s", .fa_fix(best$D, 4)) else
                              paste0(pars[[row]]$resp, "\n", gtxtf("d = %s", .fa_fix(best$d[row], 4))),
                              side=2, line=4, las=1, cex=0.7, adj=1, outer=FALSE)
        }
        mtext(gtxt("Optimization Profile (dashed line = optimal setting)"), outer=TRUE, line=1, font=2)
    }, width=min(1600, 260 * nc + 220), height=min(1600, 170 * nr + 90))
}


# ── Report capture (tables + chart images) for the HTML report ─────────────
.fa_capture <- new.env()
.fa_capture$on <- FALSE
.fa_capture$items <- list()
.fa_capture_start <- function() { .fa_capture$on <- TRUE; .fa_capture$items <- list(); .fa_capture$skip_img <- FALSE }
.fa_capture_add <- function(item) if (isTRUE(.fa_capture$on)) .fa_capture$items[[length(.fa_capture$items) + 1]] <- item

.fa_b64 <- function(path) {
    raw <- readBin(path, "raw", file.info(path)$size)
    if (requireNamespace("jsonlite", quietly=TRUE)) return(jsonlite::base64_enc(raw))
    tbl <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", "")[[1]]
    b <- as.integer(raw); n <- length(b); pad <- (3 - n %% 3) %% 3
    b <- c(b, rep(0L, pad)); m <- matrix(b, nrow=3)
    v <- m[1, ] * 65536L + m[2, ] * 256L + m[3, ]
    idx <- rbind(v %/% 262144L, (v %/% 4096L) %% 64L, (v %/% 64L) %% 64L, v %% 64L) + 1L
    s <- tbl[idx]
    if (pad) s[(length(s) - pad + 1):length(s)] <- "="
    paste(s, collapse="")
}
.fa_html_esc <- function(x) { x <- gsub("&", "&amp;", x, fixed=TRUE); x <- gsub("<", "&lt;", x, fixed=TRUE); gsub(">", "&gt;", x, fixed=TRUE) }

# Opens a saved HTML report in the default browser without making SPSS wait
# for the browser: hand-off via explorer.exe (Windows), /usr/bin/open (macOS)
# or xdg-open (Linux), never blocking and never capturing output.
.doe_open_html <- function(path) {
    tryCatch({
        if (.Platform$OS.type == "windows") {
            p <- normalizePath(path, winslash="\\", mustWork=TRUE)
            system2("explorer.exe", shQuote(p, type="cmd"), wait=FALSE, stdout=FALSE, stderr=FALSE)
        } else {
            p <- normalizePath(path, mustWork=TRUE)
            opener <- if (Sys.info()[["sysname"]] == "Darwin") "/usr/bin/open" else Sys.which("xdg-open")
            if (nzchar(opener)) system2(opener, shQuote(p), wait=FALSE, stdout=FALSE, stderr=FALSE)
        }
    }, error=function(e) NULL)
    invisible(NULL)
}

fa_write_html <- function(path, title, warns) {
    # Tables stay in the SPSS Viewer; the HTML report carries the charts only,
    # as interactive plotly.js charts (static PNG for any chart without one).
    items <- Filter(function(it) it$type %in% c("image", "plotly"), .fa_capture$items)
    if (!length(items)) return(invisible(NULL))   # no charts requested: nothing to report, no message needed
    has_ip <- any(vapply(items, function(it) it$type == "plotly", logical(1)))
    css <- paste(
        "body{font-family:Segoe UI,Helvetica,Arial,sans-serif;margin:0;color:#222;background:#fff}",
        ".wrap{max-width:1200px;margin:0 auto;padding:18px 24px}",
        "h1{font-size:22px;border-bottom:2px solid #4C78A8;padding-bottom:6px;margin:0 0 6px}",
        "h2{font-size:17px;margin:30px 0 6px;color:#2b4a6f}",
        ".cap{font-size:12px;color:#555;max-width:1000px;margin:2px 0 8px}",
        ".bar{position:sticky;top:0;z-index:5;background:#f4f6f9;border-bottom:1px solid #dde2ea;padding:8px 24px;font-size:13px;display:flex;gap:14px;align-items:center;flex-wrap:wrap}",
        ".bar button,.bar select{font:inherit;padding:3px 9px;border:1px solid #b9c2cf;border-radius:4px;background:#fff;cursor:pointer}",
        ".grid{display:grid;gap:14px}",
        ".card{border:1px solid #e3e6ea;border-radius:6px;padding:4px;background:#fff;min-width:0}",
        "img{max-width:100%;border:1px solid #e3e6ea;margin:8px 0;background:#fff}",
        ".card:not(:hover) .js-placeholder{display:none}",
        ".card{position:relative}.cardbar{display:flex;justify-content:flex-end;align-items:center;gap:6px;padding:3px 6px 0;opacity:.55;transition:opacity .15s;font-size:11px;color:#667}.card:hover .cardbar{opacity:1}",
        ".cardbar select{font:12px Segoe UI,Helvetica,Arial,sans-serif;padding:2px 4px;border:1px solid #b9c2cf;border-radius:4px;background:#fff}",
        "body.t-dark .cardbar select{background:#2b3038;color:#e6e6e6;border-color:#4a515c}",
        "@media (max-width:800px){.grid{grid-template-columns:1fr !important}}",
        "body.t-dark{background:#16181c;color:#e6e6e6} body.t-dark .card{background:#1e2127;border-color:#3a3f48}",
        "body.t-dark .bar{background:#23272e;border-color:#3a3f48} body.t-dark h2{color:#8fb3de} body.t-dark .cap{color:#aaa}",
        "body.t-dark .bar button,body.t-dark .bar select{background:#2b3038;color:#e6e6e6;border-color:#4a515c}")
    out <- c("<!DOCTYPE html><html><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>",
             paste0("<title>", .fa_html_esc(title), "</title><style>", css, "</style>"))
    if (has_ip) out <- c(out, .fa_plotly_js(), paste0("<script>", .fa_report_js, "</script>"))
    out <- c(out, "</head><body class='t-light'>")
    if (has_ip)
        out <- c(out, paste0("<div class='bar'><b>", .fa_html_esc(gtxt("Chart options")), "</b>",
            "<label>", .fa_html_esc(gtxt("Theme")), " <select onchange='doeTheme(this.value)'>",
            "<option value='light'>", .fa_html_esc(gtxt("Light")), "</option><option value='dark'>", .fa_html_esc(gtxt("Dark")),
            "</option><option value='print'>", .fa_html_esc(gtxt("Print")), "</option></select></label>",
            "<label>", .fa_html_esc(gtxt("Colour palette (all charts)")), " <select id='doe-global-pal' class='doe-pal' onchange='doePaletteAll(this.value)'></select></label>",
            "<span>", .fa_html_esc(gtxt("Text size")), " <button onclick='doeFont(-1)'>A-</button> <button onclick='doeFont(1)'>A+</button></span>",
            "<span class='cap' style='margin:0'>", .fa_html_esc(gtxt("Click any title, axis label, legend entry or note to edit it; drag the legend or notes to move them. Hover a chart for its toolbar (zoom, pan, reset, download PNG or SVG, download the data as CSV, flip X/Y) and its own Colours menu (top right of each chart). Downloads keep the colours you choose.")),
            "</span></div>"))
    out <- c(out, "<div class='wrap'>", paste0("<h1>", .fa_html_esc(title), "</h1>"),
             paste0("<p class='cap'>", .fa_html_esc(format(Sys.time(), "%Y-%m-%d %H:%M")), " &middot; ",
                    .fa_html_esc(gtxt("Charts only; all tables are in the SPSS Viewer.")), "</p>"))
    nid <- 0
    for (it in items) {
        if (it$type == "image") {
            out <- c(out, paste0("<img alt='chart' src='data:image/png;base64,", it$b64, "'/>"))
        } else {
            out <- c(out, paste0("<h2>", .fa_html_esc(it$heading), "</h2>"))
            if (!is.null(it$note)) out <- c(out, paste0("<p class='cap'>", .fa_html_esc(it$note), "</p>"))
            out <- c(out, sprintf("<div class='grid' style='grid-template-columns:repeat(%d,minmax(0,1fr))'>", max(1, it$cols)))
            for (ch in it$charts) {
                nid <- nid + 1; id <- sprintf("doechart%d", nid)
                csv <- if (is.null(ch$csv)) list() else ch$csv
                nm <- gsub("[^A-Za-z0-9_.-]+", "_", ch$name)
                out <- c(out, sprintf("<div class='card'><div class='cardbar'><span>%s</span><select class='doe-pal' data-for='%s' title='%s' onchange=\"doePalette(document.getElementById('%s'),this.value)\"></select></div><div id='%s' style='width:100%%;height:%dpx'></div></div>",
                                      .fa_html_esc(gtxt("Colours")), id, .fa_html_esc(gtxt("Colour palette for this chart")), id, id, as.integer(ch$height)),
                         sprintf("<script>doePlot('%s',%s,%s,%s,%s);</script>", id, .fa_json(ch$data), .fa_json(ch$layout),
                                 .fa_json(csv), .fa_json(nm)))
            }
            out <- c(out, "</div>")
        }
    }
    out <- c(out, "</div></body></html>")
    ok <- tryCatch({ con <- file(path, open="w", encoding="UTF-8"); writeLines(out, con, useBytes=TRUE); close(con); TRUE }, error=function(e) {
        warns$warn(gtxtf("HTML report could not be written: %s", e$message), dostop=FALSE); FALSE })
    if (ok) {
        warns$warn(gtxtf("Interactive HTML chart report saved to: %s", path), dostop=FALSE)
        .doe_open_html(path)
    }
    invisible(ok)
}

# ── Orchestration ───────────────────────────────────────────────────────────
# Runs the coded analysis for every response and (optionally) the optimizer.
fa_run_analysis <- function(data, factors, responses, designtype, warns, opts) {
    factors <- as.character(unlist(factors))
    # dataset variable names are matched case-insensitively
    fixcase <- function(v) { m <- match(tolower(v), tolower(names(data))); ifelse(is.na(m), v, names(data)[m]) }
    factors <- fixcase(factors); responses <- fixcase(as.character(unlist(responses)))
    if (!length(responses)) stop(gtxt("RESPONSEVAR must name at least one response variable."), call.=FALSE)
    if (!length(factors))
        stop(gtxt("VARNAMES must list the design factors to analyze."), call.=FALSE)
    miss <- factors[!factors %in% names(data)]
    if (length(miss))
        stop(gtxtf("Factor variable(s) not found in the dataset: %s", paste(miss, collapse=", ")), call.=FALSE)
    clash <- intersect(tolower(factors), tolower(responses))
    if (length(clash))
        stop(gtxtf("A variable cannot be both a factor and a response: %s", paste(clash, collapse=", ")), call.=FALSE)
    for (r in responses) {
        if (!is.numeric(data[[r]]))
            stop(gtxtf("Response '%s' must be numeric.", r), call.=FALSE)
    }
    alpha <- suppressWarnings(as.numeric(opts$alpha)); if (!is.finite(alpha) || alpha <= 0 || alpha >= 0.5) {
        if (!is.null(opts$alpha)) warns$warn(gtxt("ALPHA must be between 0 and 0.5; 0.05 used."), dostop=FALSE)
        alpha <- 0.05 }
    conf <- suppressWarnings(as.numeric(opts$conflevel)); if (!is.finite(conf)) conf <- 95
    if (conf > 1) conf <- conf / 100
    if (conf <= 0.5 || conf >= 1) { warns$warn(gtxt("CONFLEVEL must be between 50 and 99.99; 95 used."), dostop=FALSE); conf <- 0.95 }
    arem <- suppressWarnings(as.numeric(opts$alpharemove)); if (!is.finite(arem) || arem <= 0 || arem >= 1) arem <- 0.10
    opts$alpharemove <- arem

    aent <- suppressWarnings(as.numeric(opts$alphaenter)); if (!is.finite(aent) || aent <= 0 || aent >= 1) aent <- 0.15
    opts$alphaenter <- aent
    selection <- tolower(as.character(unlist(opts$selection %||% "none"))[1])
    selection <- switch(selection, item_sel_a="none", item_sel_b="backward", item_sel_c="forward", item_sel_d="stepwise", selection)
    if (isTRUE(opts$backward) && selection %in% c("", "none")) selection <- "backward"
    if (!selection %in% c("none","backward","forward","stepwise")) {
        warns$warn(gtxtf("SELECTION=%s is not recognized; no term selection done.", selection), dostop=FALSE); selection <- "none" }
    boxcox <- tolower(as.character(unlist(opts$boxcox %||% "none"))[1])
    boxcox <- switch(boxcox, item_bc_a="none", item_bc_b="auto", item_bc_c="log", item_bc_d="sqrt",
                     item_bc_e="inverse", item_bc_f="custom", boxcox)
    fixed_lambda <- switch(boxcox, log=0, sqrt=0.5, inverse=-1,
                           custom=suppressWarnings(as.numeric(unlist(opts$lambda))[1]), NA_real_)
    if (boxcox == "custom" && !is.finite(fixed_lambda)) {
        warns$warn(gtxt("BOXCOX=CUSTOM needs LAMBDA=value; no transformation applied."), dostop=FALSE); boxcox <- "none" }

    # Mixture and Taguchi designs have their own analyses
    if (isTRUE(opts$mixture) || isTRUE(opts$taguchi)) {
        if (isTRUE(opts$html)) .fa_capture_start()
        res <- if (isTRUE(opts$mixture)) fa_run_mixture(data, factors, responses, warns, opts, alpha=alpha, conf=conf)
               else fa_run_taguchi(data, factors, responses, warns, opts, alpha=alpha)
        if (isTRUE(opts$html)) {
            .fa_capture$on <- FALSE
            fa_write_html(opts$htmlpath, gtxtf("Design of Experiments Analysis: %s", paste(responses, collapse=", ")), warns)
        }
        return(invisible(res))
    }

    coding <- fa_build_coding(data, factors, warns, lows=opts$lows, highs=opts$highs,
                              lowlevels=opts$lowlevels, designtype=designtype,
                              categorical=opts$categorical, designmodel=opts$designmodel)
    cd <- fa_code_data(data, coding)
    blockcol <- NULL
    for (bn in c("Block","Blocks")) if (bn %in% names(data)) {
        b <- data[[bn]]; if (length(unique(b[!is.na(b)])) > 1) { blockcol <- as.character(b); break }
    }
    runorder <- if ("RunOrder" %in% names(data)) suppressWarnings(as.numeric(data$RunOrder)) else NULL
    specs <- fa_parse_terms_spec(opts$terms, coding, warns)
    unknown <- setdiff(names(specs), c(".all", responses))
    if (length(unknown))
        warns$warn(gtxtf("TERMS: '%s' is not one of the response variables; that block was ignored.", paste(unknown, collapse="', '")), dostop=FALSE)
    default_terms <- fa_default_terms(coding, include_ctpt=!isFALSE(opts$centerterm))

    fos <- list()
    html <- isTRUE(opts$html)
    if (html) .fa_capture_start()
    for (r in responses) {
        spec_r <- specs[[r]]
        if (is.null(spec_r)) spec_r <- specs[[".all"]]
        terms <- if (!is.null(spec_r) && length(spec_r)) spec_r else default_terms
        if (isFALSE(opts$centerterm)) terms <- Filter(function(t) !identical(t, "CTPT"), terms)
        user_terms <- !is.null(spec_r) && length(spec_r) > 0
        fo <- tryCatch({
            trans <- NULL; bcinfo <- NULL
            if (boxcox != "none") {
                y0 <- suppressWarnings(as.numeric(data[[r]]))
                if (any(y0[is.finite(y0)] <= 0)) {
                    warns$warn(gtxtf("%s: Box-Cox needs every response value > 0; no transformation applied.", r), dostop=FALSE)
                } else if (boxcox == "auto") {
                    f0 <- fa_fit(data, cd, coding, r, terms, warns, alpha=alpha, conf=conf, blockcol=blockcol, report_alias=FALSE)
                    bcinfo <- fa_boxcox(f0$y, f0$X)
                    if (!is.null(bcinfo) && abs(bcinfo$rounded - 1) > 1e-9) trans <- list(lambda=bcinfo$rounded)
                } else trans <- list(lambda=fixed_lambda)
            }
            f <- if (selection != "none")
                fa_select(data, cd, coding, r, terms, warns, method=selection, alpha_enter=aent,
                          alpha_remove=arem, alpha=alpha, conf=conf, blockcol=blockcol,
                          report_alias=user_terms, trans=trans)
            else fa_fit(data, cd, coding, r, terms, warns, alpha=alpha, conf=conf, blockcol=blockcol,
                        report_alias=user_terms, trans=trans)
            if (!is.null(bcinfo)) f$boxcox <- bcinfo
            if (is.null(f$selection)) f$selection <- "none"
            f
        }, error=function(e) { warns$warn(gtxtf("%s: model could not be fitted: %s", r, conditionMessage(e)), dostop=FALSE); NULL })
        if (is.null(fo)) next
        if (fo$n <= 1) { warns$warn(gtxtf("%s: not enough data to fit a model.", r), dostop=FALSE); next }
        if (fo$df_error == 0)
            warns$warn(gtxtf("%s: the model uses every degree of freedom (no error term). Tests are replaced by Lenth's method in the charts; add replicates or center points, or remove terms, for full inference.", r), dostop=FALSE)
        if (!is.null(runorder)) fo$runorder <- runorder[fo$rows]
        fos[[r]] <- fo

        StartProcedure(gtxtf("DOE Analysis: %s", r), "STATSDOECODED")
        ok <- tryCatch({
            rc <- tryCatch(fa_resid_checks(fo), error=function(e) NULL)
            if (!isFALSE(opts$tables) && !isFALSE(opts$interpret))
                tryCatch(.fa_show_interpret(fa_interpret(fo, rc), gtxtf("Summary of Results: %s", r)),
                         error=function(e) warns$warn(gtxtf("%s: summary could not be written: %s", r, conditionMessage(e)), dostop=FALSE))
            if (!isFALSE(opts$tables)) fa_display_tables(fo, opts)
            if (!isFALSE(opts$tables) && isTRUE(opts$diagnostics)) fa_display_residchecks(fo, rc)
            pts <- fa_parse_predict(opts$predict, fo$coding, if (r == responses[1]) warns else list(warn=function(...) NULL))
            if (length(pts)) fa_display_predict(fo, pts, conf)
            if (isTRUE(opts$pareto))       fa_plot_pareto(fo)
            if (isTRUE(opts$effectsplot))  fa_plot_effects_normal(fo)
            if (isTRUE(opts$maineffects))  fa_plot_main_effects(fo)
            if (isTRUE(opts$interactions)) fa_plot_interactions(fo)
            if (isTRUE(opts$cubeplot))     fa_plot_cube(fo)
            if (isTRUE(opts$contourplot))  fa_plot_contours(fo)
            if (isTRUE(opts$surfaceplot))  fa_plot_surfaces(fo)
            if (isTRUE(opts$residualplots)) fa_plot_residuals(fo)
            if (isTRUE(opts$canonical) && !isFALSE(opts$tables)) fa_display_canonical(fo)
            if (isTRUE(opts$grouping) && !isFALSE(opts$tables)) fa_display_grouping(fo)
            TRUE
        }, error=function(e) { warns$warn(gtxtf("%s: output error: %s", r, conditionMessage(e)), dostop=FALSE); FALSE })
        tryCatch(spsspkg.EndProcedure(), error=function(e) NULL)
    }

    if (isTRUE(opts$optimize) && length(fos)) {
        StartProcedure(gtxt("Response Optimization"), "STATSDOEOPTIMIZE")
        tryCatch({
            pars <- fa_opt_params(fos, opts$goals, opts$lowers, opts$targets, opts$uppers,
                                  opts$weights, opts$importance, warns)
            opt  <- fa_optimize(fos, pars, .fa_parse_pairs(opts$holds), warns,
                                nsolutions=max(1, suppressWarnings(as.integer(opts$nsolutions)), na.rm=TRUE))
            if (!isFALSE(opts$interpret))
                tryCatch(.fa_show_interpret(fa_interpret_optimizer(fos, pars, opt), gtxt("Summary of Optimization")), error=function(e) NULL)
            fa_display_optimizer(fos, pars, opt, conf=conf)
            nconf <- suppressWarnings(as.integer(unlist(opts$confirmruns))[1])
            if (is.finite(nconf) && nconf >= 1) fa_display_confirm(fos, opt, nconf, conf)
            if (isTRUE(opts$robust)) fa_display_robust(fos, pars, opt, opts$robustsd, warns)
            fa_plot_optimizer(fos, pars, opt)
            if (isTRUE(opts$overlay)) fa_plot_overlay(fos, pars, opt)
        }, error=function(e) warns$warn(gtxtf("Optimizer error: %s", conditionMessage(e)), dostop=FALSE))
        tryCatch(spsspkg.EndProcedure(), error=function(e) NULL)
    }
    if (html) {
        .fa_capture$on <- FALSE
        fa_write_html(opts$htmlpath, gtxtf("Design of Experiments Analysis: %s", paste(responses, collapse=", ")), warns)
    }
    invisible(fos)
}



# ════════════════════════════════════════════════════════════════════════════
# ADDITIONAL ANALYSES: pairwise grouping, canonical analysis, surface and
# overlaid-contour plots, confirmation runs, robustness of the optimum
# ════════════════════════════════════════════════════════════════════════════

# Tukey pairwise comparison of the fitted level means of each categorical
# factor (compact letter display: levels sharing a letter do not differ).
fa_display_grouping <- function(fo) {
    if (fo$df_error < 1) return(invisible(NULL))
    coding <- fo$coding
    mains <- unlist(Filter(function(t) !identical(t, "CTPT") && length(t) == 1, fo$terms))
    cats <- mains[vapply(coding$info[mains], function(z) z$type == "categorical", logical(1))]
    for (i in cats) {
        z <- coding$info[[i]]
        lv <- z$levels; k <- length(lv)
        m  <- vapply(lv, function(l) .fa_marginal(fo, setNames(list(l), as.character(i))), numeric(1))
        xcol <- fo$cd[[paste0("X", i)]]
        nl <- vapply(seq_len(k), function(j) {
            if (is.factor(xcol)) sum(as.character(xcol) == lv[j]) else sum(xcol == (if (j == 1) -1 else 1))
        }, numeric(1))
        crit <- qtukey(1 - fo$alpha, k, fo$df_error) / sqrt(2)
        o <- order(-m); ms <- m[o]; ns <- nl[o]
        sig <- matrix(FALSE, k, k)
        for (a in seq_len(k)) for (b in seq_len(k)) if (a != b)
            sig[a, b] <- abs(ms[a] - ms[b]) > crit * fo$s * sqrt(1 / max(1, ns[a]) + 1 / max(1, ns[b]))
        letters_out <- rep("", k); groups <- list()
        for (a in seq_len(k)) {
            g <- which(!sig[a, ]); g <- g[g >= a]
            blk <- a:max(g)
            if (all(!sig[blk, blk])) {
                if (!any(vapply(groups, function(q) all(blk %in% q), logical(1)))) groups[[length(groups) + 1]] <- blk
            } else groups[[length(groups) + 1]] <- g
        }
        for (gi in seq_along(groups)) for (a in groups[[gi]])
            letters_out[a] <- paste0(letters_out[a], LETTERS[(gi - 1) %% 26 + 1])
        df <- data.frame(ns, .fa_num(fa_untransform(fo, ms), 5), letters_out, stringsAsFactors=FALSE)
        names(df) <- c(gtxt("N"), gtxt("Fitted mean"), gtxt("Grouping"))
        .fa_show(df, gtxtf("Pairwise Comparison (Tukey) of %s: %s", z$name, fo$resp), "DOEGROUPING",
                 outline=gtxt("Pairwise Comparison"), rowlabels=lv[o],
                 caption=gtxtf("Levels that share a letter are not significantly different (family alpha = %s). Means are fitted means averaged over the other factors.", format(fo$alpha)))
    }
    invisible(NULL)
}

# second-order structure of the fitted surface at fixed categorical levels
.fa_quad_parts <- function(fo, num, catfix) {
    base <- .fa_base_row(fo$coding)
    for (nm in names(catfix)) base <- .fa_set(base, as.integer(nm), fo$coding, catfix[[nm]])
    f <- function(x) { nd <- base; for (j in seq_along(num)) nd[[paste0("X", num[j])]] <- x[j]; fa_predict(fo, nd) }
    q <- length(num); f0 <- f(rep(0, q)); b <- numeric(q); B <- matrix(0, q, q)
    fp <- fm <- numeric(q)
    for (i in seq_len(q)) {
        e <- rep(0, q); e[i] <- 1
        fp[i] <- f(e); fm[i] <- f(-e)
        b[i] <- (fp[i] - fm[i]) / 2
        B[i, i] <- (fp[i] + fm[i] - 2 * f0) / 2
    }
    if (q >= 2) for (i in 1:(q - 1)) for (j in (i + 1):q) {
        e <- rep(0, q); e[c(i, j)] <- 1
        B[i, j] <- B[j, i] <- (f(e) - fp[i] - fp[j] + f0) / 2
    }
    list(f0=f0, b=b, B=B, f=f)
}

# Canonical analysis of a second-order surface: stationary point, its
# predicted value and the eigenvalues that tell maximum / minimum / saddle.
fa_display_canonical <- function(fo) {
    coding <- fo$coding
    if (.fa_has_ctpt(fo)) return(invisible(NULL))
    num <- .fa_factors_in_model(fo)
    num <- num[vapply(coding$info[num], function(z) z$type == "numeric", logical(1))]
    has_sq <- any(vapply(fo$terms, .fa_is_square, logical(1)))
    if (length(num) < 1 || !has_sq) return(invisible(NULL))
    cat_idx <- which(vapply(coding$info, function(z) z$type == "categorical", logical(1)))
    combos <- if (length(cat_idx)) expand.grid(lapply(coding$info[cat_idx], function(z) z$levels), stringsAsFactors=FALSE)
              else data.frame(dummy=1)
    rows <- list(); labs <- character(0)
    for (ci in seq_len(nrow(combos))) {
        catfix <- if (length(cat_idx)) setNames(as.list(unlist(combos[ci, ])), as.character(cat_idx)) else list()
        qp <- .fa_quad_parts(fo, num, catfix)
        ev <- eigen(qp$B, symmetric=TRUE)$values
        xs <- tryCatch(-0.5 * solve(qp$B, qp$b), error=function(e) rep(NA_real_, length(num)))
        ys <- if (all(is.finite(xs))) qp$f(xs) else NA
        tol <- 1e-6 * max(1, abs(ev))
        nature <- if (any(abs(ev) < tol)) gtxt("ridge (a near-zero eigenvalue)")
                  else if (all(ev < 0)) gtxt("maximum") else if (all(ev > 0)) gtxt("minimum") else gtxt("saddle point")
        rng <- vapply(num, function(i) { r <- range(fo$cd[[paste0("X", i)]], na.rm=TRUE); max(1, abs(r)) }, numeric(1))
        inside <- all(is.finite(xs)) && all(abs(xs) <= rng + 1e-9)
        row <- list()
        for (j in seq_along(num)) { z <- coding$info[[num[j]]]; row[[z$name]] <- .fa_num(z$mid + xs[j] * z$half, 5) }
        row[[gtxt("Predicted")]] <- .fa_num(fa_untransform(fo, ys), 5)
        row[[gtxt("Eigenvalues")]] <- paste(.fa_num(ev, 4), collapse="  ")
        row[[gtxt("Nature")]] <- nature
        row[[gtxt("Inside design region")]] <- if (inside) gtxt("Yes") else gtxt("No")
        rows[[ci]] <- as.data.frame(row, stringsAsFactors=FALSE, check.names=FALSE)
        labs <- c(labs, if (length(cat_idx)) paste(paste0(names(coding$info)[cat_idx], "=", unlist(combos[ci, ])), collapse=", ") else fo$resp)
    }
    .fa_show(do.call(rbind, rows), gtxtf("Canonical Analysis: %s", fo$resp), "DOECANONICAL",
             outline=gtxt("Canonical Analysis"), rowlabels=labs,
             caption=gtxt("Stationary point of the fitted second-order surface (factor settings in natural units). All eigenvalues negative = maximum, all positive = minimum, mixed signs = saddle. If the point lies outside the design region, move the experiment in that direction rather than extrapolating."))
    invisible(NULL)
}

fa_plot_surfaces <- function(fo, maxpairs=6) {
    coding <- fo$coding
    fx <- .fa_factors_in_model(fo)
    fx <- fx[vapply(coding$info[fx], function(z) z$type == "numeric", logical(1))]
    if (length(fx) < 2) return(invisible(NULL))
    pairs <- combn(fx, 2, simplify=FALSE)[seq_len(min(maxpairs, choose(length(fx), 2)))]
    rng <- function(i) { r <- range(fo$cd[[paste0("X", i)]], na.rm=TRUE); c(min(-1, r[1]), max(1, r[2])) }
    np <- length(pairs); nc <- min(np, 3); nr <- ceiling(np / nc)
    .fa_try_ip(.fa_ip_contours(fo, pairs, surface=TRUE))
    .doe_submit_plot({
        op <- par(mfrow=c(nr, nc), mar=c(1.5, 1.5, 2.5, 1), oma=c(0, 0, 2.5, 0), family="sans"); on.exit(par(op))
        for (pr in pairs) {
            a <- pr[1]; b <- pr[2]
            ga <- seq(rng(a)[1], rng(a)[2], length.out=31); gb <- seq(rng(b)[1], rng(b)[2], length.out=31)
            g  <- expand.grid(ga, gb)
            nd <- .fa_base_row(coding)[rep(1, nrow(g)), , drop=FALSE]
            nd[[paste0("X", a)]] <- g[[1]]; nd[[paste0("X", b)]] <- g[[2]]
            z <- matrix(fa_predict(fo, nd), 31, 31)
            za <- coding$info[[a]]; zb <- coding$info[[b]]
            zf <- (z[-1, -1] + z[-1, -31] + z[-31, -1] + z[-31, -31]) / 4
            cols <- grDevices::hcl.colors(40, "Blues 3", rev=TRUE)
            ci <- cut(zf, 40, labels=FALSE)
            persp(za$mid + ga * za$half, zb$mid + gb * zb$half, z, theta=35, phi=25, expand=0.7,
                  col=cols[ci], border="grey40", lwd=0.3, ticktype="detailed",
                  xlab=za$name, ylab=zb$name, zlab=fo$resp, main=paste(za$name, "x", zb$name), cex.main=0.95, cex.axis=0.7)
        }
        mtext(gtxtf("Response Surface of Fitted %s (other factors at center / first level)", fo$resp), outer=TRUE, line=0.8, font=2)
    }, width=min(1400, 440 * nc + 40), height=420 * nr + 60)
}

# Overlaid contour plot: region where every response meets its limits
fa_plot_overlay <- function(fos, pars, opt) {
    if (!length(opt$solutions)) return(invisible(NULL))
    coding <- fos[[1]]$coding; best <- opt$solutions[[1]]
    num <- which(vapply(coding$info, function(z) z$type == "numeric", logical(1)))
    free <- setdiff(num, as.integer(names(opt$holds)))
    if (length(free) < 2) return(invisible(NULL))
    pairs <- combn(free, 2, simplify=FALSE)[seq_len(min(3, choose(length(free), 2)))]
    rng <- function(i) { r <- range(unlist(lapply(fos, function(f) f$cd[[paste0("X", i)]])), na.rm=TRUE); c(min(-1, r[1]), max(1, r[2])) }
    cols <- c("#4C78A8", "#E45756", "#54A24B", "#B279A2", "#F58518", "#72B7B2", "#9D755D", "#BAB0AC")
    np <- length(pairs)
    .fa_try_ip(.fa_ip_overlay(fos, pars, opt, pairs))
    .doe_submit_plot({
        op <- par(mfrow=c(1, np), mar=c(4.5, 4.5, 2.8, 1), oma=c(4, 0, 2.5, 0), family="sans"); on.exit(par(op))
        for (pr in pairs) {
            a <- pr[1]; b <- pr[2]
            ga <- seq(rng(a)[1], rng(a)[2], length.out=81); gb <- seq(rng(b)[1], rng(b)[2], length.out=81)
            g  <- expand.grid(ga, gb)
            nd <- best$nd[rep(1, nrow(g)), , drop=FALSE]
            nd[[paste0("X", a)]] <- g[[1]]; nd[[paste0("X", b)]] <- g[[2]]
            if (opt$uses_ctpt) nd$CtPt <- 0
            za <- coding$info[[a]]; zb <- coding$info[[b]]
            xa <- za$mid + ga * za$half; xb <- zb$mid + gb * zb$half
            ok <- rep(TRUE, nrow(g))
            plot(range(xa), range(xb), type="n", xlab=za$name, ylab=zb$name, main=paste(za$name, "x", zb$name), cex.main=0.95)
            Z <- list()
            for (k in seq_along(fos)) {
                y <- fa_predict_orig(fos[[k]], nd); p <- pars[[k]]
                lo <- if (p$goal == "minimize") -Inf else p$L; hi <- if (p$goal == "maximize") Inf else p$U
                ok <- ok & y >= lo & y <= hi
                Z[[k]] <- matrix(y, 81, 81)
            }
            image(xa, xb, matrix(as.numeric(ok), 81, 81), col=c("grey85", "white"), add=TRUE, breaks=c(-0.5, 0.5, 1.5))
            for (k in seq_along(fos)) {
                p <- pars[[k]]; lv <- na.omit(c(if (p$goal != "minimize") p$L, if (p$goal != "maximize") p$U))
                if (length(lv)) contour(xa, xb, Z[[k]], levels=lv, add=TRUE, col=cols[(k - 1) %% 8 + 1], lwd=2, labcex=0.7)
            }
            points(za$mid + best$xnum[match(a, opt$num)] * za$half, zb$mid + best$xnum[match(b, opt$num)] * zb$half,
                   pch=4, cex=1.6, lwd=2)
            box()
        }
        mtext(gtxt("Overlaid Contours: white = every response within its limits; x = optimal setting (other factors at the optimum)"),
              outer=TRUE, line=0.8, font=2, cex=0.9)
        par(xpd=NA)
        legend("bottom", inset=c(0, -0.02), horiz=TRUE, bty="n", lwd=2, col=cols[(seq_along(fos) - 1) %% 8 + 1],
               legend=vapply(fos, function(f) f$resp, character(1)), cex=0.85, xpd=NA)
    }, width=max(700, 520 * np), height=560)
}

# Prediction interval for the MEAN of n confirmation runs at the optimum
fa_display_confirm <- function(fos, opt, n, conf=0.95) {
    if (!length(opt$solutions)) return(invisible(NULL))
    c1 <- opt$solutions[[1]]
    rows <- lapply(fos, function(f) {
        ps <- fa_predict(f, c1$nd, se=TRUE)
        if (f$df_error < 1) return(data.frame("", "", stringsAsFactors=FALSE))
        tq <- qt(1 - (1 - conf) / 2, f$df_error)
        pi <- ps$fit + c(-1, 1) * tq * sqrt(ps$se^2 + f$mse / n)
        pi <- sort(fa_untransform(f, pi))
        data.frame(.fa_num(fa_untransform(f, ps$fit), 5), paste0("(", .fa_num(pi[1], 5), ", ", .fa_num(pi[2], 5), ")"),
                   stringsAsFactors=FALSE)
    })
    df <- do.call(rbind, rows); names(df) <- c(gtxt("Predicted"), gtxtf("%s%% interval for the average of %d runs", format(100 * conf), n))
    .fa_show(df, gtxtf("Confirmation Runs (n = %d) at the Optimal Settings", n), "DOECONFIRM",
             outline=gtxt("Confirmation Runs"), rowlabels=vapply(fos, function(f) f$resp, character(1)),
             caption=gtxt("If the average of the confirmation runs falls inside this interval, the model's prediction is confirmed; if not, the model does not describe the process well at these settings."))
    invisible(NULL)
}

# Monte Carlo robustness of the optimum: numeric settings vary with a
# standard deviation of `sdpct` percent of their half-range (setting error),
# and each simulated run adds residual noise (model S).
fa_display_robust <- function(fos, pars, opt, sdpct=5, warns, nsim=2000) {
    if (!length(opt$solutions)) return(invisible(NULL))
    sdpct <- suppressWarnings(as.numeric(unlist(sdpct))[1]); if (!is.finite(sdpct) || sdpct <= 0) sdpct <- 5
    best <- opt$solutions[[1]]; coding <- fos[[1]]$coding
    if (opt$uses_ctpt) {
        warns$warn(gtxt("Robustness check skipped: a center-point (curvature) model cannot predict between the corners and the center point."), dostop=FALSE)
        return(invisible(NULL))
    }
    set.seed(20260923)
    nd <- best$nd[rep(1, nsim), , drop=FALSE]
    for (j in seq_along(opt$num)) {
        i <- opt$num[j]
        if (!is.null(opt$holds[[as.character(i)]])) next
        r <- range(unlist(lapply(fos, function(f) f$cd[[paste0("X", i)]])), na.rm=TRUE); r <- c(min(-1, r[1]), max(1, r[2]))
        nd[[paste0("X", i)]] <- pmin(r[2], pmax(r[1], best$xnum[j] + rnorm(nsim, 0, sdpct / 100)))
    }
    P  <- sapply(fos, function(f) fa_predict(f, nd)); P <- matrix(P, nrow=nsim)
    Yr <- sapply(seq_along(fos), function(k) fa_untransform(fos[[k]], P[, k] + rnorm(nsim, 0, if (is.finite(fos[[k]]$s)) fos[[k]]$s else 0)))
    Yr <- matrix(Yr, nrow=nsim)
    Ps <- sapply(seq_along(fos), function(k) fa_untransform(fos[[k]], P[, k])); Ps <- matrix(Ps, nrow=nsim)
    within <- sapply(seq_along(fos), function(k) {
        p <- pars[[k]]; y <- Yr[, k]
        lo <- if (p$goal == "minimize") -Inf else p$L; hi <- if (p$goal == "maximize") Inf else p$U
        y >= lo & y <= hi
    }); within <- matrix(within, nrow=nsim)
    D <- apply(Ps, 1, function(pr) .fa_composite(pr, pars)$D)
    rows <- lapply(seq_along(fos), function(k) data.frame(
        .fa_num(mean(Ps[, k]), 5), .fa_num(sd(Ps[, k]), 3), .fa_num(sd(Yr[, k]), 3),
        paste0("(", .fa_num(quantile(Yr[, k], 0.05), 5), ", ", .fa_num(quantile(Yr[, k], 0.95), 5), ")"),
        paste0(formatC(100 * mean(within[, k]), digits=1, format="f"), "%"), stringsAsFactors=FALSE))
    df <- do.call(rbind, rows)
    last <- data.frame(.fa_num(mean(D), 4), .fa_num(sd(D), 3), "", "",
        paste0(formatC(100 * mean(apply(within, 1, all)), digits=1, format="f"), "%"), stringsAsFactors=FALSE)
    names(last) <- names(df)
    df <- rbind(df, last)
    names(df) <- c(gtxt("Mean prediction"), gtxt("SD from setting error"), gtxt("SD incl. run-to-run"),
                   gtxt("90% range of runs"), gtxt("Runs within limits"))
    .fa_show(df, gtxt("Robustness of the Optimal Settings"), "DOEROBUST", outline=gtxt("Robustness"),
             rowlabels=c(vapply(fos, function(f) f$resp, character(1)), gtxt("Composite D / all responses")),
             caption=gtxtf("%d simulated runs; each numeric setting varies with SD = %s%% of its half-range around the optimum (held factors fixed), plus residual variation (model S). A setting is robust when the SD from setting error is small and most runs stay within the limits.", nsim, format(sdpct)))
    invisible(NULL)
}


# ════════════════════════════════════════════════════════════════════════════
# MIXTURE DESIGNS: simplex-lattice / simplex-centroid generation and Scheffe
# model analysis (Cornell, "Experiments with Mixtures")
# ════════════════════════════════════════════════════════════════════════════

# Simplex-lattice {q, m}: every blend whose proportions are multiples of 1/m.
# Simplex-centroid: the centroid of every non-empty subset of components.
# augment=TRUE adds the q axial blends and the overall centroid (if absent).
generate_simplex <- function(spec, variables, type="lattice", degree=2, augment=FALSE,
                             mixturesum=1, warns) {
    q <- length(variables)
    if (q < 2) warns$warn(gtxt("A mixture design needs at least 2 components."), dostop=TRUE)
    if (q > 12) warns$warn(gtxt("Simplex designs are limited to 12 components."), dostop=TRUE)
    if (type == "lattice") {
        m <- suppressWarnings(as.integer(degree)); if (!is.finite(m) || m < 1) m <- 2
        if (m > 10) warns$warn(gtxt("Lattice degree must be between 1 and 10."), dostop=TRUE)
        pts <- as.matrix(expand.grid(rep(list(0:m), q)))
        pts <- pts[rowSums(pts) == m, , drop=FALSE] / m
        # conventional order: vertices first, then by number of nonzero components
        pts <- pts[order(rowSums(pts > 0), -apply(pts, 1, max)), , drop=FALSE]
    } else {
        subs <- unlist(lapply(seq_len(q), function(k) combn(q, k, simplify=FALSE)), recursive=FALSE)
        pts <- t(vapply(subs, function(s) { v <- rep(0, q); v[s] <- 1 / length(s); v }, numeric(q)))
    }
    ptype <- apply(pts, 1, function(r) if (sum(r > 0) == 1) 1 else if (sum(r > 0) == q && max(abs(r - 1/q)) < 1e-12) 0 else 2)
    if (isTRUE(augment)) {
        ax <- t(vapply(seq_len(q), function(i) { v <- rep(1 / (2 * q), q); v[i] <- (q + 1) / (2 * q); v }, numeric(q)))
        pts <- rbind(pts, ax); ptype <- c(ptype, rep(-1, q))
        if (!any(apply(pts, 1, function(r) max(abs(r - 1/q)) < 1e-12))) { pts <- rbind(pts, rep(1/q, q)); ptype <- c(ptype, 0) }
    }
    lows <- suppressWarnings(as.numeric(spec$lows)); lows[!is.finite(lows)] <- 0
    total <- suppressWarnings(as.numeric(mixturesum)); if (!is.finite(total) || total <= 0) total <- 1
    if (sum(lows) >= total) warns$warn(gtxt("The component lower bounds add up to the mixture total or more; no blends are possible."), dostop=TRUE)
    real <- sweep(pts * (total - sum(lows)), 2, lows, "+")
    df <- as.data.frame(real); names(df) <- as.character(variables)
    df <- cbind(Reps=1, df, PtType=ptype)
    list(design=df, D=NA, A=NA, Ge=NA, Dea=NA)
}

# ── Mixture analysis ─────────────────────────────────────────────────────────
.fa_mix_terms <- function(q, model) {
    model <- tolower(model)
    tt <- lapply(seq_len(q), function(i) list(type="lin", idx=i))
    if (model %in% c("quadratic","specialcubic","fullcubic","cubic") && q >= 2)
        tt <- c(tt, lapply(combn(q, 2, simplify=FALSE), function(p) list(type="quad", idx=p)))
    if (model %in% c("fullcubic","cubic") && q >= 2)
        tt <- c(tt, lapply(combn(q, 2, simplify=FALSE), function(p) list(type="cubicdiff", idx=p)))
    if (model %in% c("specialcubic","fullcubic","cubic") && q >= 3)
        tt <- c(tt, lapply(combn(q, 3, simplify=FALSE), function(p) list(type="spcubic", idx=p)))
    tt
}
.fa_mix_cols <- function(P, tt, Z=NULL) {
    X <- sapply(tt, function(t) switch(t$type,
        lin=P[, t$idx], quad=P[, t$idx[1]] * P[, t$idx[2]],
        cubicdiff=P[, t$idx[1]] * P[, t$idx[2]] * (P[, t$idx[1]] - P[, t$idx[2]]),
        spcubic=P[, t$idx[1]] * P[, t$idx[2]] * P[, t$idx[3]]))
    X <- matrix(X, nrow=nrow(P))
    if (!is.null(Z) && ncol(Z)) X <- cbind(X, Z)
    X
}
.fa_mix_label <- function(t, nm) switch(t$type,
    lin=nm[t$idx], quad=paste(nm[t$idx], collapse="*"),
    cubicdiff=paste0(nm[t$idx[1]], "*", nm[t$idx[2]], "*(", nm[t$idx[1]], "-", nm[t$idx[2]], ")"),
    spcubic=paste(nm[t$idx], collapse="*"))

fa_mix_fit <- function(y, P, tt, comps, Z=NULL, znames=character(0), resp, warns) {
    ok <- is.finite(y) & stats::complete.cases(P) & (if (is.null(Z)) TRUE else stats::complete.cases(Z))
    y <- y[ok]; P <- P[ok, , drop=FALSE]; if (!is.null(Z)) Z <- Z[ok, , drop=FALSE]
    removed <- character(0)
    repeat {
        X <- .fa_mix_cols(P, tt, Z)
        if (qr(X, tol=1e-7)$rank == ncol(X)) break
        bad <- NA
        for (j in 2:ncol(X)) if (qr(X[, 1:j, drop=FALSE], tol=1e-7)$rank < j) { bad <- j; break }
        if (is.na(bad) || bad > length(tt)) { warns$warn(gtxtf("%s: the mixture model could not be estimated.", resp), dostop=FALSE); return(NULL) }
        removed <- c(removed, .fa_mix_label(tt[[bad]], comps)); tt <- tt[-bad]
    }
    if (length(removed))
        warns$warn(gtxtf("%s: not estimable from this design and removed: %s", resp, paste(removed, collapse=", ")), dostop=FALSE)
    n <- nrow(X); p <- ncol(X)
    fit <- lm.fit(X, y); b <- fit$coefficients; res <- fit$residuals
    dfe <- n - p; sse <- sum(res^2); sst <- sum((y - mean(y))^2)
    mse <- if (dfe > 0) sse / dfe else NA_real_
    XtXi <- chol2inv(qr.R(qr(X)))
    se <- if (dfe > 0) sqrt(diag(XtXi) * mse) else rep(NA_real_, p)
    h <- rowSums((X %*% XtXi) * X)
    press <- if (all(h < 1 - 1e-10)) sum((res / (1 - h))^2) else NA_real_
    list(resp=resp, y=y, y_orig=y, P=P, Z=Z, znames=znames, tt=tt, X=X, coef=b, se=se, t=b/se,
         p=if (dfe > 0) 2 * pt(-abs(b / se), dfe) else rep(NA_real_, p),
         df_error=dfe, sse=sse, sst=sst, mse=mse, s=sqrt(mse), XtXi=XtXi, hat=h, press=press,
         fitted=as.vector(X %*% b), resid=res, n=n, lambda=NA_real_, removed=removed)
}

fa_mix_predict <- function(mo, P, Z=NULL, se=FALSE) {
    X <- .fa_mix_cols(P, mo$tt, if (!is.null(mo$Z)) (if (is.null(Z)) matrix(0, nrow(P), ncol(mo$Z)) else Z) else NULL)
    fit <- as.vector(X %*% mo$coef)
    if (!se) return(fit)
    list(fit=fit, se=sqrt(pmax(0, rowSums((X %*% mo$XtXi) * X))) * mo$s)
}

fa_mix_anova <- function(mo) {
    q <- sum(vapply(mo$tt, function(t) t$type == "lin", logical(1)))
    rows <- list()
    add <- function(src, df, ss, f=NA, p=NA) rows[[length(rows) + 1]] <<- data.frame(Source=src, DF=df, SS=ss,
        MS=if (df > 0) ss / df else NA, F=f, P=p, stringsAsFactors=FALSE)
    ftest <- function(ss, df) { if (mo$df_error > 0 && df > 0) { f <- (ss / df) / mo$mse; c(f, pf(f, df, mo$df_error, lower.tail=FALSE)) } else c(NA, NA) }
    ssr <- mo$sst - mo$sse; dfr <- ncol(mo$X) - 1
    ft <- ftest(ssr, dfr); add(gtxt("Regression"), dfr, ssr, ft[1], ft[2])
    types <- vapply(mo$tt, function(t) t$type, character(1))
    # linear blending: are the pure-component responses different?
    lin <- which(types == "lin")
    Xr <- cbind(1, mo$X[, -lin, drop=FALSE])
    ss <- .fa_sse(Xr, mo$y) - mo$sse; ft <- ftest(ss, q - 1)
    add(gtxt("  Linear"), q - 1, ss, ft[1], ft[2])
    lab <- c(quad=gtxt("  Quadratic"), cubicdiff=gtxt("  Full cubic"), spcubic=gtxt("  Special cubic"))
    for (ty in c("quad","spcubic","cubicdiff")) {
        idx <- which(types == ty); if (!length(idx)) next
        ss <- .fa_sse(mo$X[, -idx, drop=FALSE], mo$y) - mo$sse; ft <- ftest(ss, length(idx))
        add(lab[[ty]], length(idx), ss, ft[1], ft[2])
        for (i in idx) { s1 <- .fa_sse(mo$X[, -i, drop=FALSE], mo$y) - mo$sse; f1 <- ftest(s1, 1)
            add(paste0("    ", .fa_mix_label(mo$tt[[i]], mo$comps)), 1, s1, f1[1], f1[2]) }
    }
    if (length(mo$znames)) {
        zi <- (length(mo$tt) + 1):ncol(mo$X)
        ss <- .fa_sse(mo$X[, -zi, drop=FALSE], mo$y) - mo$sse; ft <- ftest(ss, length(zi))
        add(gtxt("  Process variables"), length(zi), ss, ft[1], ft[2])
    }
    add(gtxt("Error"), mo$df_error, mo$sse)
    key <- apply(round(cbind(mo$P, if (!is.null(mo$Z)) mo$Z), 8), 1, paste, collapse="|")
    g <- split(mo$y, key); pedf <- sum(lengths(g) - 1)
    if (pedf > 0 && mo$df_error - pedf > 0) {
        pess <- sum(vapply(g, function(v) sum((v - mean(v))^2), numeric(1)))
        lofdf <- mo$df_error - pedf; lofss <- mo$sse - pess
        f <- (lofss / lofdf) / (pess / pedf)
        add(gtxt("  Lack-of-Fit"), lofdf, lofss, f, if (pess > 0) pf(f, lofdf, pedf, lower.tail=FALSE) else NA)
        add(gtxt("  Pure Error"), pedf, pess)
    }
    add(gtxt("Total"), mo$n - 1, mo$sst)
    out <- do.call(rbind, rows); out$MS[out$Source == gtxt("Total")] <- NA
    out
}

fa_run_mixture <- function(data, factors, responses, warns, opts, alpha=0.05, conf=0.95) {
    flags <- as.logical(unlist(opts$mixflags))
    if (!length(flags) || all(!flags, na.rm=TRUE)) flags <- vapply(factors, function(f) is.numeric(data[[f]]), logical(1))
    flags <- rep(flags, length.out=length(factors)); flags[is.na(flags)] <- FALSE
    comps <- factors[flags]; procs <- factors[!flags]
    if (length(comps) < 2) stop(gtxt("A mixture analysis needs at least 2 mixture components (MIXTURES=YES)."), call.=FALSE)
    A <- as.matrix(data[, comps, drop=FALSE]); storage.mode(A) <- "double"
    tot <- rowSums(A)
    if (any(!is.finite(tot) | tot <= 0)) stop(gtxt("Every run needs positive, non-missing component amounts."), call.=FALSE)
    total <- stats::median(tot)
    if (max(abs(tot - stats::median(tot))) > 1e-4 * max(1, stats::median(tot)))
        warns$warn(gtxt("Component totals differ between runs; the analysis uses proportions (each run divided by its total)."), dostop=FALSE)
    Pr <- A / tot
    L  <- apply(Pr, 2, min); U <- apply(Pr, 2, max)
    mode <- tolower(as.character(unlist(opts$mixcomps %||% "auto"))[1])
    mode <- switch(mode, item_mc_a="auto", item_mc_b="pseudo", item_mc_c="proportions", mode)
    use_pseudo <- (mode == "pseudo") || (mode == "auto" && any(L > 1e-9) && sum(L) < 1 - 1e-9)
    if (!use_pseudo) L <- rep(0, length(comps))
    to_p  <- function(Pr) sweep(Pr, 2, L, "-") / (1 - sum(L))
    from_p <- function(P) sweep(P * (1 - sum(L)), 2, L, "+")
    P <- to_p(Pr)
    Z <- NULL; zinfo <- list()
    if (length(procs)) {
        Z <- sapply(procs, function(f) {
            x <- data[[f]]
            if (is.numeric(x)) { lo <- min(x, na.rm=TRUE); hi <- max(x, na.rm=TRUE); zinfo[[f]] <<- list(lo=lo, hi=hi); (x - (lo + hi) / 2) / ((hi - lo) / 2) }
            else { lv <- unique(as.character(x)); zinfo[[f]] <<- list(levels=lv); ifelse(as.character(x) == lv[1], -1, 1) }
        })
        Z <- matrix(Z, nrow=nrow(data)); colnames(Z) <- procs
    }
    model <- tolower(as.character(unlist(opts$mixmodel %||% "quadratic"))[1])
    model <- switch(model, item_mm_a="linear", item_mm_b="quadratic", item_mm_c="specialcubic", item_mm_d="fullcubic", model)
    if (!model %in% c("linear","quadratic","specialcubic","fullcubic","cubic")) model <- "quadratic"
    tt <- .fa_mix_terms(length(comps), model)
    mos <- list()
    for (r in responses) {
        mo <- fa_mix_fit(suppressWarnings(as.numeric(data[[r]])), P, tt, comps, Z, procs, r, warns)
        if (is.null(mo)) next
        mo$comps <- comps; mo$L <- L; mo$U <- U; mo$pseudo <- use_pseudo; mo$total <- total
        mo$to_p <- to_p; mo$from_p <- from_p; mo$zinfo <- zinfo
        mos[[r]] <- mo
        StartProcedure(gtxtf("Mixture Analysis: %s", r), "STATSDOEMIXTURE")
        tryCatch({
            if (!isFALSE(opts$tables)) {
                if (!isFALSE(opts$interpret))
                    tryCatch(.fa_show_interpret(fa_interpret_mixture(mo, alpha), gtxtf("Summary of Results: %s", r)), error=function(e) NULL)
                ci <- data.frame(.fa_num(L * total, 5), .fa_num(U * total, 5), stringsAsFactors=FALSE)
                names(ci) <- c(gtxt("Lowest in data"), gtxt("Highest in data"))
                .fa_show(ci, gtxtf("Mixture Components: %s", r), "DOEMIXCOMP", outline=gtxt("Mixture Components"),
                         rowlabels=comps,
                         caption=if (use_pseudo) gtxtf("Coefficients are in pseudo-components: (proportion - lower bound) / %s.", .fa_num(1 - sum(L), 5))
                                 else gtxt("Coefficients are in component proportions (each blend scaled to sum to 1)."))
                lab <- c(vapply(mo$tt, .fa_mix_label, character(1), nm=comps), mo$znames)
                lin <- c(vapply(mo$tt, function(t) t$type == "lin", logical(1)), rep(FALSE, length(mo$znames)))
                ct <- data.frame(.fa_num(mo$coef), .fa_num(mo$se), ifelse(lin, "", .fa_fix(mo$t, 2)), ifelse(lin, "", .fa_pv(mo$p)),
                                 stringsAsFactors=FALSE)
                names(ct) <- c(gtxt("Coefficient"), gtxt("SE Coef."), gtxt("t"), gtxt("Sig."))
                .fa_show(ct, gtxtf("Mixture Regression Coefficients (%s model): %s", model, r), "DOEMIXCOEF",
                         outline=gtxt("Mixture Coefficients"), rowlabels=lab,
                         caption=gtxt("Scheffe model without a constant. A linear coefficient is the predicted response of the pure component, so its t-test (against 0) is not shown; the Linear row of the ANOVA tests whether the components blend differently."))
                sm <- data.frame(.fa_num(mo$s), .fa_pct(1 - mo$sse / mo$sst),
                                 .fa_pct(if (mo$df_error > 0) 1 - (mo$sse / mo$df_error) / (mo$sst / (mo$n - 1)) else NA),
                                 .fa_pct(if (is.finite(mo$press)) max(0, 1 - mo$press / mo$sst) else NA), stringsAsFactors=FALSE)
                names(sm) <- c(gtxt("S"), gtxt("R-squared"), gtxt("Adj. R-squared"), gtxt("Pred. R-squared"))
                .fa_show(sm, gtxtf("Model Summary: %s", r), "DOEMODELSUMMARY", outline=gtxt("Model Summary"), rowlabels=r)
                an <- fa_mix_anova(mo)
                ad <- data.frame(an$DF, .fa_num(an$SS), .fa_num(an$MS), .fa_fix(an$F, 2), .fa_pv(an$P), stringsAsFactors=FALSE)
                names(ad) <- c(gtxt("df"), gtxt("Adj. SS"), gtxt("Adj. MS"), gtxt("F"), gtxt("Sig."))
                .fa_show(ad, gtxtf("Analysis of Variance for Mixture: %s", r), "DOEMIXANOVA", outline=gtxt("ANOVA"), rowlabels=an$Source)
            }
            if (isTRUE(opts$maineffects) || isTRUE(opts$traceplot)) fa_plot_mix_trace(mo)
            if (isTRUE(opts$contourplot) || isTRUE(opts$surfaceplot)) fa_plot_mix_ternary(mo)
            if (isTRUE(opts$residualplots) && mo$df_error > 0) fa_plot_residuals(mo)
        }, error=function(e) warns$warn(gtxtf("%s: mixture output error: %s", r, conditionMessage(e)), dostop=FALSE))
        tryCatch(spsspkg.EndProcedure(), error=function(e) NULL)
    }
    if (isTRUE(opts$optimize) && length(mos)) {
        StartProcedure(gtxt("Mixture Optimization"), "STATSDOEMIXOPT")
        tryCatch(fa_mix_optimize(mos, opts, warns, conf), error=function(e)
            warns$warn(gtxtf("Mixture optimizer error: %s", conditionMessage(e)), dostop=FALSE))
        tryCatch(spsspkg.EndProcedure(), error=function(e) NULL)
    }
    invisible(mos)
}

# reference blend = centroid of the design region (pseudo space)
.fa_mix_ref <- function(mo) { r <- colMeans(mo$P); r / sum(r) }

fa_plot_mix_trace <- function(mo) {
    q <- ncol(mo$P); ref <- .fa_mix_ref(mo)
    cols <- c("#4C78A8", "#E45756", "#54A24B", "#B279A2", "#F58518", "#72B7B2", "#9D755D", "#BAB0AC", "#EECA3B", "#FF9DA6", "#9ECAE9", "#6F4E7C")
    Pmin <- apply(mo$P, 2, min); Pmax <- apply(mo$P, 2, max)
    curves <- lapply(seq_len(q), function(i) {
        xi <- seq(Pmin[i], Pmax[i], length.out=60)
        P <- t(vapply(xi, function(v) { p <- ref * (1 - v) / (1 - ref[i]); p[i] <- v; p }, numeric(q)))
        ok <- apply(P, 1, function(p) all(p >= Pmin - 1e-9 & p <= Pmax + 1e-9))
        list(x=(xi - ref[i])[ok], y=fa_mix_predict(mo, P[ok, , drop=FALSE]))
    })
    .fa_try_ip(.fa_ip_mix_trace(mo, curves))
    .doe_submit_plot({
        op <- par(mar=c(5, 5, 4, 10), family="sans"); on.exit(par(op))
        yr <- range(unlist(lapply(curves, `[[`, "y")), na.rm=TRUE)
        xr <- range(unlist(lapply(curves, `[[`, "x")), na.rm=TRUE)
        plot(xr, yr, type="n", xlab=gtxt("Deviation from reference blend (component proportion, Cox direction)"),
             ylab=gtxtf("Fitted %s", mo$resp), main=gtxtf("Response Trace: %s", mo$resp), cex.main=1)
        grid(col=.fa_pal$grid); abline(v=0, lty=3, col="grey50")
        for (i in seq_len(q)) lines(curves[[i]]$x, curves[[i]]$y, lwd=2.2, col=cols[(i - 1) %% 12 + 1])
        par(xpd=TRUE)
        legend(par("usr")[2] * 1.03, par("usr")[4], legend=mo$comps, col=cols[(seq_len(q) - 1) %% 12 + 1], lwd=2.2, bty="n", cex=0.85)
    }, width=900, height=560)
}

fa_plot_mix_ternary <- function(mo) {
    q <- ncol(mo$P); ref <- .fa_mix_ref(mo)
    trip <- 1:3; if (q < 3) return(invisible(NULL))
    rest <- setdiff(seq_len(q), trip); srest <- sum(ref[rest])
    n <- 121
    gx <- seq(0, 1, length.out=n); gy <- seq(0, sqrt(3) / 2, length.out=n)
    G <- expand.grid(x=gx, y=gy)
    b3 <- G$y / (sqrt(3) / 2); b2 <- G$x - b3 / 2; b1 <- 1 - b2 - b3
    inside <- b1 >= -1e-9 & b2 >= -1e-9 & b3 >= -1e-9
    P <- matrix(0, nrow(G), q)
    P[, trip] <- cbind(b1, b2, b3) * (1 - srest)
    if (length(rest)) P[, rest] <- matrix(ref[rest], nrow(G), length(rest), byrow=TRUE)
    z <- rep(NA_real_, nrow(G)); z[inside] <- fa_mix_predict(mo, P[inside, , drop=FALSE])
    Z <- matrix(z, n, n)
    lab <- function(i) if (mo$pseudo) paste0(mo$comps[i], " (pseudo)") else mo$comps[i]
    .fa_try_ip(.fa_ip_mix_ternary(mo, gx, gy, Z, vapply(trip, lab, character(1)),
        mo$P[, trip, drop=FALSE] / pmax(1e-12, rowSums(mo$P[, trip, drop=FALSE]))))
    .doe_submit_plot({
        op <- par(mar=c(3, 2, 4, 2), family="sans"); on.exit(par(op))
        image(gx, gy, Z, col=grDevices::hcl.colors(20, "Blues 3", rev=TRUE), asp=1, axes=FALSE, xlab="", ylab="",
              main=gtxtf("Mixture Contours: %s%s", mo$resp, if (length(rest)) gtxt(" (other components at the reference blend)") else ""), cex.main=0.95)
        contour(gx, gy, Z, add=TRUE, col="grey20", labcex=0.75)
        polygon(c(0, 1, 0.5), c(0, 0, sqrt(3) / 2), border="black", lwd=1.5)
        text(0, -0.04, lab(trip[1]), cex=0.9); text(1, -0.04, lab(trip[2]), cex=0.9); text(0.5, sqrt(3) / 2 + 0.04, lab(trip[3]), cex=0.9)
        dp <- mo$P[, trip, drop=FALSE] / pmax(1e-12, rowSums(mo$P[, trip, drop=FALSE]))
        points(dp[, 2] + dp[, 3] / 2, dp[, 3] * sqrt(3) / 2, pch=19, cex=0.8, col="#E45756")
    }, width=760, height=700)
}

fa_mix_optimize <- function(mos, opts, warns, conf) {
    mo1 <- mos[[1]]; q <- ncol(mo1$P); comps <- mo1$comps
    pars <- fa_opt_params(mos, opts$goals, opts$lowers, opts$targets, opts$uppers, opts$weights, opts$importance, warns)
    Pmin <- apply(mo1$P, 2, min); Pmax <- apply(mo1$P, 2, max)
    zfix <- if (!is.null(mo1$Z)) matrix(0, 1, ncol(mo1$Z)) else NULL
    Dof <- function(P) {
        Zm <- if (is.null(zfix)) NULL else zfix[rep(1, nrow(P)), , drop=FALSE]
        PR <- sapply(mos, function(m) fa_mix_predict(m, P, Zm)); PR <- matrix(PR, nrow=nrow(P))
        apply(PR, 1, function(pr) { e <- .fa_composite(pr, pars); e$D + 1e-6 * e$tb })
    }
    set.seed(20260923)
    E <- matrix(-log(runif(20000 * q)), ncol=q); S <- E / rowSums(E)
    S <- rbind(S, mo1$P)
    feas <- apply(S, 1, function(p) all(p >= Pmin - 1e-9 & p <= Pmax + 1e-9))
    S <- S[feas, , drop=FALSE]
    if (!nrow(S)) stop(gtxt("Mixture optimizer: no feasible blend inside the component ranges."), call.=FALSE)
    Dv <- Dof(S); starts <- order(-Dv)[seq_len(min(6, length(Dv)))]
    best <- NULL
    for (s in starts) {
        p0 <- S[s, ]
        obj <- function(w) {
            p <- exp(w - max(w)); p <- p / sum(p)
            pen <- sum(pmax(0, Pmin - p)^2 + pmax(0, p - Pmax)^2) * 1e4
            -(Dof(matrix(p, 1)) - pen)
        }
        o <- tryCatch(optim(log(pmax(p0, 1e-9)), obj, method="Nelder-Mead", control=list(maxit=800)), error=function(e) NULL)
        cand <- if (!is.null(o)) { p <- exp(o$par - max(o$par)); p / sum(p) } else p0
        if (!all(cand >= Pmin - 1e-6 & cand <= Pmax + 1e-6)) cand <- p0
        v <- Dof(matrix(cand, 1))
        if (is.null(best) || v > best$v) best <- list(p=cand, v=v)
    }
    pbest <- matrix(best$p, 1)
    preds <- vapply(mos, function(m) fa_mix_predict(m, pbest, zfix), numeric(1))
    comp <- .fa_composite(preds, pars)
    real <- as.vector(mo1$from_p(pbest)) * mo1$total
    gl <- c(maximize=gtxt("Maximize"), minimize=gtxt("Minimize"), target=gtxt("Target"))
    pt <- data.frame(vapply(pars, function(p) gl[[p$goal]], character(1)),
                     .fa_num(vapply(pars, function(p) p$L, numeric(1))), .fa_num(vapply(pars, function(p) p$T, numeric(1))),
                     .fa_num(vapply(pars, function(p) p$U, numeric(1))), stringsAsFactors=FALSE)
    names(pt) <- c(gtxt("Goal"), gtxt("Lower"), gtxt("Target"), gtxt("Upper"))
    .fa_show(pt, gtxt("Optimization Criteria"), "DOEOPTCRIT", outline=gtxt("Optimization Criteria"),
             rowlabels=vapply(pars, function(p) p$resp, character(1)))
    sol <- data.frame(.fa_num(real, 5), .fa_num(real / mo1$total, 5), stringsAsFactors=FALSE)
    names(sol) <- c(gtxt("Amount"), gtxt("Proportion"))
    .fa_show(sol, gtxtf("Optimal Blend (composite D = %s)", .fa_fix(comp$D, 4)), "DOEMIXSOL",
             outline=gtxt("Optimal Blend"), rowlabels=comps,
             caption=gtxt("Search restricted to the range of each component in the data (process variables at their middle setting)."))
    pr <- do.call(rbind, lapply(seq_along(mos), function(k) {
        m <- mos[[k]]; ps <- fa_mix_predict(m, pbest, zfix, se=TRUE)
        if (m$df_error > 0) { tq <- qt(1 - (1 - conf) / 2, m$df_error)
            ci <- ps$fit + c(-1, 1) * tq * ps$se; pi <- ps$fit + c(-1, 1) * tq * sqrt(ps$se^2 + m$mse) } else ci <- pi <- c(NA, NA)
        data.frame(.fa_num(ps$fit, 5), .fa_num(ps$se, 4), paste0("(", .fa_num(ci[1], 5), ", ", .fa_num(ci[2], 5), ")"),
                   paste0("(", .fa_num(pi[1], 5), ", ", .fa_num(pi[2], 5), ")"), .fa_fix(comp$d[k], 4), stringsAsFactors=FALSE)
    }))
    names(pr) <- c(gtxt("Fit"), gtxt("SE Fit"), gtxtf("%s%% CI", format(100 * conf)), gtxtf("%s%% PI", format(100 * conf)), gtxt("Desirability d"))
    .fa_show(pr, gtxt("Predicted Responses at the Optimal Blend"), "DOEOPTPRED", outline=gtxt("Predictions at Optimum"),
             rowlabels=vapply(mos, function(m) m$resp, character(1)))
    invisible(list(p=pbest, D=comp$D, preds=preds))
}


# ════════════════════════════════════════════════════════════════════════════
# TAGUCHI (ROBUST PARAMETER) DESIGN ANALYSIS: signal-to-noise ratios,
# response tables with delta and rank, main-effects plots, additive prediction
# ════════════════════════════════════════════════════════════════════════════
fa_run_taguchi <- function(data, factors, responses, warns, opts, alpha=0.05) {
    Y <- as.matrix(data[, responses, drop=FALSE]); storage.mode(Y) <- "double"
    nrep <- ncol(Y)
    sn <- tolower(as.character(unlist(opts$sntype %||% ""))[1])
    sn <- switch(sn, item_sn_a="larger", item_sn_b="smaller", item_sn_c="nominal", item_sn_d="nominal2", sn)
    if (!nzchar(sn)) sn <- if (nrep >= 2) "nominal" else "larger"
    if (!sn %in% c("larger","smaller","nominal","nominal2")) {
        warns$warn(gtxtf("SNTYPE=%s is not recognized; LARGER used.", sn), dostop=FALSE); sn <- "larger" }
    if (sn %in% c("nominal","nominal2") && nrep < 2)
        stop(gtxt("Nominal-is-best S/N ratios need at least two response columns (replicates) per run."), call.=FALSE)
    if (sn == "larger" && any(Y == 0, na.rm=TRUE))
        stop(gtxt("Larger-is-better S/N ratio is undefined for a response of 0."), call.=FALSE)
    ybar <- rowMeans(Y, na.rm=TRUE)
    sdev <- if (nrep >= 2) apply(Y, 1, sd, na.rm=TRUE) else rep(NA_real_, nrow(Y))
    SN <- switch(sn,
        larger   = -10 * log10(rowMeans(1 / Y^2, na.rm=TRUE)),
        smaller  = -10 * log10(rowMeans(Y^2, na.rm=TRUE)),
        nominal  = 10 * log10(ybar^2 / sdev^2),
        nominal2 = -10 * log10(sdev^2))
    snlab <- switch(sn, larger=gtxt("Larger is better: -10 log10(mean(1/Y^2))"),
                    smaller=gtxt("Smaller is better: -10 log10(mean(Y^2))"),
                    nominal=gtxt("Nominal is best: 10 log10(mean^2 / s^2)"),
                    nominal2=gtxt("Nominal is best: -10 log10(s^2)"))
    ok <- is.finite(SN)
    if (sum(!ok)) warns$warn(gtxtf("%d run(s) have an undefined S/N ratio (e.g. zero standard deviation) and were excluded.", sum(!ok)), dostop=FALSE)
    lv <- lapply(factors, function(f) { x <- data[[f]]; if (is.numeric(x)) as.character(sort(unique(x[is.finite(x)]))) else unique(as.character(x[!is.na(x)])) })
    names(lv) <- factors
    xf <- lapply(factors, function(f) factor(as.character(data[[f]]), levels=lv[[f]])); names(xf) <- factors
    resp_table <- function(v) {
        M <- sapply(factors, function(f) { m <- tapply(v[ok], xf[[f]][ok], mean); m[lv[[f]]] })
        maxl <- max(lengths(lv))
        tab <- matrix("", maxl + 2, length(factors), dimnames=list(c(as.character(seq_len(maxl)), gtxt("Delta"), gtxt("Rank")), factors))
        deltas <- numeric(length(factors))
        for (j in seq_along(factors)) {
            m <- if (is.list(M)) M[[j]] else M[, j]
            tab[seq_along(m), j] <- .fa_num(m, 5)
            deltas[j] <- diff(range(m, na.rm=TRUE))
            tab[maxl + 1, j] <- .fa_num(deltas[j], 4)
        }
        tab[maxl + 2, ] <- as.character(rank(-deltas, ties.method="min"))
        list(tab=as.data.frame(tab, stringsAsFactors=FALSE), means=if (is.list(M)) M else lapply(seq_along(factors), function(j) M[, j]), deltas=deltas)
    }
    names_note <- paste(vapply(factors, function(f) paste0(f, ": ", paste(seq_along(lv[[f]]), "=", lv[[f]], collapse=", ")), character(1)), collapse="; ")
    StartProcedure(gtxt("Taguchi Analysis"), "STATSDOETAGUCHI")
    tryCatch({
        rs <- resp_table(SN); rm_ <- resp_table(ybar)
        if (!isFALSE(opts$tables) && !isFALSE(opts$interpret)) tryCatch({
            o <- order(-rs$deltas); bl <- vapply(seq_along(factors), function(j) lv[[j]][which.max(rs$means[[j]])], character(1))
            dm <- order(-rm_$deltas)
            li <- list(c(gtxt("Signal-to-noise"), gtxtf("%s has the largest effect on the S/N ratio (delta %s), then %s. Higher S/N = more robust (less sensitive to noise).",
                                                        factors[o[1]], .fa_num(rs$deltas[o[1]], 4), .fa_listx(factors[o[-1]]))),
                       c(gtxt("Best settings"), gtxtf("For the highest S/N: %s.", paste(paste0(factors, " = ", bl), collapse=", "))))
            if (sn %in% c("nominal", "nominal2")) {
                adj <- dm[!(dm %in% o[seq_len(max(1, floor(length(o) / 2)))])]
                li[[3]] <- c(gtxt("Adjusting factor"), if (length(adj)) gtxtf("%s changes the mean more than the S/N ratio: use it to bring the mean to target.", factors[adj[1]])
                                                       else gtxt("No factor affects the mean without also affecting S/N."))
            }
            .fa_show_interpret(li, gtxt("Summary of Results: Taguchi"))
        }, error=function(e) NULL)
        if (!isFALSE(opts$tables)) {
            .fa_show(rs$tab, gtxt("Response Table for Signal-to-Noise Ratios"), "DOETAGSN", outline=gtxt("S/N Response Table"),
                     rowlabels=rownames(rs$tab), caption=paste0(snlab, ". ", gtxt("Rank 1 = factor with the largest effect on S/N. Levels: "), names_note))
            .fa_show(rm_$tab, gtxt("Response Table for Means"), "DOETAGMEAN", outline=gtxt("Means Response Table"),
                     rowlabels=rownames(rm_$tab))
            if (nrep >= 2) {
                rsd <- resp_table(sdev)
                .fa_show(rsd$tab, gtxt("Response Table for Standard Deviations"), "DOETAGSD", outline=gtxt("StDev Response Table"),
                         rowlabels=rownames(rsd$tab))
            }
            # main-effects ANOVA of the S/N ratio (when there are error df)
            dfr <- data.frame(SN=SN, xf, check.names=FALSE)[ok, , drop=FALSE]
            fml <- stats::as.formula(paste("SN ~", paste(sprintf("`%s`", factors), collapse=" + ")))
            fit <- tryCatch(stats::lm(fml, data=dfr, contrasts=setNames(rep(list("contr.sum"), length(factors)), factors)), error=function(e) NULL)
            if (!is.null(fit) && fit$df.residual > 0) {
                X <- stats::model.matrix(fit); asg <- attr(X, "assign"); y <- dfr$SN; sse <- sum(stats::resid(fit)^2)
                mse <- sse / fit$df.residual
                rows <- lapply(seq_along(factors), function(j) {
                    cols <- which(asg == j); ss <- .fa_sse(X[, -cols, drop=FALSE], y) - sse; df <- length(cols)
                    f <- (ss / df) / mse
                    data.frame(df, .fa_num(ss), .fa_num(ss / df), .fa_fix(f, 2), .fa_pv(stats::pf(f, df, fit$df.residual, lower.tail=FALSE)), stringsAsFactors=FALSE)
                })
                an <- do.call(rbind, rows)
                erow <- data.frame(fit$df.residual, .fa_num(sse), .fa_num(mse), "", "", stringsAsFactors=FALSE)
                names(erow) <- names(an)
                an <- rbind(an, erow)
                names(an) <- c(gtxt("df"), gtxt("Adj. SS"), gtxt("Adj. MS"), gtxt("F"), gtxt("Sig."))
                .fa_show(an, gtxt("Analysis of Variance for S/N Ratios"), "DOETAGANOVA", outline=gtxt("S/N ANOVA"),
                         rowlabels=c(factors, gtxt("Error")),
                         caption=gtxt("Main-effects model of the S/N ratio with each factor treated as categorical."))
            }
            # prediction at the best S/N level of each factor (additive model)
            best <- vapply(seq_along(factors), function(j) which.max(rs$means[[j]]), integer(1))
            gsn <- mean(SN[ok]); gm <- mean(ybar[ok])
            psn <- gsn + sum(vapply(seq_along(factors), function(j) rs$means[[j]][best[j]] - gsn, numeric(1)))
            pm  <- gm  + sum(vapply(seq_along(factors), function(j) rm_$means[[j]][best[j]] - gm, numeric(1)))
            pr <- data.frame(t(c(vapply(seq_along(factors), function(j) lv[[j]][best[j]], character(1)), .fa_num(psn, 5), .fa_num(pm, 5))),
                             stringsAsFactors=FALSE)
            names(pr) <- c(factors, gtxt("Predicted S/N"), gtxt("Predicted mean"))
            .fa_show(pr, gtxt("Predicted Result at the Best Settings"), "DOETAGPRED", outline=gtxt("Taguchi Prediction"),
                     rowlabels=gtxt("Best S/N"),
                     caption=gtxt("Each factor set to the level with the highest mean S/N; prediction assumes the factor effects add (no interactions). For nominal-is-best, use a factor with a large effect on the mean but little effect on S/N to adjust the mean to target, then confirm with a verification run."))
        }
        if (!isFALSE(opts$maineffects) || isTRUE(opts$createplots)) {
            for (what in c("sn", "mean")) {
                tab <- if (what == "sn") rs else rm_
                .fa_try_ip(.fa_ip_taguchi(factors, lv, tab, what, mean(if (what == "sn") SN[ok] else ybar[ok]), snlab))
                .doe_submit_plot({
                    k <- length(factors); nc <- min(k, 4); nr <- ceiling(k / nc)
                    op <- par(mfrow=c(nr, nc), mar=c(4, 4.2, 2.5, 0.8), oma=c(0, 0, 3, 0), family="sans"); on.exit(par(op))
                    all <- unlist(tab$means); yr <- range(all, na.rm=TRUE); yr <- yr + c(-0.08, 0.08) * diff(yr)
                    gmv <- mean(if (what == "sn") SN[ok] else ybar[ok])
                    for (j in seq_len(k)) {
                        m <- tab$means[[j]]
                        plot(seq_along(m), m, type="b", pch=19, lwd=2, col=if (what == "sn") .fa_pal$sig else .fa_pal$line1,
                             ylim=yr, xaxt="n", xlab=factors[j], ylab=if (j %% nc == 1 || nc == 1) (if (what == "sn") gtxt("Mean of S/N ratios") else gtxt("Mean of means")) else "",
                             main=factors[j], cex.main=0.95, xlim=c(0.7, length(m) + 0.3))
                        axis(1, at=seq_along(m), labels=lv[[j]]); grid(col=.fa_pal$grid); abline(h=gmv, lty=3, col="grey55")
                    }
                    mtext(if (what == "sn") gtxtf("Main Effects for S/N Ratios (%s)", snlab) else gtxt("Main Effects for Means"),
                          outer=TRUE, line=1, font=2)
                }, width=min(1400, 300 * min(length(factors), 4) + 120), height=300 * ceiling(length(factors) / 4) + 110)
            }
        }
    }, error=function(e) warns$warn(gtxtf("Taguchi analysis error: %s", conditionMessage(e)), dostop=FALSE))
    tryCatch(spsspkg.EndProcedure(), error=function(e) NULL)
    invisible(list(SN=SN, mean=ybar))
}

# ════════════════════════════════════════════════════════════════════════════
# SPSS COMMAND PARSER
# ════════════════════════════════════════════════════════════════════════════

Run <- function(args) {
    # Force C numeric locale so jsonlite/plotly-embedded chart data always uses
    # "." not "," as decimal separator (European/Latin-American locales default
    # to ",", which breaks the JSON embedded in the interactive HTML report -
    # JS throws "Unexpected number" and charts render blank). Only LC_NUMERIC
    # is touched; display language, SPSS UI, and file paths are unaffected.
    old_lc_num <- tryCatch(suppressWarnings(Sys.getlocale("LC_NUMERIC")), error=function(e) "C")
    suppressWarnings(Sys.setlocale("LC_NUMERIC", "C"))
    on.exit(tryCatch(suppressWarnings(Sys.setlocale("LC_NUMERIC", old_lc_num)), error=function(e) NULL), add=TRUE)

    cmdname <- args[[1]]
    args    <- args[[2]]
    oobj    <- spsspkg.Syntax(templ=list(
        spsspkg.Template("GENERATEDESIGN", subc="", ktype="bool",    var="generatedesign"),
        spsspkg.Template("VARNAMES",    subc="", ktype="varname", var="varnames",   islist=TRUE),
        spsspkg.Template("FORMULA",     subc="", ktype="literal", var="frml"),
        spsspkg.Template("FACTORS",     subc="", ktype="str",     var="factors",    islist=TRUE),
        spsspkg.Template("LEVELS",      subc="", ktype="int",     var="nlevels",    islist=TRUE),
        spsspkg.Template("MIXTURES",    subc="", ktype="str",     var="mixtures",   islist=TRUE),
        spsspkg.Template("LOWS",        subc="", ktype="float",   var="lows",       islist=TRUE),
        spsspkg.Template("HIGHS",       subc="", ktype="float",   var="highs",      islist=TRUE),
        spsspkg.Template("CENTERS",     subc="", ktype="float",   var="centers",    islist=TRUE),
        spsspkg.Template("ROUNDTOS",    subc="", ktype="int",     var="roundtos",   islist=TRUE),
        spsspkg.Template("CONSTRAINTFUNC", subc="", ktype="literal", var="constraintfunc"),
        spsspkg.Template("MIXTURESUM",  subc="", ktype="float",   var="mixturesum"),
        spsspkg.Template("DESIGNTYPE",  subc="", ktype="str",     var="designtype",
            vallist=list("optimal","factorial","plackettburman","rsm","boxbehnken","ccd","taguchi","lhs","dsd","fullfactorial",
                        "simplexlattice","simplexcentroid")),
        spsspkg.Template("MODEL",       subc="", ktype="str",     var="model"),
        spsspkg.Template("TRIALS",      subc="", ktype="int",     var="ntrials"),
        spsspkg.Template("CONSTANT",    subc="", ktype="bool",    var="constant"),
        spsspkg.Template("RANDOMIZE",   subc="", ktype="bool",    var="randomize"),
        spsspkg.Template("BLOCKS",      subc="", ktype="int",     var="blocks",     vallist=list(1)),
        spsspkg.Template("REPLICATES",  subc="", ktype="int",     var="replicates", vallist=list(1)),
        spsspkg.Template("SCREENINGCENTERPTS",      subc="", ktype="bool", var="screeningcenterpts"),
        spsspkg.Template("SCREENINGCENTERPTSCOUNT", subc="", ktype="int",  var="screeningcenterptscount", vallist=list(1)),
        # ── Sequential DOE: augment an existing factorial to a CCD (additive,
        # OFF by default) ───────────────────────────────────────────────────
        spsspkg.Template("AUGMENTTOCCD",     subc="", ktype="bool", var="augmenttoccd"),
        spsspkg.Template("AUGMENTCENTERPTS", subc="", ktype="int",  var="augmentcenterpts", vallist=list(0)),
        spsspkg.Template("RESPONSEVAR", subc="", ktype="varname", var="responsevar", islist=TRUE),
        spsspkg.Template("ANALYZE",     subc="", ktype="bool",    var="analyze"),
        spsspkg.Template("CREATEPLOTS", subc="", ktype="bool",    var="createplots"),
        spsspkg.Template("OPTIMIZERESPONSE", subc="", ktype="bool",  var="optimizeresponse"),
        spsspkg.Template("OPTIMIZATIONGOAL", subc="", ktype="str",   var="optimizationgoal",
            vallist=list("maximize","minimize","target")),
        # Plural, parallel companion to OPTIMIZATIONGOAL -- one goal per
        # RESPONSEVAR entry when 2+ response variables are given (multi-
        # response optimization). Left empty by default, in which case every
        # response falls back to the single OPTIMIZATIONGOAL value, so
        # existing single-response syntax is completely unaffected.
        spsspkg.Template("OPTIMIZATIONGOALS", subc="", ktype="str", var="optimizationgoals", islist=TRUE,
            vallist=list("maximize","minimize","target")),
        # ADDITIVE: numeric target value(s), see optdesmc()'s own comment on
        # optimizationtarget/optimizationtargets for the full rationale.
        # Singular pairs with OPTIMIZATIONGOAL (single-response); plural
        # pairs with OPTIMIZATIONGOALS (multi-response), same space-separated
        # ordering convention as every other paired singular/plural keyword
        # in this file (RESPONSEVAR/OPTIMIZATIONGOALS, POWEREFFECTSIZES, etc).
        spsspkg.Template("OPTIMIZATIONTARGET",  subc="", ktype="float", var="optimizationtarget"),
        spsspkg.Template("OPTIMIZATIONTARGETS", subc="", ktype="float", var="optimizationtargets", islist=TRUE),
        spsspkg.Template("CRITERION",    subc="OPTIONS", ktype="str",  var="criterion"),
        spsspkg.Template("NUMCAND",      subc="OPTIONS", ktype="int",  var="ncand",       vallist=list(2)),
        spsspkg.Template("CENTER",       subc="OPTIONS", ktype="bool", var="center"),
        spsspkg.Template("INITIAL",      subc="OPTIONS", ktype="str",  var="initial"),
        spsspkg.Template("REPEATS",      subc="OPTIONS", ktype="int",  var="repeats",     vallist=list(1)),
        spsspkg.Template("DESIGNALG",    subc="OPTIONS", ktype="str",  var="designalg"),
        spsspkg.Template("CONFOUNDING",  subc="OUTPUT",  ktype="bool", var="confounding"),
        spsspkg.Template("MAINEFFECTS",  subc="OUTPUT",  ktype="bool", var="maineffects"),
        spsspkg.Template("INTERACTIONS", subc="OUTPUT",  ktype="bool", var="interactions"),
        spsspkg.Template("CUBEPLOT",     subc="OUTPUT",  ktype="bool", var="cubeplot"),
        spsspkg.Template("CONTOURPLOT",  subc="OUTPUT",  ktype="bool", var="contourplot"),
        spsspkg.Template("RESIDUALPLOTS",subc="OUTPUT",  ktype="bool", var="residualplots"),
        spsspkg.Template("PARETOPLOT",   subc="OUTPUT",  ktype="bool", var="paretoplot"),
        # Curvature Check Plot: average response at factorial (Cube) points
        # vs. Center points, connected by a line -- visual companion to the
        # additive curvature TERM. Off by default, additive only.
        spsspkg.Template("CURVATUREPLOT",subc="OUTPUT",  ktype="bool", var="curvatureplot"),
        spsspkg.Template("EXTERNALPLOTS",subc="OUTPUT",  ktype="str",  var="externalplots"),
        spsspkg.Template("EXPORTHTML",   subc="OUTPUT",  ktype="bool", var="exporthtml"),
        spsspkg.Template("HTMLPATH",     subc="OUTPUT",  ktype="literal", var="htmlpath"),
        spsspkg.Template("EFFECTSTABLE", subc="OUTPUT",  ktype="bool", var="effectstable"),
        spsspkg.Template("RESIDUALTESTS",subc="OUTPUT",  ktype="bool", var="residualtests"),
        spsspkg.Template("VIFTABLE",     subc="OUTPUT",  ktype="bool", var="viftable"),
        spsspkg.Template("OPTDETAILTABLE",subc="OUTPUT", ktype="bool", var="optdetailtable"),
        spsspkg.Template("SCREENINGSUMMARY", subc="OUTPUT", ktype="bool", var="screeningsummary"),
        # ── Power & Sample Size calculator (additive, OFF by default) ───────
        # A pure planning subdialog, independent of GENERATEDESIGN -- can be
        # used on its own, before any factors/design have even been entered,
        # to decide how many runs a design needs.
        spsspkg.Template("POWERTABLE",        subc="POWER", ktype="bool",  var="powertable"),
        spsspkg.Template("POWEREFFECTSIZES",  subc="POWER", ktype="float", var="powereffectsizes", islist=TRUE),
        spsspkg.Template("POWERRUNS",         subc="POWER", ktype="int",   var="powerruns",        islist=TRUE),
        spsspkg.Template("POWERALPHA",        subc="POWER", ktype="float", var="poweralpha"),
        spsspkg.Template("POWERTARGET",       subc="POWER", ktype="float", var="powertarget"),
        spsspkg.Template("DATASET",      subc="SAVE",    ktype="varname", var="outputdataset"),
        spsspkg.Template("VARSELECT",    subc="", ktype="bool", var="varselect"),
        spsspkg.Template("VARSELMETHOD", subc="", ktype="str",  var="varselmethod"),
        spsspkg.Template("STEPDIR",      subc="", ktype="str",  var="stepdir"),
        spsspkg.Template("SELECTIONTRACE", subc="", ktype="bool", var="selectiontrace"),
        spsspkg.Template("FITPROFILE",     subc="", ktype="bool", var="fitprofile"),
        spsspkg.Template("PARSIMONYPLOT",  subc="", ktype="bool", var="parsimonyplot"),
        spsspkg.Template("FACTORMAP",      subc="", ktype="bool", var="factormap"),
        # ── Coded-factor analysis engine ──────────────────────────────────
        spsspkg.Template("ANALYSISMODEL", subc="FITMODEL", ktype="str",     var="analysismodel",
            vallist=list("coded","legacy","item_am_a","item_am_b")),
        spsspkg.Template("TERMS",         subc="FITMODEL", ktype="literal", var="terms"),
        spsspkg.Template("ALPHA",         subc="FITMODEL", ktype="float",   var="alpha"),
        spsspkg.Template("BACKWARD",      subc="FITMODEL", ktype="bool",    var="backward"),
        spsspkg.Template("ALPHAREMOVE",   subc="FITMODEL", ktype="float",   var="alpharemove"),
        spsspkg.Template("CENTERTERM",    subc="FITMODEL", ktype="bool",    var="centerterm"),
        spsspkg.Template("CONFLEVEL",     subc="FITMODEL", ktype="float",   var="conflevel"),
        spsspkg.Template("LOWLEVELS",     subc="FITMODEL", ktype="literal", var="lowlevels"),
        spsspkg.Template("EQUATION",      subc="FITMODEL", ktype="bool",    var="equation"),
        spsspkg.Template("INTERPRET",     subc="FITMODEL", ktype="bool",    var="interpret"),
        spsspkg.Template("PREDICT",       subc="FITMODEL", ktype="literal", var="predict", islist=TRUE),
        spsspkg.Template("ALIASTABLE",    subc="FITMODEL", ktype="bool",    var="aliastable"),
        spsspkg.Template("DIAGTABLE",     subc="FITMODEL", ktype="bool",    var="diagtable"),
        spsspkg.Template("FACTORINFO",    subc="FITMODEL", ktype="bool",    var="factorinfo"),
        spsspkg.Template("EFFECTSPLOT",   subc="FITMODEL", ktype="bool",    var="effectsplot"),
        spsspkg.Template("LOWERS",        subc="OPTIMIZE", ktype="float",   var="optlowers", islist=TRUE),
        spsspkg.Template("UPPERS",        subc="OPTIMIZE", ktype="float",   var="optuppers", islist=TRUE),
        spsspkg.Template("WEIGHTS",       subc="OPTIMIZE", ktype="float",   var="optweights", islist=TRUE),
        spsspkg.Template("IMPORTANCE",    subc="OPTIMIZE", ktype="float",   var="optimportance", islist=TRUE),
        spsspkg.Template("HOLD",          subc="OPTIMIZE", ktype="literal", var="opthold"),
        spsspkg.Template("SOLUTIONS",     subc="OPTIMIZE", ktype="int",     var="optsolutions", vallist=list(1, 50)),
        spsspkg.Template("OVERLAY",       subc="OPTIMIZE", ktype="bool",    var="overlay"),
        spsspkg.Template("ROBUST",        subc="OPTIMIZE", ktype="bool",    var="robust"),
        spsspkg.Template("ROBUSTSD",      subc="OPTIMIZE", ktype="float",   var="robustsd"),
        spsspkg.Template("CONFIRMRUNS",   subc="OPTIMIZE", ktype="int",     var="confirmruns", vallist=list(1, 1000)),
        spsspkg.Template("SELECTION",     subc="FITMODEL", ktype="str",     var="selection",
            vallist=list("none","backward","forward","stepwise","item_sel_a","item_sel_b","item_sel_c","item_sel_d")),
        spsspkg.Template("ALPHAENTER",    subc="FITMODEL", ktype="float",   var="alphaenter"),
        spsspkg.Template("BOXCOX",        subc="FITMODEL", ktype="str",     var="boxcox",
            vallist=list("none","auto","log","sqrt","inverse","custom","item_bc_a","item_bc_b","item_bc_c","item_bc_d","item_bc_e","item_bc_f")),
        spsspkg.Template("LAMBDA",        subc="FITMODEL", ktype="float",   var="lambda"),
        spsspkg.Template("CATEGORICAL",   subc="FITMODEL", ktype="varname", var="categorical", islist=TRUE),
        spsspkg.Template("GROUPING",      subc="FITMODEL", ktype="bool",    var="grouping"),
        spsspkg.Template("SURFACEPLOT",   subc="FITMODEL", ktype="bool",    var="surfaceplot"),
        spsspkg.Template("CANONICAL",     subc="FITMODEL", ktype="bool",    var="canonical"),
        spsspkg.Template("MIXMODEL",      subc="FITMODEL", ktype="str",     var="mixmodel",
            vallist=list("linear","quadratic","specialcubic","fullcubic","item_mm_a","item_mm_b","item_mm_c","item_mm_d")),
        spsspkg.Template("MIXCOMPS",      subc="FITMODEL", ktype="str",     var="mixcomps",
            vallist=list("auto","pseudo","proportions","item_mc_a","item_mc_b","item_mc_c")),
        spsspkg.Template("TRACEPLOT",     subc="FITMODEL", ktype="bool",    var="traceplot"),
        spsspkg.Template("SNTYPE",        subc="FITMODEL", ktype="str",     var="sntype",
            vallist=list("larger","smaller","nominal","nominal2","none","item_sn_a","item_sn_b","item_sn_c","item_sn_d","item_sn_e")),
        spsspkg.Template("LATTICEDEGREE", subc="",         ktype="int",     var="latticedegree", vallist=list(1, 10)),
        spsspkg.Template("AUGMENTMIX",    subc="",         ktype="bool",    var="augmentmix")
    ))
    if ("HELP" %in% attr(args,"names")) helper(cmdname)
    else res <- spsspkg.processcmd(oobj, args, "optdesmc")
}

helper <- function(cmdname) {
    fn      <- gsub(" ","_",cmdname,fixed=TRUE)
    thefile <- Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) print("Help file not found")
    else browseURL(paste0("file://",thefile))
}
if (exists("spsspkg.helper")) assign("helper", spsspkg.helper)
