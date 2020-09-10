Function Basics
================
2020-05-03

*Purpose*: Functions are our primary tool in carying out data analysis
with the `tidyverse`. It is unreasonable to expect yourself to memorize
every function and all its details. To that end, we’ll learn some basic
*function literacy* in R; how to inspect a function, look up its
documentation, and find examples on a function’s use.

*Reading*: [Programming
Basics](https://rstudio.cloud/learn/primers/1.2). *Topics*: `functions`,
`arguments` *Reading Time*: \~ 10 minutes

**q1** How do you find help on a function? Get help on the built-in
`rnorm` function.

``` r
help(rnorm)
```

**q2** How do you show the source code for a function?

``` r
rnorm
```

    ## function (n, mean = 0, sd = 1) 
    ## .Call(C_rnorm, n, mean, sd)
    ## <bytecode: 0x5558c4855d40>
    ## <environment: namespace:stats>

**q3** Using either the documentation or the source, determine the
arguments for `rnorm`. x, q  
vector of quantiles.

p  
vector of probabilities.

n  
number of observations. If length(n) \> 1, the length is taken to be the
number required.

mean  
vector of means.

sd  
vector of standard deviations.

log, log.p  
logical; if TRUE, probabilities p are given as log(p).

lower.tail  
logical; if TRUE (default), probabilities are P\[X ≤ x\] otherwise, P\[X
\> x\].

**q4** Scroll to the bottom of the help for the `library()` function.
How do you list all available packages?

``` r
library
```

    ## function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE, 
    ##     logical.return = FALSE, warn.conflicts = TRUE, quietly = FALSE, 
    ##     verbose = getOption("verbose")) 
    ## {
    ##     testRversion <- function(pkgInfo, pkgname, pkgpath) {
    ##         if (is.null(built <- pkgInfo$Built)) 
    ##             stop(gettextf("package %s has not been installed properly\n", 
    ##                 sQuote(pkgname)), call. = FALSE, domain = NA)
    ##         R_version_built_under <- as.numeric_version(built$R)
    ##         if (R_version_built_under < "3.0.0") 
    ##             stop(gettextf("package %s was built before R 3.0.0: please re-install it", 
    ##                 sQuote(pkgname)), call. = FALSE, domain = NA)
    ##         current <- getRversion()
    ##         if (length(Rdeps <- pkgInfo$Rdepends2)) {
    ##             for (dep in Rdeps) if (length(dep) > 1L) {
    ##                 target <- dep$version
    ##                 res <- if (is.character(target)) {
    ##                   do.call(dep$op, list(as.numeric(R.version[["svn rev"]]), 
    ##                     as.numeric(sub("^r", "", dep$version))))
    ##                 }
    ##                 else {
    ##                   do.call(dep$op, list(current, as.numeric_version(target)))
    ##                 }
    ##                 if (!res) 
    ##                   stop(gettextf("This is R %s, package %s needs %s %s", 
    ##                     current, sQuote(pkgname), dep$op, target), 
    ##                     call. = FALSE, domain = NA)
    ##             }
    ##         }
    ##         if (R_version_built_under > current) 
    ##             warning(gettextf("package %s was built under R version %s", 
    ##                 sQuote(pkgname), as.character(built$R)), call. = FALSE, 
    ##                 domain = NA)
    ##         platform <- built$Platform
    ##         r_arch <- .Platform$r_arch
    ##         if (.Platform$OS.type == "unix") {
    ##             if (!nzchar(r_arch) && grepl("\\w", platform) && 
    ##                 !testPlatformEquivalence(platform, R.version$platform)) 
    ##                 stop(gettextf("package %s was built for %s", 
    ##                   sQuote(pkgname), platform), call. = FALSE, 
    ##                   domain = NA)
    ##         }
    ##         else {
    ##             if (nzchar(platform) && !grepl("mingw", platform)) 
    ##                 stop(gettextf("package %s was built for %s", 
    ##                   sQuote(pkgname), platform), call. = FALSE, 
    ##                   domain = NA)
    ##         }
    ##         if (nzchar(r_arch) && file.exists(file.path(pkgpath, 
    ##             "libs")) && !file.exists(file.path(pkgpath, "libs", 
    ##             r_arch))) 
    ##             stop(gettextf("package %s is not installed for 'arch = %s'", 
    ##                 sQuote(pkgname), r_arch), call. = FALSE, domain = NA)
    ##     }
    ##     testFeatures <- function(features, pkgInfo, pkgname, pkgpath) {
    ##         needsComp <- as.character(pkgInfo$DESCRIPTION["NeedsCompilation"])
    ##         if (identical(needsComp, "yes")) {
    ##             internalsID <- features$internalsID
    ##             if (is.null(internalsID)) 
    ##                 internalsID <- "0310d4b8-ccb1-4bb8-ba94-d36a55f60262"
    ##             if (internalsID != .Internal(internalsID())) 
    ##                 stop(gettextf("package %s was installed by an R version with different internals; it needs to be reinstalled for use with this R version", 
    ##                   sQuote(pkgname)), call. = FALSE, domain = NA)
    ##         }
    ##     }
    ##     checkNoGenerics <- function(env, pkg) {
    ##         nenv <- env
    ##         ns <- .getNamespace(as.name(pkg))
    ##         if (!is.null(ns)) 
    ##             nenv <- asNamespace(ns)
    ##         if (exists(".noGenerics", envir = nenv, inherits = FALSE)) 
    ##             TRUE
    ##         else {
    ##             !any(startsWith(names(env), ".__T"))
    ##         }
    ##     }
    ##     checkConflicts <- function(package, pkgname, pkgpath, nogenerics, 
    ##         env) {
    ##         dont.mind <- c("last.dump", "last.warning", ".Last.value", 
    ##             ".Random.seed", ".Last.lib", ".onDetach", ".packageName", 
    ##             ".noGenerics", ".required", ".no_S3_generics", ".Depends", 
    ##             ".requireCachedGenerics")
    ##         sp <- search()
    ##         lib.pos <- which(sp == pkgname)
    ##         ob <- names(as.environment(lib.pos))
    ##         if (!nogenerics) {
    ##             these <- ob[startsWith(ob, ".__T__")]
    ##             gen <- gsub(".__T__(.*):([^:]+)", "\\1", these)
    ##             from <- gsub(".__T__(.*):([^:]+)", "\\2", these)
    ##             gen <- gen[from != package]
    ##             ob <- ob[!(ob %in% gen)]
    ##         }
    ##         fst <- TRUE
    ##         ipos <- seq_along(sp)[-c(lib.pos, match(c("Autoloads", 
    ##             "CheckExEnv"), sp, 0L))]
    ##         for (i in ipos) {
    ##             obj.same <- match(names(as.environment(i)), ob, nomatch = 0L)
    ##             if (any(obj.same > 0)) {
    ##                 same <- ob[obj.same]
    ##                 same <- same[!(same %in% dont.mind)]
    ##                 Classobjs <- which(startsWith(same, ".__"))
    ##                 if (length(Classobjs)) 
    ##                   same <- same[-Classobjs]
    ##                 same.isFn <- function(where) vapply(same, exists, 
    ##                   NA, where = where, mode = "function", inherits = FALSE)
    ##                 same <- same[same.isFn(i) == same.isFn(lib.pos)]
    ##                 not.Ident <- function(ch, TRAFO = identity, ...) vapply(ch, 
    ##                   function(.) !identical(TRAFO(get(., i)), TRAFO(get(., 
    ##                     lib.pos)), ...), NA)
    ##                 if (length(same)) 
    ##                   same <- same[not.Ident(same)]
    ##                 if (length(same) && identical(sp[i], "package:base")) 
    ##                   same <- same[not.Ident(same, ignore.environment = TRUE)]
    ##                 if (length(same)) {
    ##                   if (fst) {
    ##                     fst <- FALSE
    ##                     packageStartupMessage(gettextf("\nAttaching package: %s\n", 
    ##                       sQuote(package)), domain = NA)
    ##                   }
    ##                   msg <- .maskedMsg(sort(same), pkg = sQuote(sp[i]), 
    ##                     by = i < lib.pos)
    ##                   packageStartupMessage(msg, domain = NA)
    ##                 }
    ##             }
    ##         }
    ##     }
    ##     if (verbose && quietly) 
    ##         message("'verbose' and 'quietly' are both true; being verbose then ..")
    ##     if (!missing(package)) {
    ##         if (is.null(lib.loc)) 
    ##             lib.loc <- .libPaths()
    ##         lib.loc <- lib.loc[dir.exists(lib.loc)]
    ##         if (!character.only) 
    ##             package <- as.character(substitute(package))
    ##         if (length(package) != 1L) 
    ##             stop("'package' must be of length 1")
    ##         if (is.na(package) || (package == "")) 
    ##             stop("invalid package name")
    ##         pkgname <- paste0("package:", package)
    ##         newpackage <- is.na(match(pkgname, search()))
    ##         if (newpackage) {
    ##             pkgpath <- find.package(package, lib.loc, quiet = TRUE, 
    ##                 verbose = verbose)
    ##             if (length(pkgpath) == 0L) {
    ##                 txt <- if (length(lib.loc)) 
    ##                   gettextf("there is no package called %s", sQuote(package))
    ##                 else gettext("no library trees found in 'lib.loc'")
    ##                 if (logical.return) {
    ##                   warning(txt, domain = NA)
    ##                   return(FALSE)
    ##                 }
    ##                 else stop(txt, domain = NA)
    ##             }
    ##             which.lib.loc <- normalizePath(dirname(pkgpath), 
    ##                 "/", TRUE)
    ##             pfile <- system.file("Meta", "package.rds", package = package, 
    ##                 lib.loc = which.lib.loc)
    ##             if (!nzchar(pfile)) 
    ##                 stop(gettextf("%s is not a valid installed package", 
    ##                   sQuote(package)), domain = NA)
    ##             pkgInfo <- readRDS(pfile)
    ##             testRversion(pkgInfo, package, pkgpath)
    ##             ffile <- system.file("Meta", "features.rds", package = package, 
    ##                 lib.loc = which.lib.loc)
    ##             features <- if (file.exists(ffile)) 
    ##                 readRDS(ffile)
    ##             else NULL
    ##             testFeatures(features, pkgInfo, package, pkgpath)
    ##             if (is.character(pos)) {
    ##                 npos <- match(pos, search())
    ##                 if (is.na(npos)) {
    ##                   warning(gettextf("%s not found on search path, using pos = 2", 
    ##                     sQuote(pos)), domain = NA)
    ##                   pos <- 2
    ##                 }
    ##                 else pos <- npos
    ##             }
    ##             .getRequiredPackages2(pkgInfo, quietly = quietly)
    ##             deps <- unique(names(pkgInfo$Depends))
    ##             if (packageHasNamespace(package, which.lib.loc)) {
    ##                 if (isNamespaceLoaded(package)) {
    ##                   newversion <- as.numeric_version(pkgInfo$DESCRIPTION["Version"])
    ##                   oldversion <- as.numeric_version(getNamespaceVersion(package))
    ##                   if (newversion != oldversion) {
    ##                     res <- tryCatch(unloadNamespace(package), 
    ##                       error = function(e) {
    ##                         P <- if (!is.null(cc <- conditionCall(e))) 
    ##                           paste("Error in", deparse(cc)[1L], 
    ##                             ": ")
    ##                         else "Error : "
    ##                         stop(gettextf("Package %s version %s cannot be unloaded:\n %s", 
    ##                           sQuote(package), oldversion, paste0(P, 
    ##                             conditionMessage(e), "\n")), domain = NA)
    ##                       })
    ##                   }
    ##                 }
    ##                 tt <- tryCatch({
    ##                   attr(package, "LibPath") <- which.lib.loc
    ##                   ns <- loadNamespace(package, lib.loc)
    ##                   env <- attachNamespace(ns, pos = pos, deps)
    ##                 }, error = function(e) {
    ##                   P <- if (!is.null(cc <- conditionCall(e))) 
    ##                     paste(" in", deparse(cc)[1L])
    ##                   else ""
    ##                   msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
    ##                     sQuote(package), P, conditionMessage(e))
    ##                   if (logical.return) 
    ##                     message(paste("Error:", msg), domain = NA)
    ##                   else stop(msg, call. = FALSE, domain = NA)
    ##                 })
    ##                 if (logical.return && is.null(tt)) 
    ##                   return(FALSE)
    ##                 attr(package, "LibPath") <- NULL
    ##                 {
    ##                   on.exit(detach(pos = pos))
    ##                   nogenerics <- !.isMethodsDispatchOn() || checkNoGenerics(env, 
    ##                     package)
    ##                   if (warn.conflicts && !exists(".conflicts.OK", 
    ##                     envir = env, inherits = FALSE)) 
    ##                     checkConflicts(package, pkgname, pkgpath, 
    ##                       nogenerics, ns)
    ##                   on.exit()
    ##                   if (logical.return) 
    ##                     return(TRUE)
    ##                   else return(invisible(.packages()))
    ##                 }
    ##             }
    ##             else stop(gettextf("package %s does not have a namespace and should be re-installed", 
    ##                 sQuote(package)), domain = NA)
    ##         }
    ##         if (verbose && !newpackage) 
    ##             warning(gettextf("package %s already present in search()", 
    ##                 sQuote(package)), domain = NA)
    ##     }
    ##     else if (!missing(help)) {
    ##         if (!character.only) 
    ##             help <- as.character(substitute(help))
    ##         pkgName <- help[1L]
    ##         pkgPath <- find.package(pkgName, lib.loc, verbose = verbose)
    ##         docFiles <- c(file.path(pkgPath, "Meta", "package.rds"), 
    ##             file.path(pkgPath, "INDEX"))
    ##         if (file.exists(vignetteIndexRDS <- file.path(pkgPath, 
    ##             "Meta", "vignette.rds"))) 
    ##             docFiles <- c(docFiles, vignetteIndexRDS)
    ##         pkgInfo <- vector("list", 3L)
    ##         readDocFile <- function(f) {
    ##             if (basename(f) %in% "package.rds") {
    ##                 txt <- readRDS(f)$DESCRIPTION
    ##                 if ("Encoding" %in% names(txt)) {
    ##                   to <- if (Sys.getlocale("LC_CTYPE") == "C") 
    ##                     "ASCII//TRANSLIT"
    ##                   else ""
    ##                   tmp <- try(iconv(txt, from = txt["Encoding"], 
    ##                     to = to))
    ##                   if (!inherits(tmp, "try-error")) 
    ##                     txt <- tmp
    ##                   else warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible", 
    ##                     call. = FALSE)
    ##                 }
    ##                 nm <- paste0(names(txt), ":")
    ##                 formatDL(nm, txt, indent = max(nchar(nm, "w")) + 
    ##                   3L)
    ##             }
    ##             else if (basename(f) %in% "vignette.rds") {
    ##                 txt <- readRDS(f)
    ##                 if (is.data.frame(txt) && nrow(txt)) 
    ##                   cbind(basename(gsub("\\.[[:alpha:]]+$", "", 
    ##                     txt$File)), paste(txt$Title, paste0(rep.int("(source", 
    ##                     NROW(txt)), ifelse(nzchar(txt$PDF), ", pdf", 
    ##                     ""), ")")))
    ##                 else NULL
    ##             }
    ##             else readLines(f)
    ##         }
    ##         for (i in which(file.exists(docFiles))) pkgInfo[[i]] <- readDocFile(docFiles[i])
    ##         y <- list(name = pkgName, path = pkgPath, info = pkgInfo)
    ##         class(y) <- "packageInfo"
    ##         return(y)
    ##     }
    ##     else {
    ##         if (is.null(lib.loc)) 
    ##             lib.loc <- .libPaths()
    ##         db <- matrix(character(), nrow = 0L, ncol = 3L)
    ##         nopkgs <- character()
    ##         for (lib in lib.loc) {
    ##             a <- .packages(all.available = TRUE, lib.loc = lib)
    ##             for (i in sort(a)) {
    ##                 file <- system.file("Meta", "package.rds", package = i, 
    ##                   lib.loc = lib)
    ##                 title <- if (nzchar(file)) {
    ##                   txt <- readRDS(file)
    ##                   if (is.list(txt)) 
    ##                     txt <- txt$DESCRIPTION
    ##                   if ("Encoding" %in% names(txt)) {
    ##                     to <- if (Sys.getlocale("LC_CTYPE") == "C") 
    ##                       "ASCII//TRANSLIT"
    ##                     else ""
    ##                     tmp <- try(iconv(txt, txt["Encoding"], to, 
    ##                       "?"))
    ##                     if (!inherits(tmp, "try-error")) 
    ##                       txt <- tmp
    ##                     else warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible", 
    ##                       call. = FALSE)
    ##                   }
    ##                   txt["Title"]
    ##                 }
    ##                 else NA
    ##                 if (is.na(title)) 
    ##                   title <- " ** No title available ** "
    ##                 db <- rbind(db, cbind(i, lib, title))
    ##             }
    ##             if (length(a) == 0L) 
    ##                 nopkgs <- c(nopkgs, lib)
    ##         }
    ##         dimnames(db) <- list(NULL, c("Package", "LibPath", "Title"))
    ##         if (length(nopkgs) && !missing(lib.loc)) {
    ##             pkglist <- paste(sQuote(nopkgs), collapse = ", ")
    ##             msg <- sprintf(ngettext(length(nopkgs), "library %s contains no packages", 
    ##                 "libraries %s contain no packages"), pkglist)
    ##             warning(msg, domain = NA)
    ##         }
    ##         y <- list(header = NULL, results = db, footer = NULL)
    ##         class(y) <- "libraryIQR"
    ##         return(y)
    ##     }
    ##     if (logical.return) 
    ##         TRUE
    ##     else invisible(.packages())
    ## }
    ## <bytecode: 0x5558c25580d8>
    ## <environment: namespace:base>

The **examples** in the help documentation are often *extremely* helpful
for learning how to use a function (or reminding yourself how its
used)\! Get used to checking the examples, as they’re a great resource.

<!-- include-exit-ticket -->

# Exit Ticket

<!-- -------------------------------------------------- -->

Once you have completed this exercise, make sure to fill out the **exit
ticket survey**, [linked
here](https://docs.google.com/forms/d/e/1FAIpQLSeuq2LFIwWcm05e8-JU84A3irdEL7JkXhMq5Xtoalib36LFHw/viewform?usp=pp_url&entry.693978880=e-setup02-functions-assignment.Rmd).
