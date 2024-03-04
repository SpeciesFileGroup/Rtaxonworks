pkgname <- "rtaxonworks"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('rtaxonworks')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("tw_biological_associations")
### * tw_biological_associations

flush(stderr()); flush(stdout())

### Name: tw_biological_associations
### Title: Biological Associations
### Aliases: tw_biological_associations tw_ba

### ** Examples

## Not run: 
##D tw_biological_associations()
##D 
##D # How to get all simple biological associations with pagination
##D page <- 1
##D per <- 500
##D results <- res <- tw_ba(subresource='simple', page=page, per=per)
##D while (nrow(res) > 0) {
##D   page <- page + 1
##D   res <- tw_ba(subresource='simple', page=page, per=per)
##D   results <- rbind(results, res)
##D }
## End(Not run)



cleanEx()
nameEx("tw_collection_objects")
### * tw_collection_objects

flush(stderr()); flush(stdout())

### Name: tw_collection_objects
### Title: Collection objects
### Aliases: tw_collection_objects tw_co

### ** Examples

## Not run: 
##D tw_taxon_names(name="Lycorma delicatula", valid=TRUE)
## End(Not run)



cleanEx()
nameEx("tw_otus")
### * tw_otus

flush(stderr()); flush(stdout())

### Name: tw_otus
### Title: Otus
### Aliases: tw_otus tw_otu

### ** Examples

## Not run: 
##D tw_otus()
## End(Not run)



cleanEx()
nameEx("tw_projects")
### * tw_projects

flush(stderr()); flush(stdout())

### Name: tw_projects
### Title: Projects
### Aliases: tw_projects

### ** Examples

## Not run: 
##D tw_projects()
## End(Not run)



cleanEx()
nameEx("tw_taxon_name_classifications")
### * tw_taxon_name_classifications

flush(stderr()); flush(stdout())

### Name: tw_taxon_name_classifications
### Title: Taxon Name Classifications
### Aliases: tw_taxon_name_classifications tw_tnc

### ** Examples

## Not run: 
##D tw_taxon_name_classifications()
## End(Not run)



cleanEx()
nameEx("tw_taxon_name_relationships")
### * tw_taxon_name_relationships

flush(stderr()); flush(stdout())

### Name: tw_taxon_name_relationships
### Title: Taxon Name Relationships
### Aliases: tw_taxon_name_relationships tw_tnr

### ** Examples

## Not run: 
##D tw_taxon_name_relationships()
## End(Not run)



cleanEx()
nameEx("tw_taxon_names")
### * tw_taxon_names

flush(stderr()); flush(stdout())

### Name: tw_taxon_names
### Title: Taxon Names
### Aliases: tw_taxon_names tw_tn

### ** Examples

## Not run: 
##D tw_taxon_names(name="Lycorma delicatula", valid=TRUE)
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
