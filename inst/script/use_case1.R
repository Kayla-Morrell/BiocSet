# This document is to show certain functionality of GSEABase and how the same
# functionality can be achieved (hopefully) with BiocSet.

# The first step is to acquire 
# http://software.broadinstitute.org/gsea/msigdb/download_file.jsp?filePath=/resources/msigdb/6.2/h.all.v6.2.symbols.gmt
# (this requires authentication)

# Load both of the packages...
library(GSEABase)
library(BiocSet)

# 1) ease of import of popular curated gene sets
# 1a) what are popular curated gene sets?

# I have kept the file mentioned above in my downloads, be sure to change the 
# path accordingly.
file <- "~/Downloads/h.all.v6.2.symbols.gmt"

# how to import and display with GSEABase
h1 = getGmt(file)
h1
head(names(h1))

# how to import and display with BiocSet
h2 = import(file)
h2
h2 %>% es_set() ## to show just the es_set() tibble
h2 %>% es_set() %>% pull(set) ## to show just the set column
h2 %>% es_set() %>% pull(set) %>% head() ## to show just the head of the sets

# Both packages are able to import the .gmt file and display the set names.

# 2) concept of "gene set collection" -- do we want to preserve it? clearly 
# msigdb implements the concept and that is a plus

# gene set collection with GSEABase
h1

# gene set collection with BiocSet
h2

# Though the information is not explicitly displayed in the BiocSet object, we 
# do show in the information within the tibbles. For example, the 'names:' in 
# h1 show a few examples and then (50 total). This information is shown in the
# es_set() tibble in h2 with the first 3 sets being printed and the dimensions 
# being 50x1. The same can be said for the 'unique identifiers:' in h1 being 
# shown in the es_element() tibble of h2.


# 2a) assuming we preseve collection concept, ease of measurement of set sizes

# set sizes with GSEABase
sapply(h1, function(x) length(geneIds(x)))

# set sizes with BiocSet
h2 %>% group_by(set) %>% dplyr::count()

# Both packages are able to give information about the set sizes.

# 2b) manage metadata about collections -- in GSEABase, the report on h1 above 
# is useful

# This information is not stored or displayed in the BiocSet object, but with 
# the `mutate_element()`, `mutate_set()`, or `mutate_elementset()` any info can
# be added to the tibbles.

# 3) manage metadata about sets

# metadata with GSEABase
details(h1[[1]])

# This most of this information is not stored or displayed in the BiocSet 
# object. I'm working on having the 'source' column in `es_elementset()` moved 
# to the `es_set()` tibble since this is mostly information regarding the sets.
# As I mentioned above with 2b), any of this information could be added by the 
# user if they needed it. 

# 4) support identifier conversions

# ID conversion with GSEABase
fl <- system.file("extdata", "Broad1.xml", package = "GSEABase")
bgs <- GeneSet(BroadCollection(), urls=fl)
bgs1 <- mapIdentifiers(bgs, AnnotationIdentifier("hgu95av2"))
bgs1

bgs2 <- mapIdentifiers(bgs, RefseqIdentifier("org.Hs.eg.db"))

# ID conversion with BiocSet (not ideal...)
library(hgu95av2.db)
library(org.Hs.eg.db)

xx <- as.list(hgu95av2SYMBOL)
genes <- geneIds(bgs)

bgs <- BiocSet("chr16q24" = unlist(xx[which(xx %in% genes)], use.names = FALSE))
bgs
bgs %>% es_map(hgu95av2.db, "SYMBOL", "PROBEID")

bgs %>% es_map(org.Hs.eg.db, "SYMBOL", "REFSEQ")

# One obvious issue here is that BiocSet does not support .xml files which is
# an area of improvement. I plan to take a look at what file types GSEABase
# supports and try to implement in BiocSet. I was able to do a bit of work 
# around using the 'hgu95av2.db' and 'org.Hs.eg.db' libraries to get a similar
# output to what Vince was able to produce with GSEABase. It's not a perfect
# match to Vince's output, but it gives a good idea of how BiocSet utilizes
# it's `es_map()` function to map between different IDs and databases.

# 5) relationships to ontologies -- a basic concern is relating cell types to 
# expression signatures, the latter are conceptually close to gene sets. This is
# a little table from the ctmarks app in ontoProc that starts to get at this...
# the basic idea is that some CL entries have exact-PRO relationships that we 
# can harvest to enumerate genes and thus generate gene sets. This seems like a
# good way to proceed to get institutionally endorsed assertions of cell type 
# signatures, but it goes slowly. Section 3.3 of the ontoProc vignette gets into
# ways of improvising extensions to CL to make use of the infrastructure in real
# time, anticipating proposals to CL maintainers as experimental evidence 
# solidifies.

# This would have to worked on a bit more. In our function `go_sets()` we do 
# allow the user to indicate with evidence type or ontology type they would like
# wen selecting the GO ids (the default is all evidence and ontoloty types). But
# this is as far as we got with working with ontologies.
