# rMAFdb

Convert Multiple Annotation Format (MAF) Files to a database and vice-versa

This package creates an easy to use interface to operate with huge MAF files.
MAF filed (possibly following GDC standard) can be parsed and loaded into a database
directly, data manipulation can be then done with dbplyr so that the related
code can be easly adapted from regular "on RAM" operations. 
The use of databases allows the user to create indexes to esily speed up queries
even for smaller data sets.

## Main features:

* Easy to use: load data directly from a file on the disk, easily explore database structure, compatible with dbplyr
* Transparent: queries can always produce a regular MAF file to be used with other tools
* Efficient: MAF file elaboration code is written C++, allowing fast database creation even on older machines.
  The use of indexes can noticeably speed up further analysis.
* Flexible: When a MAF file respects GDC standards or uses GDC standard columns, the data is interpreted and 
  reorganized automatically. User can provide basic interpretation (numerical, character) for columns of its
  MAF files and the database will be prepared accordingly. There are no mandatory columns.
  
### Installation

```{r}
# install
devtools::install_github("redsnic/rMAFdb", build_vignette=T)
# display vignettes
browseVignettes("rMAFdb")
```
