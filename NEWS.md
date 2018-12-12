# HexSimR 0.9.0.900
## New functionality
collate.census gains keep.zeros

TO-DO ##################################################################

## Bugs
  *  fixed issues when using start or end arguments in collate.census()
  TO-DO
  
# HexSimR 0.4.4.900
## New functionality
LHS.scenario
compress.logs
rename.replicates
copy.results is now deprecated

## Bugs
  *  fixed bug that prevented start and end arguments in Pext() to be used when numeric
  *  Pext() now throws an error when paramenters are improper
  
# HexSimR 0.3.4.900
## New functionality
None

## Bugs
  *  Fixed issue with ggplot due to new class as.data.frame.list (now removed). 
This caused problems only with R >= 3.5.0
  *  Fixed labelling bug when there were > 10 census
  *  Removed choose.dir() to ensure compatibility with platforms other than Windows.

# HexSimR 0.3.4.0
## New functionality
  *  collate.census gains two new arguments ('start' and 'end')
  
## Bugs
  *  Minor improvements to documentation
  
# HexSimR 0.3.3.0
## New functionality
  *  scenarios.batch.modifier
  *  workspace.path.modifier
  
## Bugs
  *  Minor improvements to documentation and fixed links to external resources
  
# HexSimR 0.2.3.0 
Evaluation release