
#!/bin/bash


R CMD BATCH 3a-waz-mean.R 
R CMD BATCH 3b-waz-diff-unadj.R 
R CMD BATCH 3c-waz-diff-adj.R 
R CMD BATCH 3d-waz-perm-test.R 
R CMD BATCH 3e-waz-perm-test-adj.R
