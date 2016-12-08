
#!/bin/bash


R CMD BATCH 4a-whz-mean.R 
R CMD BATCH 4b-whz-diff-unadj.R 
R CMD BATCH 4c-whz-diff-adj.R 
R CMD BATCH 4d-whz-perm-test.R 
R CMD BATCH 4e-whz-perm-test-adj.R
