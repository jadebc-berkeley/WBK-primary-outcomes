
#!/bin/bash


R CMD BATCH 5a-hcz-mean.R 
R CMD BATCH 5b-hcz-diff-unadj.R 
R CMD BATCH 5c-hcz-diff-adj.R 
R CMD BATCH 5d-hcz-perm-test.R 
R CMD BATCH 5e-hcz-perm-test-adj.R
