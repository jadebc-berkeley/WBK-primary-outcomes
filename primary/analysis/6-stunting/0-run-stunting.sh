
#!/bin/bash


R CMD BATCH 6a-stunting-prev.R 
R CMD BATCH 6b-stunting-PR-unadj.R 
R CMD BATCH 6c-stunting-PR-adj.R 
R CMD BATCH 6d-stunting-perm-test.R 
R CMD BATCH 6e-stunting-perm-test-adj.R
