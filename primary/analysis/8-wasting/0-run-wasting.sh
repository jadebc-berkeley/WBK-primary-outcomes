
#!/bin/bash


R CMD BATCH 8a-wasting-prev.R 
R CMD BATCH 8b-wasting-PR-unadj.R 
R CMD BATCH 8c-wasting-PR-adj.R 
R CMD BATCH 8d-wasting-perm-test.R 
R CMD BATCH 8e-wasting-perm-test-adj.R
