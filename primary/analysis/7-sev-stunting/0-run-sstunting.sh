
#!/bin/bash


R CMD BATCH 7a-sstunting-prev.R 
R CMD BATCH 7b-sstunting-PR-unadj.R 
R CMD BATCH 7c-sstunting-PR-adj.R 
R CMD BATCH 7d-sstunting-perm-test.R 
R CMD BATCH 7e-sstunting-perm-test-adj.R
