
#!/bin/bash


R CMD BATCH 10b-distance-matrix.R &
R CMD BATCH 10c-distance-matrix-subset.R & 
R CMD BATCH 10d-dist-spillover-test.R & 
R CMD BATCH 10e-dist-spillover-test-adj.R & 