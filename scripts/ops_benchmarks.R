# Benchmark R

# 10000 rows x 25000 columns matrix (floats)
mat <- matrix(data = data.table::fread("numba.csv")$NUMBA, nrow = 10000L)

# Operations
microbenchmark::microbenchmark(
  single_add = {mat+mat},
  multi_add = {mat + mat + mat + mat + mat},
  single_multiply = {mat * mat},
  multi_multiply = {mat * mat * mat * mat * mat},
  single_divide = {mat / mat},
  multi_divide = {mat / mat / mat / mat / mat},
  power = {mat ^ 2},
  times = 5
)

# It yields pretty similar results than native C, so as long as
# we stick to simple vector operations, I do not think we need
# to go to C. We can parallelize via parallel.
