oplan <- future::plan()
future::plan(future::multisession, workers = future::availableCores()-1)
on.exit(future::plan(oplan), add = TRUE)

# 28  sec 0.12.1.9001
# 22  sec 0.12.1.9002

tictoc::tic();px_micro(px(NHANES::NHANES));tictoc::toc();beepr::beep(2)
