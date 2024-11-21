oplan <- future::plan()
future::plan(future::multisession, workers = future::availableCores()-1)
on.exit(future::plan(oplan), add = TRUE)

# Version     SDU laptop   ThinkPad laptop
# 0.12.1.9001    28  sec            41 sec
# 0.12.1.9002    22  sec            35 sec
devtools::load_all(".");tictoc::tic();pxmake::px_micro(pxmake::px(NHANES::NHANES));tictoc::toc();beepr::beep(2)


devtools::load_all(".");pxmake::px_micro(pxmake::px(NHANES::NHANES))
