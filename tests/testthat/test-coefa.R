test_that("coefa_tflm() and coefa_tflm2() work correctly", {
  mx1<-matrix(c(0.1,0.2,0.3,0.4,0.5,0.6),nrow = 3,byrow = TRUE)
  mx2<-matrix(c(0.6,0.5,0.4,0.3,0.2,0.1),nrow = 3,byrow = TRUE)
  list_raw<-list(mx1,mx2)
  #Loeber&Schmaling's method
  ##Method = Loeber&Schmaling's method; cutoff=0.3
  mx1_tflm_ls_0.3<-matrix(c(0,0,1,1,1,1),nrow = 3,byrow = TRUE)
  mx2_tflm_ls_0.3<-matrix(c(1,1,1,1,0,0),nrow = 3,byrow = TRUE)
  list_tflm_ls_0.3<-list(mx1_tflm_ls_0.3,mx2_tflm_ls_0.3)
  expect_identical(coefa_tflm(list_raw,methodE = "ls",cutoff = 0.3),list_tflm_ls_0.3)
  ###coefa_tflm2
  expect_identical(coefa_tflm(list_raw,methodE = "ls",cutoff = 0.3),coefa_tflm2(list_raw,methodE = "ls",cutoff = 0.3))
  ##Method = Loeber&Schmaling's method; cutoff=0.4
  mx1_tflm_ls_0.4<-matrix(c(0,0,0,1,1,1),nrow = 3,byrow = TRUE)
  mx2_tflm_ls_0.4<-matrix(c(1,1,1,0,0,0),nrow = 3,byrow = TRUE)
  list_tflm_ls_0.4<-list(mx1_tflm_ls_0.4,mx2_tflm_ls_0.4)
  expect_identical(coefa_tflm(list_raw,methodE = "ls",cutoff = 0.4),list_tflm_ls_0.4)
  ###coefa_tflm2
  expect_identical(coefa_tflm(list_raw,methodE = "ls",cutoff = 0.4),coefa_tflm2(list_raw,methodE = "ls",cutoff = 0.4))
  ##Method = Loeber&Schmaling's method; cutoff=0.5
  mx1_tflm_ls_0.5<-matrix(c(0,0,0,0,1,1),nrow = 3,byrow = TRUE)
  mx2_tflm_ls_0.5<-matrix(c(1,1,0,0,0,0),nrow = 3,byrow = TRUE)
  list_tflm_ls_0.5<-list(mx1_tflm_ls_0.5,mx2_tflm_ls_0.5)
  expect_identical(coefa_tflm(list_raw,methodE = "ls",cutoff = 0.5),list_tflm_ls_0.5)
  ###coefa_tflm2
  expect_identical(coefa_tflm(list_raw,methodE = "ls",cutoff = 0.5),coefa_tflm2(list_raw,methodE = "ls",cutoff = 0.5))
  #Method = Shafer's method
  ##Method = Shafer's method; cutoff=0.3
  mx1_tflm_s_0.3<-matrix(c(0,0,0,1,0,1),nrow = 3,byrow = TRUE)
  mx2_tflm_s_0.3<-matrix(c(1,0,1,0,0,0),nrow = 3,byrow = TRUE)
  list_tflm_s_0.3<-list(mx1_tflm_s_0.3,mx2_tflm_s_0.3)
  expect_identical(coefa_tflm(list_raw,methodE = "s",cutoff = 0.3),list_tflm_s_0.3)
  ###coefa_tflm2
  expect_identical(coefa_tflm(list_raw,methodE = "s",cutoff = 0.3),coefa_tflm2(list_raw,methodE = "s",cutoff = 0.3))
  ##Method = Shafer's method; cutoff=0.4
  mx1_tflm_s_0.4<-matrix(c(0,0,0,1,0,1),nrow = 3,byrow = TRUE)
  mx2_tflm_s_0.4<-matrix(c(1,0,1,0,0,0),nrow = 3,byrow = TRUE)
  list_tflm_s_0.4<-list(mx1_tflm_s_0.4,mx2_tflm_s_0.4)
  expect_identical(coefa_tflm(list_raw,methodE = "s",cutoff = 0.4),list_tflm_s_0.4)
  ###coefa_tflm2
  expect_identical(coefa_tflm(list_raw,methodE = "s",cutoff = 0.4),coefa_tflm2(list_raw,methodE = "s",cutoff = 0.4))
  ##Method = Shafer's method; cutoff=0.5
  mx1_tflm_s_0.5<-matrix(c(0,0,0,0,0,1),nrow = 3,byrow = TRUE)
  mx2_tflm_s_0.5<-matrix(c(1,0,0,0,0,0),nrow = 3,byrow = TRUE)
  list_tflm_s_0.5<-list(mx1_tflm_s_0.5,mx2_tflm_s_0.5)
  expect_identical(coefa_tflm(list_raw,methodE = "s",cutoff = 0.5),list_tflm_s_0.5)
  ###coefa_tflm2
  expect_identical(coefa_tflm(list_raw,methodE = "s",cutoff = 0.5),coefa_tflm2(list_raw,methodE = "s",cutoff = 0.5))
})

test_that("coefa_gcm() work correctly",{
  mx1_gcm<-matrix(c(0,1,0,1,0,1),nrow = 3,byrow = TRUE)
  mx2_gcm<-matrix(c(0,1,1,0,1,0),nrow = 3,byrow = TRUE)
  list_gcm<-list(mx1_gcm,mx2_gcm)
  mx1_gcm_multiply<-mx1_gcm%*%t(mx1_gcm)
  mx2_gcm_multiply<-mx2_gcm%*%t(mx2_gcm)
  mx_gcm<-coefa_gcm(list_gcm)
  list_multiply<-list(mx1_gcm_multiply,mx2_gcm_multiply)
  expect_identical(mx_gcm,list_multiply)
})

test_that("coefa_acm() work correctly",{
  mx1_gcm<-matrix(c(0,1,0,1,0,1),nrow = 3,byrow = TRUE)
  mx2_gcm<-matrix(c(0,1,1,0,1,0),nrow = 3,byrow = TRUE)
  list_mx<-list(mx1_gcm,mx2_gcm)
  list_gcm<-coefa_gcm(list_mx)
  list_gcm2<-(list_gcm[[1]]*100+list_gcm[[2]]*200)/300
  ##coefa_acm,samplesize<-c(100,200)
  samplesize<-c(100,200)
  list_acm<-coefa_acm(list_gcm,sz=samplesize,samplesized = TRUE)
  expect_identical(list_acm,list_gcm2)
})
