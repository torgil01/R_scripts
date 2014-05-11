
hunt.methods <- c("FS_orig","FS_opt","newRBMT1T2029","newRBMT1034","ARBMT1T2","ARBMT1")

blandaltman.plot(hunt.manual$Manual,hunt.manual$FS_orig,title="FreeSurfer")
blandaltman.plot(hunt.manual$Manual,hunt.manual$FS_opt,title="FreeSurfer opt")
blandaltman.plot(hunt.manual$Manual,hunt.manual$newRBMT1034,title="RBM")
blandaltman.plot(hunt.manual$Manual,hunt.manual$newRBMT1T2029,title="RBM multi")
blandaltman.plot(hunt.manual$Manual,hunt.manual$ARBMT1,title="ARBM")
blandaltman.plot(hunt.manual$Manual,hunt.manual$ARBMT1T2,title="ARBM multi")
