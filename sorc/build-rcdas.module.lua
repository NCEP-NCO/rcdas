
load("PrgEnv-intel/"..os.getenv("PrgEnv_intel_ver"))
load("intel/"..os.getenv("intel_ver"))
load("ip/"..os.getenv("ip_ver"))
load("sp/"..os.getenv("sp_ver"))
load("bacio/"..os.getenv("bacio_ver"))
load("bufr/"..os.getenv("bufr_ver"))
load("w3emc/"..os.getenv("w3emc_ver"))
load("w3nco/"..os.getenv("w3nco_ver"))
load("craype/"..os.getenv("craype_ver"))
load("cray-mpich/"..os.getenv("cray_mpich_ver"))

setenv("CC","cc")
setenv("FC","ftn")
