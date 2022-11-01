@echo off
cd glide2x
make -f Makefile.win32 FX_GLIDE_HW=cvg clean
make -f Makefile.win32 FX_GLIDE_HW=sst1 clean
make -f Makefile.win32 FX_GLIDE_HW=sst96 clean
cd ..
cd glide3x
make -f Makefile.win32 FX_GLIDE_HW=cvg clean
make -f Makefile.win32 FX_GLIDE_HW=sst1 clean
make -f Makefile.win32 FX_GLIDE_HW=sst96 clean
cd ..
