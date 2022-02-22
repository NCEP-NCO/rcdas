EXE= rcdas_bctend.fd/rcdas_bctend \
rcdas_bufr2i3e.fd/rcdas_bufr2i3e \
rcdas_bufr_extrat1b.fd/rcdas_bufr_extrat1b \
rcdas_bufr_extrtv1b.fd/rcdas_bufr_extrtv1b \
rcdas_conus_precip.fd/rcdas_conus_precip \
rcdas_disag_us_mex.fd/rcdas_disag_us_mex \
rcdas_edasfcst7x1.fd/rcdas_edasfcst7x1 \
rcdas_edasfcst4x5.fd/rcdas_edasfcst4x5 \
rcdas_editbufr.fd/rcdas_editbufr \
rcdas_etapost.fd/rcdas_etapost \
rcdas_fix_rr1.cd/rcdas_fix_rr1 \
rcdas_fix_rr_conv.cd/rcdas_fix_rr_conv \
rcdas_fix_rr_mask.cd/rcdas_fix_rr_mask \
rcdas_g6tor3x.fd/rcdas_g6tor3x \
rcdas_getsst.fd/rcdas_getsst \
rcdas_gribify_cmorph.cd/rcdas_gribify_cmorph \
rcdas_gribify_conus_gauge.cd/rcdas_gribify_conus_gauge \
rcdas_gribify_us_mex2.cd/rcdas_gribify_us_mex2 \
rcdas_gribify_us_mex.cd/rcdas_gribify_us_mex \
rcdas_gridtobs.fd/rcdas_gridtobs \
rcdas_grtorr2.fd/rcdas_grtorr2 \
rcdas_iceblend.fd/rcdas_iceblend \
rcdas_mkbnd.fd/rcdas_mkbnd \
rcdas_new_date.cd/rcdas_new_date \
rcdas_pcpto32.fd/rcdas_pcpto32 \
rcdas_prdgen.fd/rcdas_prdgen \
rcdas_prepfits.fd/rcdas_prepfits \
rcdas_r3dvar.fd/rcdas_r3dvar \
rcdas_rot_vector.cd/rcdas_rot_vector \
rcdas_rrvents.fd/rcdas_rrvents \
rcdas_snowget.fd/rcdas_snowget \
rcdas_window1b_h1b.fd/rcdas_window1b_h1b \
rcdas_window1b_m1b.fd/rcdas_window1b_m1b \
rcdas_swap.cd/rcdas_swap \
rcdas_fix_afwa_snow.cd/rcdas_fix_afwa_snow \
cdas2_v1_postvents.fd/cdas2_v1_postvents \
cdas2_v1_sig2sig.fd/cdas2_v1_sig2dbl \
cdas2_v1_sig2sig.fd/cdas2_v1_sig2sngl 


all:
	cd gribw.cd ; make
#	cd transutil.fd; ./makelibgem.sh; ./makelibtransutil.sh
	cd transutil.fd; ./makelibtransutil.sh
	cd rcdas_bctend.fd ; make 
	cd rcdas_bufr2i3e.fd ; make 
	cd rcdas_bufr_extrat1b.fd ; make 
	cd rcdas_bufr_extrtv1b.fd ; make 
	cd rcdas_conus_precip.fd ; make 
	cd rcdas_disag_us_mex.fd ; make 
	cd rcdas_edasfcst7x1.fd ; make 
	cd rcdas_edasfcst4x5.fd ; make 
	cd rcdas_editbufr.fd ; make 
	cd rcdas_etapost.fd ; make 
	cd rcdas_fix_rr1.cd ; make 
	cd rcdas_fix_rr_conv.cd ; make 
	cd rcdas_fix_rr_mask.cd ; make 
	cd rcdas_g6tor3x.fd ; make 
	cd rcdas_getsst.fd ; make 
	cd rcdas_gribify_cmorph.cd ; make 
	cd rcdas_gribify_conus_gauge.cd ; make 
	cd rcdas_gribify_us_mex2.cd ; make 
	cd rcdas_gribify_us_mex.cd ; make 
	cd rcdas_gridtobs.fd ; make 
	cd rcdas_grtorr2.fd ; make 
	cd rcdas_iceblend.fd ; make 
	cd rcdas_mkbnd.fd ; make 
	cd rcdas_new_date.cd ; make 
	cd rcdas_pcpto32.fd ; make 
	cd rcdas_prdgen.fd ; make 
	cd rcdas_prepfits.fd ; make 
	cd rcdas_r3dvar.fd ; make 
	cd rcdas_rot_vector.cd ; make 
	cd rcdas_rrvents.fd ; make 
	cd rcdas_snowget.fd ; make 
	cd rcdas_window1b_h1b.fd ; make 
	cd rcdas_window1b_m1b.fd ; make 
	cd rcdas_swap.cd ; make
	cd rcdas_fix_afwa_snow.cd ; make
	cd cdas2_v1_postvents.fd ; make
	cd cdas2_v1_sig2sig.fd ; make
	cp ${EXE} ../exec/

clean:
	cd rcdas_bctend.fd ; make  clean; echo "clean"
	cd rcdas_bufr2i3e.fd ; make clean; echo "clean"
	cd rcdas_bufr_extrat1b.fd ; make clean; echo "clean"
	cd rcdas_bufr_extrtv1b.fd ; make clean; echo "clean"
	cd rcdas_conus_precip.fd ; make clean; echo "clean"
	cd rcdas_disag_us_mex.fd ; make clean; echo "clean"
	cd rcdas_edasfcst7x1.fd ; make clean; echo "clean"
	cd rcdas_edasfcst4x5.fd ; make clean; echo "clean"
	cd rcdas_editbufr.fd ; make clean; echo "clean"
	cd rcdas_etapost.fd ; make clean; echo "clean"
	cd rcdas_fix_rr1.cd ; make clean; echo "clean"
	cd rcdas_fix_rr_conv.cd ; make clean; echo "clean"
	cd rcdas_fix_rr_mask.cd ; make clean; echo "clean"
	cd rcdas_g6tor3x.fd ; make clean; echo "clean"
	cd rcdas_getsst.fd ; make clean; echo "clean"
	cd rcdas_gribify_cmorph.cd ; make clean; echo "clean"
	cd rcdas_gribify_conus_gauge.cd ; make clean; echo "clean"
	cd rcdas_gribify_us_mex2.cd ; make clean; echo "clean"
	cd rcdas_gribify_us_mex.cd ; make clean; echo "clean"
	cd rcdas_gridtobs.fd ; make clean; echo "clean"
	cd rcdas_grtorr2.fd ; make clean; echo "clean"
	cd rcdas_iceblend.fd ; make clean; echo "clean"
	cd rcdas_mkbnd.fd ; make clean; echo "clean"
	cd rcdas_new_date.cd ; make clean; echo "clean"
	cd rcdas_pcpto32.fd ; make clean; echo "clean"
	cd rcdas_prdgen.fd ; make clean; echo "clean"
	cd rcdas_prepfits.fd ; make clean; echo "clean"
	cd rcdas_r3dvar.fd ; make clean; echo "clean"
	cd rcdas_rot_vector.cd ; make clean; echo "clean"
	cd rcdas_rrvents.fd ; make clean; echo "clean"
	cd rcdas_snowget.fd ; make clean; echo "clean"
	cd rcdas_window1b_h1b.fd ; make clean; echo "clean"
	cd rcdas_window1b_m1b.fd ; make clean; echo "clean"
	cd rcdas_swap.cd ; make clean ; echo "clean"
	cd rcdas_fix_afwa_snow.cd ; make clean ; echo "clean"
	cd cdas2_v1_postvents.fd ; make clean
	cd cdas2_v1_sig2sig.fd ; make clean
	cd gribw.cd ; make clean
	cd transutil.fd; touch junk.a ; rm *.a
