!
      integer(4),pointer::is_loc_table(:),js_loc_table(:)
      integer(4),pointer::ie_loc_table(:),je_loc_table(:)
      integer(4),pointer::ichunktab(:)
      COMMON/MPPCOM/MYPE,NPES,MY_IS_GLB,MY_IE_GLB,MY_JS_GLB,MY_JE_GLB
      common/mppcom/MY_IS_LOC,MY_IE_LOC,MY_JS_LOC,MY_JE_LOC
      common/mppcom/MYIS,MYIS1,MYIS2,MYIE,MYIE1,MYIE2
      common/mppcom/MYIS_P1,MYIS_P2,MYIS_P3,MYIS_P4
      common/mppcom/MYIS1_P1,MYIS1_P2
      common/mppcom/MYIE_P1,MYIE_P2,MYIE_P3,MYIE_P4
      common/mppcom/MYIE1_P1,MYIE1_P2,MYIE1_P3,MYIE2_P1
      common/mppcom/MYJS,MYJS1,MYJS2,MYJS3,MYJS4,MYJS5
      common/mppcom/MYJS_P1,MYJS_P2,MYJS_P3,MYJS_P4,MYJS_P5
      common/mppcom/MYJS1_P1,MYJS1_P2,MYJS1_P3,MYJS1_P4
      common/mppcom/MYJS2_P1,MYJS2_P2,MYJS2_P4
      common/mppcom/MYJS3_P4,MYJS4_P4,MYJS5_P2
      common/mppcom/MYJE,MYJE1,MYJE2,MYJE3,MYJE4,MYJE5
      common/mppcom/MYJE_P1,MYJE_P2,MYJE_P3,MYJE_P4,MYJE_P5
      common/mppcom/MYJE1_P1,MYJE1_P2,MYJE1_P3,MYJE1_P4
      common/mppcom/MYJE2_P1,MYJE2_P2,MYJE2_P4
      common/mppcom/MYJE3_P4,MYJE4_P4,MYJE5_P2
      common/mppcom/MY_N,MY_E,MY_S,MY_W
      common/mppcom/MY_NE,MY_SE,MY_SW,MY_NW,MY_NEB(8)
      common/mppcom/ILCOL,IRCOL,IBROW,ITROW
      common/mppcom/ILPAD1,ILPAD2,ILPAD3,ILPAD4
      common/mppcom/IRPAD1,IRPAD2,IRPAD3,IRPAD4
      common/mppcom/JBPAD1,JBPAD2,JBPAD3,JBPAD4,JBPAD5
      common/mppcom/JTPAD1,JTPAD2,JTPAD3,JTPAD4,JTPAD5
      common/mppcom/IS_LOC_TABLE,JS_LOC_TABLE
      common/mppcom/IE_LOC_TABLE,JE_LOC_TABLE,ICHUNKTAB
!
      integer(4),pointer::is_glb_table(:),ie_glb_table(:)
      integer(4),pointer::js_glb_table(:),je_glb_table(:)
      COMMON/GLB_TABLE/IS_GLB_TABLE,IE_GLB_TABLE
      common/glb_table/JS_GLB_TABLE,JE_GLB_TABLE
!
      real(4),pointer::temp1(:,:),temp2(:,:)
      real(4),pointer::temp3(:,:),temp4(:,:)
      real(4),pointer::temp5(:,:),temp6(:,:)
      real(4),pointer::temp7(:,:),temp8(:,:)
      real(4),pointer::temp9(:,:),temp10(:,:),temp11(:,:)
      real(4),pointer::temp12(:,:),temp13(:,:),temp14(:,:)
      real(4),pointer::temp15(:,:),temp16(:,:)
      integer(4),pointer::itemp(:,:),itemp2(:,:)
      COMMON/TEMPCOM/TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,TEMP8
      COMMON/TEMPCOM/TEMP9,TEMP10,TEMP11,TEMP12,TEMP13,TEMP14,TEMP15
      COMMON/TEMPCOM/TEMP16
      common/tempcom/ITEMP,ITEMP2
!
      real(4),pointer::temp2x(:,:),ttvg(:,:),htmg(:,:,:)
      COMMON/TOPO/TEMP2X,TTVG,HTMG
!
      integer(4),pointer::g2li(:),g2lj(:),l2gi(:),l2gj(:)
      COMMON/MAPPINGS/G2LI,L2GI,G2LJ,L2GJ        
