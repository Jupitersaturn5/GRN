# GRN
Good Receipt Note Integration(Outbound)
  METHOD if_rest_resource~get.
**************************************************************************
*                                                                        *
*   Class for Extraction of Goods Receipt details From SAP *
*   Class name : ZCL_GRN_PROVIDER                             *
*   DATE CREATED         : 11.05.2023                                    *
**************************************************************************

*    *-----------------------------------Structure for MKPF tables values----------------------------
    TYPES : BEGIN OF ty_mkpf,
              mblnr TYPE mblnr,
              bldat TYPE bldat,
              budat TYPE budat,
              xblnr TYPE xblnr1,
            END OF ty_mkpf,
*    *-----------------------------------Structure for MSEG tables values----------------------------
            BEGIN OF ty_mseg,
              mblnr      TYPE mblnr,
              zeile      TYPE mblpo,
              bwart      TYPE bwart,
              matnr      TYPE matnr,
              werks      TYPE werks_d,
              lifnr      TYPE elifn,
              menge      TYPE menge_d,
              meins      TYPE meins,
              ebeln      TYPE bstnr,
              ebelp      TYPE ebelp,
              cpudt_mkpf TYPE cpudt,
              vbeln_im   TYPE mseg-vbeln_im,
              vbelp_im   TYPE mseg-vbelp_im,
            END OF ty_mseg,



            BEGIN OF ty_mseg_n,
              mblnr TYPE mblnr,
              zeile TYPE mblpo,
              bwart TYPE bwart,
              matnr TYPE matnr,
              werks TYPE werks_d,
              lifnr TYPE elifn,
              menge TYPE menge_d,
              meins TYPE meins,
              ebeln TYPE bstnr,
              ebelp TYPE ebelp,
              lfbnr TYPE lfbnr,
            END OF ty_mseg_n,

*    *-----------------------------------Structure for LFA1 tables values----------------------------
            BEGIN OF ty_lfa1,
              lifnr TYPE lifnr,
              name1 TYPE name1_gp,
            END OF ty_lfa1,

*    *-----------------------------------Structure for EKKO tables values----------------------------

            BEGIN OF ty_ekko,
              ebeln TYPE ebeln,
              bstyp TYPE ebstyp,
              aedat TYPE erdat,
            END OF ty_ekko,

*    *-----------------------------------Structure for EKPO tables values----------------------------
            BEGIN OF ty_ekpo,
              ebeln TYPE ebeln,
              ebelp TYPE ebelp,
              pstyp TYPE pstyp,
            END OF ty_ekpo,

*    *-----------------------------------Structure for MAKT tables values----------------------------

            BEGIN OF ty_makt,
              matnr TYPE matnr,
              spras TYPE spras,
              maktx TYPE maktx,
            END OF ty_makt,
*    *-----------------------------------Structure for EKET tables values----------------------------
            BEGIN OF ty_eket,
              ebeln TYPE ebeln,
              ebelp TYPE ebelp,
              etenr TYPE etenr,
              eindt TYPE eindt,
            END OF ty_eket,

*    *-----------------------------------Structure for QAMB tables values----------------------------

            BEGIN OF ty_qamb,
              prueflos TYPE qplos,
              mblnr    TYPE mblnr,
              zeile    TYPE mblpo,
            END OF ty_qamb,

*    *-----------------------------------Structure for QALS tables values----------------------------

            BEGIN OF ty_qals,
              prueflos TYPE qplos,
              lmenge01 TYPE qlmenge01f,
              lmenge04 TYPE qlmenge04f,
            END OF ty_qals,

*    *-----------------------------------Structure for Final tables values----------------------------

            BEGIN OF ty_final,
              recordtype(8) TYPE c,
              grnno         TYPE mseg-mblnr,
              grnitem       TYPE mseg-zeile,
              grndate       TYPE mkpf-budat,
              branchid      TYPE mseg-werks,
              vendorcode    TYPE mseg-lifnr,
              vendorname    TYPE lfa1-name1,
              pono          TYPE mseg-ebeln,
              poitemlineno  TYPE mseg-ebelp,
              podate        TYPE ekko-aedat,
              scheduleno    TYPE eket-etenr,
              scheduledate  TYPE eket-eindt,
              supinvno      TYPE mkpf-xblnr,
              supinvdate    TYPE mkpf-bldat,
              itemcode      TYPE mseg-matnr,
              description   TYPE makt-maktx,
              uom           TYPE mseg-meins,
              grnqty        TYPE mseg-menge,
              acceptedqty   TYPE qals-lmenge01,
              rejectedqty   TYPE qals-lmenge07,
              asnno         TYPE c LENGTH 20,
            END OF ty_final.

*-----------------------------------Data Declaration for Internal tables  and Work area----------------------------

    DATA : ls_mkpf   TYPE ty_mkpf,
           lt_mkpf   TYPE TABLE OF ty_mkpf,
           ls_mseg   TYPE ty_mseg,
           ls_mseg1  TYPE ty_mseg,
           lt_mseg1  TYPE STANDARD TABLE OF ty_mseg WITH NON-UNIQUE SORTED KEY mblnr COMPONENTS mblnr,
           ls_mseg_n TYPE ty_mseg_n,
           lt_mseg_n TYPE TABLE OF ty_mseg_n,
           ls_nmseg  TYPE ty_mseg,
           lt_nmseg  TYPE TABLE OF ty_mseg,
           ls_lfa1   TYPE ty_lfa1,
           lt_lfa1   TYPE TABLE OF ty_lfa1,
           ls_ekko   TYPE ty_ekko,
           lt_ekko   TYPE TABLE OF ty_ekko,
           ls_ekpo   TYPE ty_ekpo,
           lt_ekpo   TYPE TABLE OF ty_ekpo,
           ls_eket   TYPE ty_eket,
           lt_eket   TYPE TABLE OF ty_eket,
           ls_qamb   TYPE ty_qamb,
           lt_qamb   TYPE TABLE OF ty_qamb,
           ls_qals   TYPE ty_qals,
           lt_qals   TYPE TABLE OF ty_qals,
           ls_makt   TYPE ty_makt,
           lt_makt   TYPE TABLE OF ty_makt,
           ls_final  TYPE ty_final,
           lt_final  TYPE TABLE OF ty_final,
           ls_json   TYPE string.

    FIELD-SYMBOLS : <fs_mseg> TYPE ty_mseg.

**----------Query Parameter for input data from user ------------------
    DATA(l_value) = mo_request->get_uri_query_parameter( 'AEDAT' ).
    DATA(l_value1) = mo_request->get_uri_query_parameter( 'DATE' ).
*
    IF l_value IS NOT INITIAL.         "Delta Load
      SELECT  mblnr,
              zeile,
              bwart,
              matnr,
              werks,
              lifnr,
              menge,
              meins,
              ebeln,
              ebelp,
              cpudt_mkpf,
              vbeln_im,
              vbelp_im
              FROM mseg
              INTO TABLE @lt_mseg1
              WHERE cpudt_mkpf = @l_value
              AND  ( bwart = '101'
              OR bwart = '103'
              OR bwart = '105' ).
    ENDIF.

*------------------------Fetch data from mseg table for if movement type is given----------------------------
    IF l_value1 IS NOT INITIAL.
      SELECT  mblnr,
              zeile,
              bwart,
              matnr,
              werks,
              lifnr,
              menge,
              meins,
              ebeln,
              ebelp,
              cpudt_mkpf,
              vbeln_im,
              vbelp_im
              FROM mseg
              INTO TABLE @lt_mseg1
              WHERE cpudt_mkpf GE @l_value1 AND ( bwart = '101'
              OR bwart = '103'
              OR bwart = '105' ).
    ENDIF.

*    IF lt_mseg1[] IS NOT INITIAL.
*      SELECT  mblnr,
*              zeile,
*              bwart,
*              matnr,
*              werks,
*              lifnr,
*              menge,
*              meins,
*              ebeln,
*              ebelp,
*              lfbnr
*              FROM mseg
*              INTO TABLE @lt_mseg_n
*              FOR ALL ENTRIES IN @lt_mseg1
*              WHERE lfbnr = @lt_mseg1-mblnr
*              AND ( bwart = '102'
*              OR bwart = '104'
*              OR bwart = '106' ).                  "#EC CI_NO_TRANSFORM
*    ENDIF.

    IF lt_mseg1 IS NOT INITIAL.
      SORT lt_mseg1 BY ebeln ebelp.
      SELECT ebeln,
             ebelp,
             pstyp
             FROM ekpo
             INTO TABLE @lt_ekpo
             FOR ALL ENTRIES IN @lt_mseg1
             WHERE ebeln = @lt_mseg1-ebeln
             AND   ebelp = @lt_mseg1-ebelp
             AND   pstyp NE '3'.

      REFRESH lt_mseg1[].
    ENDIF.

    IF lt_ekpo IS NOT INITIAL.
      SORT lt_ekpo BY ebeln ebelp.
      SELECT  mblnr,
              zeile,
              bwart,
              matnr,
              werks,
              lifnr,
              menge,
              meins,
              ebeln,
              ebelp,
              cpudt_mkpf,
              vbeln_im,
              vbelp_im
              FROM mseg
              INTO TABLE @lt_mseg1
              FOR ALL ENTRIES IN @lt_ekpo
              WHERE ebeln = @lt_ekpo-ebeln
               AND  ebelp = @lt_ekpo-ebelp.
    ENDIF.

    IF lt_mseg1 IS NOT INITIAL.
      SORT lt_mseg1 BY mblnr.
      SELECT  mblnr,
              zeile,
              bwart,
              matnr,
              werks,
              lifnr,
              menge,
              meins,
              ebeln,
              ebelp,
              lfbnr
              FROM mseg
              INTO TABLE @lt_mseg_n
              FOR ALL ENTRIES IN @lt_mseg1
              WHERE lfbnr = @lt_mseg1-mblnr
              AND ( bwart = '102'
              OR bwart = '104'
              OR bwart = '106' ).

      SORT lt_mseg1 BY mblnr.
      SORT lt_mseg_n BY lfbnr.
    ENDIF.

*
*    LOOP AT lt_mseg_n[] INTO <fs_mseg>_n.
*      READ TABLE lt_mseg[] INTO <fs_mseg> WITH KEY mblnr = <fs_mseg>_n-lfbnr
*                                               BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        DELETE lt_mseg[] WHERE mblnr = <fs_mseg>_n-lfbnr.
*      ENDIF.
*      CLEAR <fs_mseg>.
*      CLEAR <fs_mseg>_n.
*    ENDLOOP.

    DATA(lt_mseg) = FILTER #( lt_mseg1 USING KEY mblnr
                                       EXCEPT IN lt_mseg_n
                                       WHERE mblnr = lfbnr ).

    IF lt_mseg[] IS NOT INITIAL.
      SELECT  mblnr,
              bldat,
              budat
              FROM mkpf
              INTO TABLE @lt_mkpf
              FOR ALL ENTRIES IN @lt_mseg
              WHERE mblnr = @lt_mseg-mblnr.


*------------------------Fetch data from LFA1 table for Vendor details----------------------------
      SELECT lifnr,
             name1
             FROM lfa1
             INTO TABLE @lt_lfa1
             FOR ALL ENTRIES IN @lt_mseg
             WHERE lifnr = @lt_mseg-lifnr.
*------------------------Fetch data from Ekko table for Change purchase order date details----------------------------

      SELECT ebeln,
             bstyp,
             aedat
             FROM ekko
             INTO TABLE @lt_ekko
             FOR ALL ENTRIES IN @lt_mseg
             WHERE ebeln = @lt_mseg-ebeln.

*------------------------Fetch data from MAKT table for Purchase Requisition details----------------------------

      SELECT matnr,
             spras,
             maktx
             FROM makt
             INTO TABLE @lt_makt
             FOR ALL ENTRIES IN @lt_mseg
             WHERE matnr = @lt_mseg-matnr
             AND   spras = 'E'.

      SELECT ebeln,
             ebelp,
             etenr,
             eindt
             FROM eket
             INTO TABLE @lt_eket
             FOR ALL ENTRIES IN @lt_mseg
             WHERE ebeln = @lt_mseg-ebeln
              AND  ebelp = @lt_mseg-ebelp.

      SELECT prueflos,
             mblnr,
             zeile
            FROM qamb
            INTO TABLE @lt_qamb
            FOR ALL ENTRIES IN @lt_mseg
            WHERE mblnr = @lt_mseg-mblnr
            AND   zeile = @lt_mseg-zeile.

      IF lt_qamb[] IS NOT INITIAL.
        SELECT prueflos,
               lmenge01,
               lmenge04
               FROM qals
               INTO TABLE @lt_qals
               FOR ALL ENTRIES IN @lt_qamb
               WHERE prueflos = @lt_qamb-prueflos. "#EC CI_NO_TRANSFORM
      ENDIF.

      IF lt_mseg IS NOT INITIAL.
        SELECT asnno,
               delivery,
               delivery_item
               FROM zasn_create
               INTO TABLE @DATA(lt_asn)
               FOR ALL ENTRIES IN @lt_mseg
               WHERE delivery      = @lt_mseg-vbeln_im
               AND   delivery_item = @lt_mseg-vbelp_im.
      ENDIF.
    ENDIF.
*    DATA : ls_asn LIKE LINE OF lt_asn.

    IF lt_mseg[] IS NOT INITIAL.

      SORT lt_mkpf[] BY mblnr.
      SORT lt_mseg[] BY mblnr.
      SORT lt_lfa1[] BY lifnr.
      SORT lt_ekko[] BY ebeln.
      SORT lt_makt[] BY matnr.
      SORT lt_eket[] BY ebeln ebelp.
      SORT lt_qamb[] BY mblnr zeile.
      SORT lt_qals[] BY prueflos.
      SORT lt_asn[]  BY delivery delivery_item.


*------------------------Loop the data into final internal table  ----------------------------
      LOOP AT lt_mseg[] ASSIGNING <fs_mseg> WHERE ( bwart = '101'
                                                 OR bwart = '103'
                                                 OR bwart = '105' ).
*        READ TABLE lt_mseg_n[] INTO ls_mseg_n WITH KEY lfbnr = <fs_mseg>-mblnr
*                                               BINARY SEARCH.
*        IF sy-subrc IS NOT INITIAL.
        ls_final-grnno          = <fs_mseg>-mblnr.
        ls_final-grnitem        = <fs_mseg>-zeile.
        ls_final-branchid       = <fs_mseg>-werks.
        ls_final-vendorcode     = <fs_mseg>-lifnr.
        ls_final-pono           = <fs_mseg>-ebeln.
        ls_final-poitemlineno   = <fs_mseg>-ebelp.
        ls_final-itemcode       = <fs_mseg>-matnr.
        ls_final-uom            = <fs_mseg>-meins.
        ls_final-grnqty         = <fs_mseg>-menge.

        IF l_value IS NOT INITIAL.
          ls_final-recordtype = TEXT-002. "New
        ENDIF.

        READ TABLE lt_mkpf[] INTO ls_mkpf WITH KEY mblnr = <fs_mseg>-mblnr
                                                  BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_final-grndate = ls_mkpf-budat.
          ls_final-supinvno = ls_mkpf-xblnr.
          ls_final-supinvdate = ls_mkpf-bldat.
        ENDIF.

        READ TABLE lt_lfa1[] INTO ls_lfa1 WITH KEY lifnr = <fs_mseg>-lifnr
                                                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_final-vendorname = ls_lfa1-name1.
        ENDIF.
        READ TABLE lt_ekko[] INTO ls_ekko WITH KEY ebeln = <fs_mseg>-ebeln
                                                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_final-podate = ls_ekko-aedat.
        ENDIF.
        READ TABLE lt_makt[] INTO ls_makt WITH KEY matnr = <fs_mseg>-matnr
                                                 spras = 'E'
                                                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_final-description = ls_makt-maktx.
        ENDIF.

        READ TABLE lt_eket[] INTO ls_eket WITH KEY ebeln = <fs_mseg>-ebeln
                                                  ebelp = <fs_mseg>-ebelp
                                                  BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_final-scheduleno = ls_eket-etenr.
          ls_final-scheduledate = ls_eket-eindt.
        ENDIF.

        READ TABLE lt_qamb[] INTO ls_qamb WITH KEY  mblnr = <fs_mseg>-mblnr
                                                  zeile = <fs_mseg>-zeile
                                                  BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE lt_qals[] INTO ls_qals WITH KEY  prueflos = ls_qamb-prueflos
                                                    BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_final-acceptedqty = ls_qals-lmenge01.
            ls_final-rejectedqty = ls_qals-lmenge04.
          ENDIF.
        ENDIF.

        IF <fs_mseg>-bwart = 105 OR <fs_mseg>-bwart = 103.
          READ TABLE lt_asn[] INTO DATA(ls_asn) WITH KEY delivery = <fs_mseg>-vbeln_im
                                                         delivery_item = <fs_mseg>-vbelp_im
                                                         BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            ls_final-asnno = ls_asn-asnno.

            UPDATE zasn_create SET grnno = ls_mkpf-mblnr WHERE delivery      = ls_asn-delivery
                                                           AND delivery_item = ls_asn-delivery_item.
          ENDIF.
        ENDIF.
        APPEND ls_final TO lt_final[].
*        ENDIF.
        CLEAR ls_mkpf.
        CLEAR <fs_mseg>.
        CLEAR ls_mseg_n.
        CLEAR ls_lfa1.
        CLEAR ls_ekko.
        CLEAR ls_makt.
        CLEAR ls_eket.
        CLEAR ls_qamb.
        CLEAR ls_qals.
        CLEAR ls_final.
      ENDLOOP.
    ENDIF.

**------------------------Convert internal table into Json format  ----------------------
    IF lt_final[] IS NOT INITIAL.
      CALL METHOD /ui2/cl_json=>serialize(
        EXPORTING
          data   = lt_final
        RECEIVING
          r_json = ls_json
      ).
    ELSE.
      CALL METHOD /ui2/cl_json=>serialize(
        EXPORTING
          data   = TEXT-001
        RECEIVING
          r_json = ls_json
      ).
    ENDIF.
    mo_response->create_entity( )->set_string_data( iv_data = ls_json ).


  ENDMETHOD.
