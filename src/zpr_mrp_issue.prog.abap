*&---------------------------------------------------------------------*
*& Report ZPR_MRP_ISSUE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpr_mrp_issue.

* Top Include
INCLUDE zpr_mrp_issue_top.
* Selection Include
INCLUDE zpr_mrp_issue_sel.
* Class Include
INCLUDE zpr_mrp_issue_class.
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*
  DATA(lo_get_data) = NEW lcl_get_data( ).
  DATA(lo_set_output) = NEW lcl_set_output( ).

  DATA: gt_result TYPE lo_get_data->tt_result,
        gt_output TYPE lo_get_data->tt_output.


  lo_get_data->result_data( RECEIVING lt_result = gt_result ).

  lo_get_data->output_data( EXPORTING it_result = gt_result
                            RECEIVING lt_output = gt_output ).

  IF p_alvr = abap_true.
    lo_set_output->alv_recipient( it_output = gt_output ).
  ELSEIF p_mail = abap_true.
    lo_set_output->email_send( it_output  = gt_output ).
  ENDIF.
