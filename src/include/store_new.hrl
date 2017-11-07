-record(mcht_txn_acc, {
acc_index,mcht_id,txn_type,month_date,acc
}).
-record(ums_reconcile_result, {
id,settlement_date,txn_date,txn_time,ums_mcht_id,term_id,bank_card_no,txn_amt,txn_type,txn_fee,term_batch_no,term_seq,sys_trace_no,ref_id,auth_resp_code,cooperate_fee,cooperate_mcht_id,up_txn_seq,ums_order_id,memo
}).
-record(mchants, {
id,mcht_full_name,mcht_short_name,status,payment_method,up_mcht_id,quota,up_term_no,update_ts
}).
-record(up_txn_log, {
mcht_index_key,txn_type,up_merId,up_txnTime,up_orderId,up_txnAmt,up_reqReserved,up_orderDesc,up_issInsCode,up_index_key,up_queryId,up_respCode,up_respMsg,up_settleAmt,up_settleDate,up_traceNo,up_traceTime,up_query_index_key,txn_status,up_accNo
}).
-record(mcht_txn_log, {
mcht_index_key,txn_type,mcht_id,mcht_txn_date,mcht_txn_time,mcht_txn_seq,mcht_txn_amt,mcht_order_desc,gateway_id,bank_id,prod_id,prod_bank_acct_id,prod_bank_acct_corp_name,prod_bank_name,mcht_back_url,mcht_front_url,prod_memo,query_id,settle_date,quota,resp_code,resp_msg,orig_mcht_txn_date,orig_mcht_txn_seq,orig_query_id,txn_status,bank_card_no
}).
