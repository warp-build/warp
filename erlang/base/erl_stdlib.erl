-module(erl_stdlib).

-export([modules/0]).
-export([is_user_module/1]).
-export([is_user_include/1]).
-export([erl_ext/0]).
-export([file_to_module/1]).
-export([files_to_modules/1]).

erl_ext() -> ".erl".

file_to_module(Path) ->
  BaseName = filename:basename(Path),
  ModName = filename:basename(Path, erl_ext()),
  case BaseName =:= ModName of
    true -> {error, path_is_not_a_module};
    false when is_list(ModName) -> {ok, erlang:list_to_atom(ModName)};
    false when is_binary(ModName) -> {ok, erlang:binary_to_atom(ModName, utf8)}
  end.

files_to_modules(Srcs) ->
  lists:filtermap(
    fun (Src) ->
        case file_to_module(Src) of
          {ok, ModName} -> {true, ModName};
          {error, path_is_not_a_module} -> false
        end
    end, Srcs).

is_user_include(Mod) -> 
  Path = path:new(Mod),
  not path:contains(Path, "dist/lib/erlang/lib").

is_user_module(Mod) -> not lists:member(Mod, modules()).


modules() ->
  rebar3_modules()
  ++ [
      alarm_handler,
      application,
      application_controller,
      application_master,
      application_starter,
      appmon_info,
      array,
      asn1_db,
      asn1ct,
      asn1ct_check,
      asn1ct_constructed_ber_bin_v2,
      asn1ct_constructed_per,
      asn1ct_eval_ext,
      asn1ct_func,
      asn1ct_gen,
      asn1ct_gen_ber_bin_v2,
      asn1ct_gen_check,
      asn1ct_gen_jer,
      asn1ct_gen_per,
      asn1ct_imm,
      asn1ct_name,
      asn1ct_parser2,
      asn1ct_pretty_format,
      asn1ct_rtt,
      asn1ct_table,
      asn1ct_tok,
      asn1ct_value,
      asn1rt_nif,
      atomics,
      auth,
      base64,
      beam_a,
      beam_asm,
      beam_block,
      beam_call_types,
      beam_clean,
      beam_dict,
      beam_digraph,
      beam_disasm,
      beam_flatten,
      beam_jump,
      beam_kernel_to_ssa,
      beam_lib,
      beam_listing,
      beam_opcodes,
      beam_peep,
      beam_ssa,
      beam_ssa_bc_size,
      beam_ssa_bool,
      beam_ssa_bsm,
      beam_ssa_codegen,
      beam_ssa_dead,
      beam_ssa_funs,
      beam_ssa_lint,
      beam_ssa_opt,
      beam_ssa_pp,
      beam_ssa_pre_codegen,
      beam_ssa_recv,
      beam_ssa_share,
      beam_ssa_throw,
      beam_ssa_type,
      beam_trim,
      beam_types,
      beam_utils,
      beam_validator,
      beam_z,
      binary,
      c,
      calendar,
      cdv_atom_cb,
      cdv_bin_cb,
      cdv_detail_wx,
      cdv_dist_cb,
      cdv_ets_cb,
      cdv_fun_cb,
      cdv_gen_cb,
      cdv_html_wx,
      cdv_info_wx,
      cdv_int_tab_cb,
      cdv_mem_cb,
      cdv_mod_cb,
      cdv_multi_wx,
      cdv_persistent_cb,
      cdv_port_cb,
      cdv_proc_cb,
      cdv_sched_cb,
      cdv_table_wx,
      cdv_term_cb,
      cdv_timer_cb,
      cdv_virtual_list_wx,
      cdv_wx,
      cerl,
      cerl_clauses,
      cerl_closurean,
      cerl_inline,
      cerl_lib,
      cerl_pmatch,
      cerl_prettypr,
      cerl_trees,
      cerl_typean,
      code,
      code_server,
      compile,
      core_lib,
      core_lint,
      core_parse,
      core_pp,
      core_scan,
      counters,
      cover,
      cprof,
      cpu_sup,
      crashdump_viewer,
      crypto,
      crypto_ec_curves,
      ct,
      ct_config,
      ct_config_plain,
      ct_config_xml,
      ct_conn_log_h,
      ct_cover,
      ct_default_gl,
      ct_event,
      ct_framework,
      ct_ftp,
      ct_gen_conn,
      ct_groups,
      ct_hooks,
      ct_hooks_lock,
      ct_logs,
      ct_make,
      ct_master,
      ct_master_event,
      ct_master_logs,
      ct_master_status,
      ct_netconfc,
      ct_property_test,
      ct_release_test,
      ct_repeat,
      ct_rpc,
      ct_run,
      ct_slave,
      ct_snmp,
      ct_ssh,
      ct_suite,
      ct_telnet,
      ct_telnet_client,
      ct_testspec,
      ct_util,
      cth_conn_log,
      cth_log_redirect,
      cth_surefire,
      dbg,
      dbg_debugged,
      dbg_icmd,
      dbg_idb,
      dbg_ieval,
      dbg_iload,
      dbg_iserver,
      dbg_istk,
      dbg_wx_break,
      dbg_wx_break_win,
      dbg_wx_code,
      dbg_wx_filedialog_win,
      dbg_wx_interpret,
      dbg_wx_mon,
      dbg_wx_mon_win,
      dbg_wx_settings,
      dbg_wx_src_view,
      dbg_wx_trace,
      dbg_wx_trace_win,
      dbg_wx_view,
      dbg_wx_win,
      dbg_wx_winman,
      debugger,
      dets,
      dets_server,
      dets_sup,
      dets_utils,
      dets_v9,
      dialyzer,
      dialyzer_analysis_callgraph,
      dialyzer_behaviours,
      dialyzer_callgraph,
      dialyzer_cl,
      dialyzer_cl_parse,
      dialyzer_clean_core,
      dialyzer_codeserver,
      dialyzer_contracts,
      dialyzer_coordinator,
      dialyzer_dataflow,
      dialyzer_dep,
      dialyzer_dot,
      dialyzer_explanation,
      dialyzer_gui_wx,
      dialyzer_options,
      dialyzer_plt,
      dialyzer_race_data_server,
      dialyzer_races,
      dialyzer_succ_typings,
      dialyzer_timing,
      dialyzer_typesig,
      dialyzer_utils,
      dialyzer_worker,
      diameter,
      diameter_app,
      diameter_callback,
      diameter_capx,
      diameter_codec,
      diameter_codegen,
      diameter_config,
      diameter_config_sup,
      diameter_dbg,
      diameter_dict_parser,
      diameter_dict_scanner,
      diameter_dict_util,
      diameter_dist,
      diameter_etcp,
      diameter_etcp_sup,
      diameter_exprecs,
      diameter_gen,
      diameter_gen_acct_rfc6733,
      diameter_gen_base_accounting,
      diameter_gen_base_rfc3588,
      diameter_gen_base_rfc6733,
      diameter_gen_doic_rfc7683,
      diameter_gen_relay,
      diameter_info,
      diameter_lib,
      diameter_make,
      diameter_misc_sup,
      diameter_peer,
      diameter_peer_fsm,
      diameter_peer_fsm_sup,
      diameter_reg,
      diameter_sctp,
      diameter_sctp_sup,
      diameter_service,
      diameter_service_sup,
      diameter_session,
      diameter_stats,
      diameter_sup,
      diameter_sync,
      diameter_tcp,
      diameter_tcp_sup,
      diameter_traffic,
      diameter_transport,
      diameter_transport_sup,
      diameter_types,
      diameter_watchdog,
      diameter_watchdog_sup,
      dict,
      digraph,
      digraph_utils,
      disk_log,
      disk_log_1,
      disk_log_server,
      disk_log_sup,
      disksup,
      dist_ac,
      dist_util,
      docgen_edoc_xml_cb,
      docgen_otp_specs,
      docgen_xmerl_xml_cb,
      docgen_xml_to_chunk,
      dtls_connection,
      dtls_connection_sup,
      dtls_gen_connection,
      dtls_handshake,
      dtls_listener_sup,
      dtls_packet_demux,
      dtls_record,
      dtls_server_session_cache_sup,
      dtls_server_sup,
      dtls_socket,
      dtls_sup,
      dtls_v1,
      dyntrace,
      edlin,
      edlin_expand,
      edoc,
      edoc_cli,
      edoc_data,
      edoc_doclet,
      edoc_doclet_chunks,
      edoc_extract,
      edoc_layout,
      edoc_layout_chunks,
      edoc_lib,
      edoc_macros,
      edoc_parser,
      edoc_refs,
      edoc_report,
      edoc_run,
      edoc_scanner,
      edoc_specs,
      edoc_tags,
      edoc_types,
      edoc_wiki,
      eldap,
      epp,
      epp_dodger,
      eprof,
      erl2html2,
      erl_abstract_code,
      erl_anno,
      erl_bif_types,
      erl_bifs,
      erl_bits,
      erl_boot_server,
      erl_comment_scan,
      erl_compile,
      erl_compile_server,
      erl_ddll,
      erl_distribution,
      erl_driver,
      erl_epmd,
      erl_error,
      erl_erts_errors,
      erl_eval,
      erl_expand_records,
      erl_features,
      erl_id_trans,
      erl_init,
      erl_internal,
      erl_kernel_errors,
      erl_lint,
      erl_nif,
      erl_parse,
      erl_posix_msg,
      erl_pp,
      erl_prettypr,
      erl_prim_loader,
      erl_recomment,
      erl_reply,
      erl_scan,
      erl_signal_handler,
      erl_stdlib_errors,
      erl_syntax,
      erl_syntax_lib,
      erl_tar,
      erl_tracer,
      erl_types,
      erlang,
      erlsrv,
      erpc,
      error_handler,
      error_logger,
      error_logger_file_h,
      error_logger_tty_h,
      erts_alloc_config,
      erts_code_purger,
      erts_debug,
      erts_dirty_process_signal_handler,
      erts_internal,
      erts_literal_area_collector,
      escript,
      et,
      et_collector,
      et_selector,
      et_viewer,
      et_wx_contents_viewer,
      et_wx_viewer,
      etop,
      etop_tr,
      etop_txt,
      ets,
      eunit,
      eunit_autoexport,
      eunit_data,
      eunit_lib,
      eunit_listener,
      eunit_proc,
      eunit_serial,
      eunit_server,
      eunit_striptests,
      eunit_surefire,
      eunit_test,
      eunit_tests,
      eunit_tty,
      eval_bits,
      file,
      file_io_server,
      file_server,
      file_sorter,
      filelib,
      filename,
      format_lib_supp,
      fprof,
      ftp,
      ftp_app,
      ftp_progress,
      ftp_response,
      ftp_sup,
      gb_sets,
      gb_trees,
      gen,
      gen_event,
      gen_fsm,
      gen_sctp,
      gen_server,
      gen_statem,
      gen_tcp,
      gen_tcp_socket,
      gen_udp,
      gen_udp_socket,
      gl,
      global,
      global_group,
      global_search,
      glu,
      group,
      group_history,
      heart,
      http_chunk,
      http_request,
      http_response,
      http_transport,
      http_uri,
      http_util,
      httpc,
      httpc_cookie,
      httpc_handler,
      httpc_handler_sup,
      httpc_manager,
      httpc_profile_sup,
      httpc_request,
      httpc_response,
      httpc_sup,
      httpd,
      httpd_acceptor,
      httpd_acceptor_sup,
      httpd_cgi,
      httpd_conf,
      httpd_connection_sup,
      httpd_custom,
      httpd_custom_api,
      httpd_esi,
      httpd_example,
      httpd_file,
      httpd_instance_sup,
      httpd_log,
      httpd_logger,
      httpd_manager,
      httpd_misc_sup,
      httpd_request,
      httpd_request_handler,
      httpd_response,
      httpd_script_env,
      httpd_socket,
      httpd_sup,
      httpd_util,
      i,
      inet,
      inet6_sctp,
      inet6_tcp,
      inet6_tcp_dist,
      inet6_tls_dist,
      inet6_udp,
      inet_config,
      inet_db,
      inet_dns,
      inet_gethost_native,
      inet_hosts,
      inet_parse,
      inet_res,
      inet_sctp,
      inet_tcp,
      inet_tcp_dist,
      inet_tls_dist,
      inet_udp,
      inets,
      inets_app,
      inets_lib,
      inets_service,
      inets_sup,
      inets_trace,
      init,
      instrument,
      int,
      io,
      io_lib,
      io_lib_format,
      io_lib_format_ryu_table,
      io_lib_fread,
      io_lib_pretty,
      kernel,
      kernel_config,
      kernel_refc,
      lcnt,
      leex,
      lists,
      local_tcp,
      local_udp,
      log_mf_h,
      logger,
      logger_backend,
      logger_config,
      logger_disk_log_h,
      logger_filters,
      logger_formatter,
      logger_h_common,
      logger_handler_watcher,
      logger_olp,
      logger_proxy,
      logger_server,
      logger_simple_h,
      logger_std_h,
      logger_sup,
      make,
      maps,
      math,
      megaco,
      megaco_ber_encoder,
      megaco_ber_media_gateway_control_v1,
      megaco_ber_media_gateway_control_v2,
      megaco_ber_media_gateway_control_v3,
      megaco_binary_encoder,
      megaco_binary_encoder_lib,
      megaco_binary_name_resolver_v1,
      megaco_binary_name_resolver_v2,
      megaco_binary_name_resolver_v3,
      megaco_binary_term_id,
      megaco_binary_term_id_gen,
      megaco_binary_transformer_v1,
      megaco_binary_transformer_v2,
      megaco_binary_transformer_v3,
      megaco_compact_text_encoder,
      megaco_compact_text_encoder_v1,
      megaco_compact_text_encoder_v2,
      megaco_compact_text_encoder_v3,
      megaco_config,
      megaco_config_misc,
      megaco_digit_map,
      megaco_edist_compress,
      megaco_encoder,
      megaco_erl_dist_encoder,
      megaco_erl_dist_encoder_mc,
      megaco_filter,
      megaco_flex_scanner,
      megaco_flex_scanner_handler,
      megaco_messenger,
      megaco_messenger_misc,
      megaco_misc_sup,
      megaco_monitor,
      megaco_per_encoder,
      megaco_per_media_gateway_control_v1,
      megaco_per_media_gateway_control_v2,
      megaco_per_media_gateway_control_v3,
      megaco_pretty_text_encoder,
      megaco_pretty_text_encoder_v1,
      megaco_pretty_text_encoder_v2,
      megaco_pretty_text_encoder_v3,
      megaco_sdp,
      megaco_stats,
      megaco_sup,
      megaco_tcp,
      megaco_tcp_accept,
      megaco_tcp_accept_sup,
      megaco_tcp_connection,
      megaco_tcp_connection_sup,
      megaco_tcp_sup,
      megaco_text_mini_decoder,
      megaco_text_mini_parser,
      megaco_text_parser_v1,
      megaco_text_parser_v2,
      megaco_text_parser_v3,
      megaco_text_scanner,
      megaco_timer,
      megaco_trans_sender,
      megaco_trans_sup,
      megaco_transport,
      megaco_udp,
      megaco_udp_server,
      megaco_udp_sup,
      megaco_user,
      megaco_user_default,
      memsup,
      merl,
      merl_transform,
      misc_supp,
      mnesia,
      mnesia_app,
      mnesia_backend_type,
      mnesia_backup,
      mnesia_bup,
      mnesia_checkpoint,
      mnesia_checkpoint_sup,
      mnesia_controller,
      mnesia_dumper,
      mnesia_event,
      mnesia_ext_sup,
      mnesia_frag,
      mnesia_frag_hash,
      mnesia_index,
      mnesia_kernel_sup,
      mnesia_late_loader,
      mnesia_lib,
      mnesia_loader,
      mnesia_locker,
      mnesia_log,
      mnesia_monitor,
      mnesia_recover,
      mnesia_registry,
      mnesia_rpc,
      mnesia_schema,
      mnesia_snmp_hook,
      mnesia_sp,
      mnesia_subscr,
      mnesia_sup,
      mnesia_text,
      mnesia_tm,
      mod_actions,
      mod_alias,
      mod_auth,
      mod_auth_dets,
      mod_auth_mnesia,
      mod_auth_plain,
      mod_auth_server,
      mod_cgi,
      mod_dir,
      mod_disk_log,
      mod_esi,
      mod_get,
      mod_head,
      mod_log,
      mod_range,
      mod_responsecontrol,
      mod_security,
      mod_security_server,
      mod_trace,
      ms_transform,
      msacc,
      net,
      net_adm,
      net_kernel,
      nteventlog,
      observer,
      observer_alloc_wx,
      observer_app_wx,
      observer_backend,
      observer_html_lib,
      observer_lib,
      observer_perf_wx,
      observer_port_wx,
      observer_pro_wx,
      observer_procinfo,
      observer_sock_wx,
      observer_sys_wx,
      observer_trace_wx,
      observer_traceoptions_wx,
      observer_tv_table,
      observer_tv_wx,
      observer_wx,
      orddict,
      ordsets,
      os,
      os_mon,
      os_mon_mib,
      os_mon_sysinfo,
      os_sup,
      otp_internal,
      peer,
      persistent_term,
      pg,
      pg2,
      pool,
      prettypr,
      prim_buffer,
      prim_eval,
      prim_file,
      prim_inet,
      prim_net,
      prim_socket,
      prim_zip,
      proc_lib,
      proplists,
      pubkey_cert,
      pubkey_cert_records,
      pubkey_crl,
      pubkey_ocsp,
      pubkey_pbe,
      pubkey_pem,
      pubkey_ssh,
      public_key,
      qlc,
      qlc_pt,
      queue,
      ram_file,
      rand,
      random,
      raw_file_io,
      raw_file_io_compressed,
      raw_file_io_deflate,
      raw_file_io_delayed,
      raw_file_io_inflate,
      raw_file_io_list,
      rb,
      rb_format_supp,
      re,
      rec_env,
      release_handler,
      release_handler_1,
      reltool,
      reltool_app_win,
      reltool_fgraph,
      reltool_fgraph_win,
      reltool_mod_win,
      reltool_server,
      reltool_sys_win,
      reltool_target,
      reltool_utils,
      rpc,
      runtime_tools,
      runtime_tools_sup,
      sasl,
      sasl_report,
      sasl_report_file_h,
      sasl_report_tty_h,
      scheduler,
      seq_trace,
      sets,
      shell,
      shell_default,
      shell_docs,
      slave,
      snmp,
      snmp_app,
      snmp_app_sup,
      snmp_community_mib,
      snmp_conf,
      snmp_config,
      snmp_framework_mib,
      snmp_generic,
      snmp_generic_mnesia,
      snmp_index,
      snmp_log,
      snmp_mini_mib,
      snmp_misc,
      snmp_note_store,
      snmp_notification_mib,
      snmp_pdus,
      snmp_shadow_table,
      snmp_standard_mib,
      snmp_target_mib,
      snmp_user_based_sm_mib,
      snmp_usm,
      snmp_verbosity,
      snmp_view_based_acm_mib,
      snmpa,
      snmpa_acm,
      snmpa_agent,
      snmpa_agent_sup,
      snmpa_app,
      snmpa_authentication_service,
      snmpa_conf,
      snmpa_discovery_handler,
      snmpa_discovery_handler_default,
      snmpa_error,
      snmpa_error_io,
      snmpa_error_logger,
      snmpa_error_report,
      snmpa_get,
      snmpa_get_lib,
      snmpa_get_mechanism,
      snmpa_local_db,
      snmpa_mib,
      snmpa_mib_data,
      snmpa_mib_data_tttn,
      snmpa_mib_lib,
      snmpa_mib_storage,
      snmpa_mib_storage_dets,
      snmpa_mib_storage_ets,
      snmpa_mib_storage_mnesia,
      snmpa_misc_sup,
      snmpa_mpd,
      snmpa_net_if,
      snmpa_net_if_filter,
      snmpa_network_interface,
      snmpa_network_interface_filter,
      snmpa_notification_delivery_info_receiver,
      snmpa_notification_filter,
      snmpa_set,
      snmpa_set_lib,
      snmpa_set_mechanism,
      snmpa_supervisor,
      snmpa_svbl,
      snmpa_symbolic_store,
      snmpa_target_cache,
      snmpa_trap,
      snmpa_usm,
      snmpa_vacm,
      snmpc,
      snmpc_lib,
      snmpc_mib_gram,
      snmpc_mib_to_hrl,
      snmpc_misc,
      snmpc_tok,
      snmpm,
      snmpm_conf,
      snmpm_config,
      snmpm_misc_sup,
      snmpm_mpd,
      snmpm_net_if,
      snmpm_net_if_filter,
      snmpm_net_if_mt,
      snmpm_network_interface,
      snmpm_network_interface_filter,
      snmpm_server,
      snmpm_server_sup,
      snmpm_supervisor,
      snmpm_user,
      snmpm_user_default,
      snmpm_user_old,
      snmpm_usm,
      socket,
      socket_registry,
      sofs,
      ssh,
      ssh_acceptor,
      ssh_acceptor_sup,
      ssh_agent,
      ssh_app,
      ssh_auth,
      ssh_bits,
      ssh_channel,
      ssh_channel_sup,
      ssh_cli,
      ssh_client_channel,
      ssh_client_key_api,
      ssh_connection,
      ssh_connection_handler,
      ssh_daemon_channel,
      ssh_dbg,
      ssh_file,
      ssh_fsm_kexinit,
      ssh_fsm_userauth_client,
      ssh_fsm_userauth_server,
      ssh_info,
      ssh_io,
      ssh_lib,
      ssh_message,
      ssh_no_io,
      ssh_options,
      ssh_server_channel,
      ssh_server_key_api,
      ssh_sftp,
      ssh_sftpd,
      ssh_sftpd_file,
      ssh_sftpd_file_api,
      ssh_shell,
      ssh_subsystem_sup,
      ssh_system_sup,
      ssh_tcpip_forward_acceptor,
      ssh_tcpip_forward_acceptor_sup,
      ssh_tcpip_forward_client,
      ssh_tcpip_forward_srv,
      ssh_transport,
      ssh_xfer,
      ssl,
      ssl_admin_sup,
      ssl_alert,
      ssl_app,
      ssl_certificate,
      ssl_cipher,
      ssl_cipher_format,
      ssl_client_session_cache_db,
      ssl_config,
      ssl_connection_sup,
      ssl_crl,
      ssl_crl_cache,
      ssl_crl_cache_api,
      ssl_crl_hash_dir,
      ssl_dh_groups,
      ssl_dist_admin_sup,
      ssl_dist_connection_sup,
      ssl_dist_sup,
      ssl_gen_statem,
      ssl_handshake,
      ssl_listen_tracker_sup,
      ssl_logger,
      ssl_manager,
      ssl_pem_cache,
      ssl_pkix_db,
      ssl_record,
      ssl_server_session_cache,
      ssl_server_session_cache_db,
      ssl_server_session_cache_sup,
      ssl_session,
      ssl_session_cache_api,
      ssl_srp_primes,
      ssl_sup,
      ssl_upgrade_server_session_cache_sup,
      standard_error,
      string,
      supervisor,
      supervisor_bridge,
      sys,
      sys_core_alias,
      sys_core_bsm,
      sys_core_fold,
      sys_core_fold_lists,
      sys_core_inline,
      sys_core_prepare,
      sys_messages,
      sys_pre_attributes,
      system_information,
      systools,
      systools_lib,
      systools_make,
      systools_rc,
      systools_relup,
      tags,
      test_server,
      test_server_ctrl,
      test_server_gl,
      test_server_io,
      test_server_node,
      test_server_sup,
      tftp,
      tftp_app,
      tftp_binary,
      tftp_engine,
      tftp_file,
      tftp_lib,
      tftp_logger,
      tftp_sup,
      timer,
      tls_bloom_filter,
      tls_client_ticket_store,
      tls_connection,
      tls_connection_1_3,
      tls_connection_sup,
      tls_dist_server_sup,
      tls_dist_sup,
      tls_dtls_connection,
      tls_dyn_connection_sup,
      tls_gen_connection,
      tls_handshake,
      tls_handshake_1_3,
      tls_record,
      tls_record_1_3,
      tls_sender,
      tls_server_session_ticket,
      tls_server_session_ticket_sup,
      tls_server_sup,
      tls_socket,
      tls_sup,
      tls_v1,
      ttb,
      ttb_autostart,
      ttb_et,
      typer,
      unicode,
      unicode_util,
      unix_telnet,
      uri_string,
      user,
      user_drv,
      user_sup,
      v3_core,
      v3_kernel,
      v3_kernel_pp,
      win32reg,
      wrap_log_reader,
      wx,
      wxAcceleratorEntry,
      wxAcceleratorTable,
      wxActivateEvent,
      wxArtProvider,
      wxAuiDockArt,
      wxAuiManager,
      wxAuiManagerEvent,
      wxAuiNotebook,
      wxAuiNotebookEvent,
      wxAuiPaneInfo,
      wxAuiSimpleTabArt,
      wxAuiTabArt,
      wxBitmap,
      wxBitmapButton,
      wxBitmapDataObject,
      wxBookCtrlBase,
      wxBookCtrlEvent,
      wxBoxSizer,
      wxBrush,
      wxBufferedDC,
      wxBufferedPaintDC,
      wxButton,
      wxCalendarCtrl,
      wxCalendarDateAttr,
      wxCalendarEvent,
      wxCaret,
      wxCheckBox,
      wxCheckListBox,
      wxChildFocusEvent,
      wxChoice,
      wxChoicebook,
      wxClientDC,
      wxClipboard,
      wxClipboardTextEvent,
      wxCloseEvent,
      wxColourData,
      wxColourDialog,
      wxColourPickerCtrl,
      wxColourPickerEvent,
      wxComboBox,
      wxCommandEvent,
      wxContextMenuEvent,
      wxControl,
      wxControlWithItems,
      wxCursor,
      wxDC,
      wxDCOverlay,
      wxDataObject,
      wxDateEvent,
      wxDatePickerCtrl,
      wxDialog,
      wxDirDialog,
      wxDirPickerCtrl,
      wxDisplay,
      wxDisplayChangedEvent,
      wxDropFilesEvent,
      wxEraseEvent,
      wxEvent,
      wxEvtHandler,
      wxFileDataObject,
      wxFileDialog,
      wxFileDirPickerEvent,
      wxFilePickerCtrl,
      wxFindReplaceData,
      wxFindReplaceDialog,
      wxFlexGridSizer,
      wxFocusEvent,
      wxFont,
      wxFontData,
      wxFontDialog,
      wxFontPickerCtrl,
      wxFontPickerEvent,
      wxFrame,
      wxGBSizerItem,
      wxGCDC,
      wxGLCanvas,
      wxGLContext,
      wxGauge,
      wxGenericDirCtrl,
      wxGraphicsBrush,
      wxGraphicsContext,
      wxGraphicsFont,
      wxGraphicsGradientStops,
      wxGraphicsMatrix,
      wxGraphicsObject,
      wxGraphicsPath,
      wxGraphicsPen,
      wxGraphicsRenderer,
      wxGrid,
      wxGridBagSizer,
      wxGridCellAttr,
      wxGridCellBoolEditor,
      wxGridCellBoolRenderer,
      wxGridCellChoiceEditor,
      wxGridCellEditor,
      wxGridCellFloatEditor,
      wxGridCellFloatRenderer,
      wxGridCellNumberEditor,
      wxGridCellNumberRenderer,
      wxGridCellRenderer,
      wxGridCellStringRenderer,
      wxGridCellTextEditor,
      wxGridEvent,
      wxGridSizer,
      wxHelpEvent,
      wxHtmlEasyPrinting,
      wxHtmlLinkEvent,
      wxHtmlWindow,
      wxIcon,
      wxIconBundle,
      wxIconizeEvent,
      wxIdleEvent,
      wxImage,
      wxImageList,
      wxInitDialogEvent,
      wxJoystickEvent,
      wxKeyEvent,
      wxLayoutAlgorithm,
      wxListBox,
      wxListCtrl,
      wxListEvent,
      wxListItem,
      wxListItemAttr,
      wxListView,
      wxListbook,
      wxLocale,
      wxLogNull,
      wxMDIChildFrame,
      wxMDIClientWindow,
      wxMDIParentFrame,
      wxMask,
      wxMaximizeEvent,
      wxMemoryDC,
      wxMenu,
      wxMenuBar,
      wxMenuEvent,
      wxMenuItem,
      wxMessageDialog,
      wxMiniFrame,
      wxMirrorDC,
      wxMouseCaptureChangedEvent,
      wxMouseCaptureLostEvent,
      wxMouseEvent,
      wxMoveEvent,
      wxMultiChoiceDialog,
      wxNavigationKeyEvent,
      wxNotebook,
      wxNotificationMessage,
      wxNotifyEvent,
      wxOverlay,
      wxPageSetupDialog,
      wxPageSetupDialogData,
      wxPaintDC,
      wxPaintEvent,
      wxPalette,
      wxPaletteChangedEvent,
      wxPanel,
      wxPasswordEntryDialog,
      wxPen,
      wxPickerBase,
      wxPopupTransientWindow,
      wxPopupWindow,
      wxPostScriptDC,
      wxPreviewCanvas,
      wxPreviewControlBar,
      wxPreviewFrame,
      wxPrintData,
      wxPrintDialog,
      wxPrintDialogData,
      wxPrintPreview,
      wxPrinter,
      wxPrintout,
      wxProgressDialog,
      wxQueryNewPaletteEvent,
      wxRadioBox,
      wxRadioButton,
      wxRegion,
      wxSashEvent,
      wxSashLayoutWindow,
      wxSashWindow,
      wxScreenDC,
      wxScrollBar,
      wxScrollEvent,
      wxScrollWinEvent,
      wxScrolledWindow,
      wxSetCursorEvent,
      wxShowEvent,
      wxSingleChoiceDialog,
      wxSizeEvent,
      wxSizer,
      wxSizerFlags,
      wxSizerItem,
      wxSlider,
      wxSpinButton,
      wxSpinCtrl,
      wxSpinEvent,
      wxSplashScreen,
      wxSplitterEvent,
      wxSplitterWindow,
      wxStaticBitmap,
      wxStaticBox,
      wxStaticBoxSizer,
      wxStaticLine,
      wxStaticText,
      wxStatusBar,
      wxStdDialogButtonSizer,
      wxStyledTextCtrl,
      wxStyledTextEvent,
      wxSysColourChangedEvent,
      wxSystemOptions,
      wxSystemSettings,
      wxTaskBarIcon,
      wxTaskBarIconEvent,
      wxTextAttr,
      wxTextCtrl,
      wxTextDataObject,
      wxTextEntryDialog,
      wxToggleButton,
      wxToolBar,
      wxToolTip,
      wxToolbook,
      wxTopLevelWindow,
      wxTreeCtrl,
      wxTreeEvent,
      wxTreebook,
      wxUpdateUIEvent,
      wxWebView,
      wxWebViewEvent,
      wxWindow,
      wxWindowCreateEvent,
      wxWindowDC,
      wxWindowDestroyEvent,
      wxXmlResource,
      wx_misc,
      wx_object,
      wxe_master,
      wxe_server,
      wxe_util,
      xmerl,
      xmerl_b64Bin,
      xmerl_b64Bin_scan,
      xmerl_eventp,
      xmerl_html,
      xmerl_lib,
      xmerl_otpsgml,
      xmerl_regexp,
      xmerl_sax_old_dom,
      xmerl_sax_parser,
      xmerl_sax_parser_latin1,
      xmerl_sax_parser_list,
      xmerl_sax_parser_utf16be,
      xmerl_sax_parser_utf16le,
      xmerl_sax_parser_utf8,
      xmerl_sax_simple_dom,
      xmerl_scan,
      xmerl_sgml,
      xmerl_simple,
      xmerl_text,
      xmerl_ucs,
      xmerl_uri,
      xmerl_validate,
      xmerl_xlate,
      xmerl_xml,
      xmerl_xpath,
      xmerl_xpath_lib,
      xmerl_xpath_parse,
      xmerl_xpath_pred,
      xmerl_xpath_scan,
      xmerl_xs,
      xmerl_xsd,
      xmerl_xsd_type,
      xref,
      xref_base,
      xref_compiler,
      xref_parser,
      xref_reader,
      xref_scanner,
      xref_utils,
      yecc,
      zip,
      zlib
     ].

%===============================================================================
% List of standard rebar3 modules that are only available to plugins.
%===============================================================================

rebar3_modules() -> [
                     rebar_api,
                     rebar_app_info,
                     rebar_config,
                     rebar_dir,
                     rebar_file_utils,
                     rebar_hex_repos,
                     rebar_hooks,
                     rebar_log,
                     rebar_opts,
                     rebar_prv_cover,
                     rebar_resource_v2,
                     rebar_state,
                     rebar_string,
                     rebar_utils
                    ].
