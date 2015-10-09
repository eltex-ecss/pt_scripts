%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-compile({parse_transform, pt_macro}).

-define(FUNC, (pt_macro_define(function))).
-define(FUNC_NAME, (pt_macro_define(function_name))).
-define(FUNC_STRING, (pt_macro_define(function_string))).
-define(ARGS_LIST, (pt_macro_define(args_list))).
-define(REGEXP(Expr), (pt_macro_define(regexp, Expr, []))).
-define(REGEXP_OPT(Expr, Options), (pt_macro_define(regexp, Expr, Options))).