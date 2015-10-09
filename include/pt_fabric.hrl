%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-compile({parse_transform, pt_fabric}).

-define(FABRIC_PARAM(Param), pt_fabric:get_param(Param)).
-define(FABRIC_MODULE_NAME, pt_fabric:get_module_name()).