%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-compile({parse_transform, pt_pmodule}).

-define(MPARAM(Param),pt_pmodule:get_param(Param)).