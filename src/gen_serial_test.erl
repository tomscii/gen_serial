-module(gen_serial_test).

%% Exports used by the 8 bit clean transfer test.
-export([test_8bit_clean/2, test_8bit_clean/3, ebc_dest/3]).

test_8bit_clean(SrcDevice, DestDevice) ->
	test_8bit_clean(SrcDevice, node(), DestDevice).

test_8bit_clean(SrcDevice, DestNode, DestDevice) ->
	SerialCfg = [{packet, {fixed, 256}}],
	DestPid = spawn(
		DestNode,
		?MODULE, ebc_dest, [self(), DestDevice, SerialCfg]
	),
	receive
	{DestPid, ok} ->
		ebc_src(DestPid, SrcDevice, SerialCfg);
	{DestPid, Other} ->
		Other
	end.

ebc_src(DestPid, SrcDevice, SerialCfg) ->
	case gen_serial:open(SrcDevice, SerialCfg) of
	{ok, SrcPort} ->
		Ret = ebc_src_send(DestPid, SrcPort),
		gen_serial:close(SrcPort),
		Ret;
	Other ->
		Other
	end.

ebc_src_send(DestPid, SrcPort) ->
	Packet = all_bytes(),
	case gen_serial:send(SrcPort, Packet) of
	ok ->
		receive
		{DestPid, timeout} ->
			{error, {timeout, dest}};
		{DestPid, data, Packet} ->
			ok;
		{DestPid, data, _} ->
			{error, "Packet was incorrect."}
		after 5000 ->
			DestPid ! exit,
			{error, {timeout, src}}
		end;
	Other ->
		Other
	end.

ebc_dest(MasterPid, DestDevice, SerialCfg) ->
	case gen_serial:open(DestDevice, SerialCfg) of
	{ok, DestPort} ->
		MasterPid ! {self(), ok},
		receive
		exit ->
			ok;
		{serial, DestPort, Packet} ->
			MasterPid ! {self(), data, Packet}
		after 5000 ->
			MasterPid ! {self(), timeout}
		end,
		gen_serial:close(DestPort);
	Other ->
		MasterPid ! {self(), Other}
	end.

all_bytes() ->		list_to_binary(all_bytes1(255, [])).
all_bytes1(0, T) ->	[0 | T];
all_bytes1(C, T) ->	all_bytes1(C - 1, [C | T]).
