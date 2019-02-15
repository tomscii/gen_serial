%% @doc Generic serial port interface.
%%
%% <p>
%% The gen_serial API allows Erlang programs to make use of standard
%% serial port devices on both Windows and POSIX (Linux and UNIX-like)
%% platforms, from a common interface module. As the native serial
%% port handling services of the underlying operating system are used,
%% this module supports any device supported by the operating system,
%% ie. not just traditional RS-232 serial ports, but also USB-serial
%% converters and the like.
%% </p>
%%
%% <p>
%% External port processes are used to connect to the host serial
%% port APIs, thereby placing each serial port connection in its own
%% memory-protected sandbox.  Should the serial port process crash,
%% only that port will be shutdown; the Erlang node will continue to
%% function properly, as will all other serial ports.  On Windows the
%% serial port processes are named <code>COMn_esock.exe</code>, where
%% the <code>COMn</code> part indicates the name of the serial port
%% being accessed by the program.  On UNIX, the first argument to the
%% program <code>serial_esock</code> has the serial device's name.
%% </p>
%%
%% <p>
%% Much of the API should be similiar to <code>gen_tcp</code> and
%% <code>ssl</code>, making the switch to serial communications
%% easy on Erlang developers.
%% </p>
%%
%% <p>
%% Unlike other Erlang communication APIs, gen_serial only allows use
%% of binaries and lists of binaries as packet input. Character lists
%% (aka strings or IO lists) are just simply not supported at this
%% time.
%% </p>
%%
%% <p>
%% <i>Disclaimer: This is alpha level code.</i>
%% The latest version is available from:
%% <a href="http://github.com/tomszilagyi/gen_serial">
%% http://github.com/tomszilagyi/gen_serial</a>
%% </p>
%%
%% <a name="-messages"><h3>Port Owner Messages:</h3></a>
%% <p>
%% The port owner (see {@link set_owner/2}) is sent a
%% series of messages containing data packets (when the port is active)
%% and close and error messages, when the port is closed or an error
%% related to the port occurs.
%% </p>
%%
%% <b>{serial, PortRef, Packet}</b><br />
%%	<ul>
%%		<li>PortRef = {@type port_ref()}</li>
%%		<li>Packet = {@type binary()}</li>
%%	</ul>
%%	<p>
%%	Sent when a packet of data has been received and decoded by the
%%	serial port driver.  If there is a packet level protocol being
%%	used by the driver, Packet will contain one complete packet of
%%	data.  If no packet level protocol is used, Packet will typically
%%	be a single byte, as the port driver is significantly faster than
%%	the serial port.
%%	</p>
%%
%%	<p>
%%	This message is only sent when the port has the 'active' option
%%	set to 'true' or 'once'.
%%	See <a href="#-options-active">{active, When}</a> for more
%%	information.
%%	</p>
%%
%% <b>{serial_error, PortRef, Error}</b><br />
%%	<ul>
%%		<li>PortRef = {@type port_ref()}</li>
%%		<li>Error = {@type term()}</li>
%%	</ul>
%%	<p>
%%	Sent when the port driver has detected a problem with the serial
%%	port.  The error may be critical and cause the port to close.
%%	</p>
%%
%% <b>{serial_closed, PortRef}</b><br/>
%%	<ul>
%%		<li>PortRef = {@type port_ref()}</li>
%%	</ul>
%%	<p>
%%	Sent when the port is being closed, before the port process goes
%%	down.
%%	</p>
%%
%% <a name="-options"><h3>Available Options:</h3></a>
%% <p>
%% The following options can be used in an {@type option_list()} to
%% configure the serial port for communications with another device.
%% </p>
%%
%% <b>{rcvbuf, Bytes}</b><br/>
%%	<ul>
%%		<li>Bytes = {@type integer()}</li>
%%	</ul>
%% <p>Size of the OS receive buffer (for data coming in from the serial
%% port).  Specified in bytes, must be between 32 and 32,768.  Not
%% all OSes will allow all values in this range.  Default is 4096,
%% which should work on all platforms. Currently this setting has no
%% effect on the POSIX backend.</p>
%%
%% <b>{sndbuf, Bytes}</b><br/>
%%	<ul>
%%		<li>Bytes = {@type integer()}</li>
%%	</ul>
%% <p>Size of the OS send buffer (for data going out the serial
%% port).  Specified in bytes, must be between 32 and 32,768.  Not
%% all OSes will allow all values in this range.  Default is 4096,
%% which should work on all platforms. Currently this setting has no
%% effect on the POSIX backend.</p>
%%
%% <b>{bufsz, Bytes}</b><br/>
%%	<ul>
%%		<li>Bytes = {@type integer()}</li>
%%	</ul>
%% <p>Size of the packet buffers.  If using delimited packets or
%% line-oriented packets, the packet buffer must be sized larger than
%% the largest input line expected, or else the application will
%% receive fragmented packets.  If fixed size packets are being
%% used the bufsz may be set larger or smaller than the actual packet
%% size.  Default is 8192, large enough for most applications.</p>
%%
%% <b>{register, Name}</b><br/>
%%	<ul>
%%		<li>Name = {@type true | false | atom()}</li>
%%	</ul>
%% <p>Register the interface process as a named process, making it
%% visible in the shell tools, etc.  If the atom 'true' is supplied
%% the actual name registered will be a mangled form of the device
%% name.  If 'false' is supplied, no name will be registered for
%% the interface process.  Default is 'false'.</p>
%%
%% <b>register</b><br />
%% <p>Same as {register, true}.</p>
%%
%% <b>{baud, BitsPerSecond}</b> or <b>{baudrate, BitsPerSecond}</b><br/>
%% <p>
%% Supported by Windows backend:
%%	<ul>
%%		<li>BitsPerSecond = {@type 110 | 300 | 600 | 1200 | 2400
%%			| 4800 | 9600 | 14400 | 19200 | 38400 | 56000 | 57600
%%			| 115200 | 128000 | 256000 | integer()}</li>
%%	</ul>
%% Supported by POSIX backend:
%%	<ul>
%%		<li>BitsPerSecond = {@type 110 | 134 | 150 | 200 | 300 | 600
%%                      | 1200 | 2400 | 4800 | 9600 | 19200 | 38400 | 57600
%%                      | 115200 | 230400 | 460800 | 500000 | 576000 | 921600
%%                      | 1000000 | 1152000 | 1500000 | 2000000 | 2500000
%%                      | 3000000 | 3500000 | 4000000}</li>
%%	</ul>
%% </p>
%% <p>Set the baud rate of the serial port, as the number of bits to
%% transfer per second.  Most serial ports will only accept a subset
%% of the baud rates listed above. The interface will accept any baud
%% rate over 1 bit per second and attempt to configure the backend
%% driver for that rate. If a rate listed above for the appropriate
%% backend is rejected, it is because the OS cannot support that rate,
%% or the hardware cannot support that rate. Default is 9600 as this
%% is extremely common.</p>
%%
%% <b>{bytesz, BitsPerByte}</b><br/>
%%	<ul>
%%		<li>BitsPerByte = {@type 5 | 6 | 7 | 8}</li>
%%	</ul>
%% <p>Set the number of bits per data byte. Default is 8. Again, the
%% OS and the device may or may not support a particular setting.</p>
%%
%% <b>{parity, Parity}</b><br/>
%%	<ul>
%%		<li>Parity = {@type none | odd | even}</li>
%%	</ul>
%% <p>Enable or disable parity checking.  Default is none.</p>
%%
%% <b>{stop_bits, StopBits}</b><br/>
%%	<ul>
%%		<li>StopBits = {@type 1 | 1.5 | 2}</li>
%%	</ul>
%% <p>Set the number of stop bits used.  Default is 1. A setting of 1.5
%% is unsupported by the POSIX backend (it has the same effect as 1).</p>
%%
%% <b>{flow_control, Type}</b><br/>
%%	<ul>
%%		<li>Type = {@type none | software | hardware}</li>
%%	</ul>
%% <p>Select the type of flow control which will be used by the serial
%% port. Hardware is also known as RTS/CTS and software as XON/XOFF
%% flow control. Default is hardware as it is the most reliable
%% form.</p>
%%
%% <h4>Packet options</h4>
%%
%% <p>These refer to the incoming packet only. Outgoing packets are
%% sent directly in whatever pieces and units of data the user calls
%% the send functions with.</p>
%%
%% <b>{packet, none}</b><br/>
%% <p>No packet formatting is done by the driver. All bytes are
%% delivered as they are received, one or more bytes at a time
%% (depending on the interface speed and the speed and workload of the
%% system).  If the application needs to assemble packets from the
%% data, it is up to the application developer to properly buffer data
%% and assemble the packets prior to processing.</p>
%%
%% <b>{packet, cr}</b><br />
%% <p>Packets are line oriented, terminated by a single carriage
%% return ($\r, ASCII value 13, hex 0D).  If this packet format is
%% used, the option 'bufsz' must be set large enough to hold the
%% longest line, including the carriage return character.  The
%% carriage return is stripped from the data packet before the
%% packet is delivered to the application.</p>
%% <p>Same as {packet, {delimited, &lt;&lt;"\r"&gt;&gt;}}.</p>
%%
%% <b>{packet, lf}</b><br />
%% <p>Packets are line oriented, terminated by a single line feed
%% ($\n, ASCII value 10, hex 0A).  If this packet format is
%% used, the option 'bufsz' must be set large enough to hold the
%% longest line, including the line feed character.  The line feed
%% is stripped from the data packet before the packet is delivered
%% to the application.</p>
%% <p>Same as {packet, {delimited, &lt;&lt;"\n"&gt;&gt;}}.</p>
%%
%% <b>{packet, crlf}</b><br />
%% <p>Packets are line oriented, terminated by a carriage return / 
%% line feed pair ("\r\n", ASCII value 13, hex 0D followed by ASCII value
%% 10 hex 0A).  If this packet format is used, the option 'bufsz' must
%% be set large enough to hold the longest line, including the
%% carriage return and line feed characters.  The carriage return and
%% line feed are both stripped from the data packet before the
%% packet is delivered to the application.</p>
%% <p>Same as {packet, {delimited, &lt;&lt;"\r\n"&gt;&gt;}}.</p>
%%
%% <b>{packet, {delimited, Delimiter}}</b><br />
%%	<ul>
%%		<li>Delimiter = {@type binary()}
%%                              when size(Delimiter) =&lt; 8</li>
%%	</ul>
%% <p>Packets are variable length and delimited by a sequence of one
%% or more bytes.  All bytes in Delimiter must occur in order to form
%% a packet boundary.  The Delimiter cannot exceed 8 bytes in length
%% due to internal limitations in the driver.  If this packet format
%% is used, the 'bufsz' option must specify a buffer large enough to
%% hold the largest packet and the complete Delimiter.</p>
%% <p>Delimiter may contain any binary data sequence necessary, as
%% the driver is fully 8 bit clean.</p>
%%
%% <b>{packet, {fixed, Bytes}}</b><br />
%%	<ul>
%%		<li>Bytes = {@type integer()}</li>
%%	</ul>
%% <p>Packets are fixed length in size, with every packet being exactly
%% Bytes number of bytes in length.  The application will not be given a
%% packet until exactly Bytes number of bytes have been received by
%% the serial port.  If this option is used, 'bufsz' may be smaller than
%% Bytes, the driver is smart enough to not fragment the packet.</p>
%%
%% <a name="-options-active"><b>{active, When}</b></a><br />
%%	<ul>
%%		<li>When = {@type false | true | once}</li>
%%	</ul>
%% <p>Just like the active option to <code>ssl</code> or
%% <code>gen_tcp</code>.  When set to 'true' the port owner will receive
%% all data packets as Erlang messages.  If set to 'once' the port owner
%% will receive a single data packet, and the active setting will be
%% set to 'false'.  The 'once' option prevents the port owner from being
%% flooded with data on a fast link.  If 'false', the port owner will
%% not receive any data packets at all, until set to 'true' or 'once'.
%% <b>Currently only active mode is supported!</b>
%% </p>
%%
-module(gen_serial).

%% Public exports.
-export([open/2, close/1, close/2,
	 setopts/2, set_owner/1, set_owner/2,
	 recv/2, recv/3,
	 send/2, asend/2, bsend/2, bsend/3, flush/1, flush/2]).

-export_type([port_ref/0]).

%% Interface server loop.  This is not a public export, but is used
%% to support code reloading.
-export([init/3, loop/4]).

-define(SERIAL_CFG_MAGIC,               <<"gens001", 0:8>>).
-define(SERIAL_PARITY_NONE,		0).
-define(SERIAL_PARITY_ODD,		1).
-define(SERIAL_PARITY_EVEN,		2).

-define(SERIAL_STOPBITS_1,		1).
-define(SERIAL_STOPBITS_2,		2).
-define(SERIAL_STOPBITS_1_5,	       -1).

-define(SERIAL_PACKET_NONE,		0).
-define(SERIAL_PACKET_1,		1).
-define(SERIAL_PACKET_2,		2).
-define(SERIAL_PACKET_4,		4).
-define(SERIAL_PACKET_DELIM,	       $d).

-define(PACKET_ACTIVE,		       $a).
-define(PACKET_CLOSE,		       $q).
-define(PACKET_CONFIG,		       $c).
-define(PACKET_DATA,		       $d).
-define(PACKET_ERROR,		       $e).
-define(PACKET_FLUSH,		       $f).
-define(PACKET_OK,		       $k).
-define(PACKET_OPEN,		       $o).

-define(PORT_ACTIVE_FALSE,		0).
-define(PORT_ACTIVE_TRUE,		1).
-define(PORT_ACTIVE_ONCE,		2).

-record(gen_serial, {
	  pid  :: pid(),
	  port :: port()
}).

-record(serial_cfg, {
	  rcvbuf_size,
	  sndbuf_size,
	  baud_rate,
	  byte_size,
	  parity,
	  stop_bits,
	  use_xonxoff,
	  use_cts,
	  use_dsr,
	  use_dtr,
	  use_rts,
	  packet_format,
	  initially_active,
	  packet_delim,

	  %% The following are not part of the serial_cfg struct, but are
	  %% used within this module to manage the serial port.
	  e_bufsz,
	  e_debug,
	  e_register
}).

-type device_name() :: string() | atom() | integer().
%% <p>
%% The name of a serial port on the host operating system.  On
%% Windows machines this is frequently of the form "COMn" where
%% n is some integer &gt;= 1.  On UNIX systems this may be a
%% tty device, for example "/dev/ttya".
%% </p>
%%
%% <p>
%% Either atoms or strings are allowed, making it easy to spec
%% 'com1' or "COM1".  On UNIX atoms will automatically
%% have "/dev/" prefixed to them, forming a proper device path,
%% however case does matter.  With the prefixing, 'tty0' becomes
%% the full path "/dev/tty0".
%% </p>
%%
%% <p>
%% If an integer is supplied, the ordinal is used with an
%% OS specific prefix to locate the serial port.  This does not
%% work on all platforms, and on some like Windows may not work
%% for all serial ports as not all serial ports start with the
%% "COM" prefix.
%% </p>

-type option() :: atom() | {atom(), term()}.
%% <p>All items in an {@type option_list()} value must be single atoms
%% or name/value pair tuples.  (A standard Erlang property list.) The
%% type of Value and its range of values depends on the specific Name
%% atom paired with it.</p>
%%
%% <p>See <a href="#-options">Available Options</a>.</p>

-type option_list() :: [option()].
%% <p>
%% A list of options to configure the serial port and how Erlang
%% reads and writes data from it.  The option list allows setting
%% baud rate, buffer size, packet formatting, and other options.
%% </p>
%%
%% <p>See <a href="#-options">Available Options</a>.</p>

-opaque port_ref() :: #gen_serial{}.
%% <p>Opaque term returned by {@link open/2} to allow callers to
%% interact with an open serial port. The internals of the term should
%% not be directly accessed or modified by the caller; and the caller
%% should not rely on the term format as it may change in the future
%% without notice.</p>

-type time_in_ms() :: integer().
%% <p>
%% A length of time, expressed as a number of milliseconds.  The
%% special atom <code>infinity</code> is not accepted as a valid
%% value for this type.
%% </p>


%% @doc Open a serial port for communications.
%% <p>
%% When a serial port is opened, the caller is setup as the port
%% owner.  (See {@link set_owner/1}, {@link set_owner/2}.)
%% At open, the port is linked to the owner, ensuring that if the owner
%% terminates, the port will be automatically closed as well.
%% </p>

-spec open(Device, Options) -> {ok, PortRef} | {error, Reason} when
      Device :: device_name(),
      Options :: option_list(),
      PortRef :: port_ref(),
      Reason :: term().

open(Device, Options) when is_integer(Device) ->
    case os:type() of
	{unix, linux} -> open("/dev/tty" ++ integer_to_list(Device), Options);
	{unix, _} -> open("/dev/tty" ++ integer_to_list($a + Device), Options);
	{win32, _} -> open("COM" ++ integer_to_list(Device), Options);
	_ -> open(integer_to_list(Device), Options)
    end;
open(Device, Options) when is_atom(Device) ->
    case os:type() of
	{unix, _} -> open("/dev/" ++ atom_to_list(Device), Options);
	_ -> open(atom_to_list(Device), Options)
    end;
open(Device, Options) ->
    case cfg(Options, cfg_defaults()) of
	Cfg when is_record(Cfg, serial_cfg) -> startup_port(Device, Cfg);
	Other -> Other
    end.


%% @doc Change the current options on the serial port.
%% <p>
%% <i>Currently only the active flag can be changed, but note that
%% only the setting {active, true} is supported at this time.</i>
%% </p>
%% <p>See <a href="#-options-active">{active, When}</a>.</p>

-spec setopts(PortRef, Options) -> ok | {error, Reason} when
      PortRef :: port_ref(),
      Options :: [{active, When}],
      When :: false | true | once,
      Reason :: term().

setopts(#gen_serial{port = Port}, Options=[{active, _}]) ->
	Cfg = cfg(Options, cfg_defaults()),
	Active = Cfg#serial_cfg.initially_active,
	true = port_command(Port, <<?PACKET_ACTIVE:8, Active:8>>),
	ok.


%% @equiv set_owner(PortRef, self())

-spec set_owner(PortRef) -> ok | {error, Reason} when
      PortRef :: port_ref(),
      Reason :: term().

set_owner(PortRef) ->
	set_owner(PortRef, self()).


%% @doc Change the owner of the serial port to another process.
%%
%% <p>
%% The owner of the serial port receives a set of messages, similar
%% to the messages sent by the gen_tcp or ssl modules.  The messages
%% are defined above in <a href="#-messages">Port Owner Messages</a>.
%% </p>
%% <p>
%% The port is always linked to the port owner process.  If the port
%% owner exits, the serial port will automatically close, ensuring
%% resources are freed up automatically.
%% </p>
%%
%% @see set_owner/1

-spec set_owner(PortRef, To) -> ok | {error, Reason} when
      PortRef :: port_ref(),
      To :: pid(),
      Reason :: term().

set_owner(PortRef, To) ->
	call(PortRef, {set_owner, To}, infinity).


%% @equiv close(PortRef, 3000)

-spec close(PortRef) -> ok | killed when
      PortRef :: port_ref().

close(PortRef) ->
	close(PortRef, 3000).


%% @doc Close an open serial port.
%%
%% <p>
%% This call is not always necessary, as the port will automatically
%% close when the port owner terminates.
%% </p>
%% <p>
%% A timeout can be supplied, as this call blocks until it receives
%% confirmation from the serial port process that all pending output
%% has been transferred to the endpoint, and the port has closed down
%% gracefully.
%% </p>
%% <p>
%% If the port's output buffer is full (because the endpoint has stopped
%% receiving data, or flow control has been broken), the close command
%% may not be able to be processed in a timely fashion. In this case,
%% this function will wait for <code>Timeout</code> to expire, and then
%% brutually kill the serial port.  Brutally killing the port will
%% release all resources correctly, but data will be lost when the output
%% buffers are destroyed.  If the brutal kill is required, the atom
%% <code>killed</code> is returned instead of <code>ok</code>.  The
%% brutal kill version of this function will not cause the port owner
%% process to crash, as the exit reason used is <code>normal</code>.
%% </p>
%% <p>
%% Special note: If the caller attempts to use the atom 'infinity' as
%% the Timeout value, it will be silently converted to 60 seconds to
%% prevent locking up the caller indefinitely.
%% </p>

-spec close(PortRef, Timeout) -> ok | killed when
      PortRef :: port_ref(),
      Timeout :: time_in_ms().

close(PortRef, infinity) ->
    close(PortRef, 60 * 1000);
close(PortRef, Timeout) ->
    case call(PortRef, {close, Timeout}, Timeout) of
	ok ->
	    ok;
	{error, timeout} ->
	    case PortRef of
		_ when is_atom(PortRef) -> exit(whereis(PortRef), normal);
		_ when is_pid(PortRef) -> exit(PortRef, normal);
		#gen_serial{pid = Pid} -> exit(Pid, normal)
	    end,
	    killed
    end.


%% @equiv recv(PortRef, Length, infinity)

-spec recv(PortRef, Length) -> {ok, Packet} | {error, Reason} when
      PortRef :: port_ref(),
      Length :: integer(),
      Packet :: binary(),
      Reason :: term().

recv(PortRef, Len) ->
    recv(PortRef, Len, infinity).


%% @doc Read data from an open serial port.
%%
%% <p>
%% Reads exactly <code>Length</code> bytes from the serial port and
%% returns them as a single binary object.  If the port has less than
%% <code>Length</code> bytes immediately available in the receive
%% buffers, this call will block until the timeout expires or the
%% total number of bytes requested has been received.
%% </p>
%% <p>
%% If the caller doesn't want to block while waiting for data, the
%% caller should either use a short timeout, or use an active mode
%% port instead of using {@link recv/2} or {@link recv/3}.
%% </p>
%% <p>
%% <i>Note: currently only active mode is supported and thus
%% recv/2 and recv/3 are not implemented by the backend driver.
%% Do not use them!</i>
%% </p>

-spec recv(PortRef, Length, Timeout) -> {ok, Packet} | {error, Reason} when
      PortRef :: port_ref(),
      Length :: integer(),
      Timeout :: infinity | time_in_ms(),
      Packet :: binary(),
      Reason :: term().

recv(PortRef, Len, Timeout) ->
    call(PortRef, {recv, Len}, Timeout).


%% @doc Partially asynchronous data transmission.
%%
%% <p>
%% Sends data through the serial port.  The caller sends the data
%% out the port directly, which means the caller may block indefinitely
%% if all IO buffers are full and flow control has broken down.  This
%% is the fastest way to send data to the serial port, as it does not
%% have to pass through the interface process first, but may be risky
%% due to the flow control issues.
%% </p>
%% <p>
%% When this call returns, the data may only be queued for delivery.
%% There are no guarantees that the data was actually transmitted
%% out the serial port.  Use {@link flush/1}, {@link flush/2} or
%% {@link bsend/2} to wait for the data to have actually been sent out
%% the serial port to the endpoint.
%% </p>
%% <p>
%% If the caller wants true nonblocking sends, see {@link asend/2}.
%% </p>
%% 
%% @see asend/2
%% @see bsend/2
%% @see bsend/3

-spec send(PortRef, Packet) -> ok when
      PortRef :: port_ref(),
      Packet :: binary() | [binary()].

send(#gen_serial{port = Port}, Data) ->
    true = port_command(Port, [<<?PACKET_DATA:8>> | Data]),
    ok.


%% @doc Asynchronous data transmission.
%%
%% <p>
%% Sends data through the serial port.  The data is first sent to the
%% interface process, which means the caller should never block when
%% this method is called, even if flow control has broken down and
%% all IO buffers are full (as the interface process' message queue
%% is only limited by the memory ERTS can obtain from the OS).
%% </p>
%% <p>
%% When this call returns, the data may only be queued for delivery.
%% There are no guarantees that the data was actually transmitted
%% out the serial port.  Use {@link flush/1}, {@link flush/2} or
%% {@link bsend/2} to wait for the data to have actually been sent out
%% the serial port to the endpoint.
%% </p>
%%
%% @see send/2
%% @see bsend/2
%% @see bsend/3

-spec asend(PortRef, Packet) -> ok when
      PortRef :: port_ref(),
      Packet :: binary() | [binary()].

asend(#gen_serial{pid = Pid}, Data) ->
    Pid ! {send, Data},
    ok.


%% @equiv bsend(PortRef, Packet, infinity)

-spec bsend(PortRef, Packet) -> ok | {error, Reason} when
      PortRef :: port_ref(),
      Packet :: binary() | [binary()],
      Reason :: term().

bsend(PortRef, Data) ->
    bsend(PortRef, Data, infinity).


%% @doc Synchronous data transmission.
%%
%% <p>
%% Sends data through the serial port.  Unlike all other forms of the
%% send call, {@link bsend/2} and {@link bsend/3} wait until the
%% endpoint has received the data before returning to the caller.
%% This may take some time, depending on the speed of the serial port
%% and how much data is already queued up in the output queues.
%% </p>
%% <p>
%% Callers are encouraged to use this form rather than {@link
%% bsend/2}, as it allows specification of a timeout that prevents
%% hanging in case flow control has broken down and the data already
%% buffered cannot be sent.
%% </p>
%% <p>
%% This function is equivalent to (but easier to use than):
%% </p>
%% <pre>
%%	case send(PortRef, Packet) of
%%	    ok -> flush(PortRef, Timeout);
%%	    Other -> Other
%%	end
%% </pre>
%%
%% @see send/2
%% @see asend/2
%% @see bsend/2

-spec bsend(PortRef, Packet, Timeout) ->
		   ok | {error, timeout} | {error, Reason} when
      PortRef :: port_ref(),
      Packet :: binary() | [binary()],
      Timeout :: infinity | time_in_ms(),
      Reason :: term().

bsend(PortRef, Data, Timeout) ->
    case send(PortRef, Data) of
	ok -> flush(PortRef, Timeout);
	Other -> Other
    end.


%% @equiv flush(PortRef, infinity)

-spec flush(PortRef) -> ok | {error, Reason} when
      PortRef :: port_ref(),
      Reason :: term().

flush(PortRef) ->
    flush(PortRef, infinity).


%% @doc Wait until buffered data has been transmitted to the endpoint.
%%
%% <p>
%% Waits until the port's outgoing data buffers have been fully drained
%% and transmitted to the endpoint.  If any error is detected during
%% transmission (while waiting for the data to finish being sent), the
%% error will both be returned by this function and sent to the port
%% owner as a message unless the caller is the port owner, in which
%% case the error is returned and no message is sent.
%% </p>

-spec flush(PortRef, Timeout) ->
		   ok | {error, timeout} | {error, Reason} when
      PortRef :: port_ref(),
      Timeout :: infinity | time_in_ms(),
      Reason :: term().

flush(PortRef, Timeout) ->
    call(PortRef, {flush, Timeout}, Timeout).


call(Name, Call, Timeout) when is_atom(Name) ->
    call(whereis(Name), Call, Timeout);
call(#gen_serial{pid = Pid}, Call, Timeout) ->
    call(Pid, Call, Timeout);
call(Pid, Call, Timeout) when is_pid(Pid) ->
    Pid ! {'$call', self(), Call},
    receive
	{'$reply', Pid, Any} ->	Any
    after Timeout -> {error, timeout}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

startup_port(Device, Cfg) ->
    Pid = spawn_link(?MODULE, init, [self(), Device, Cfg]),
    receive
	{ok, Pid, R} ->	{ok, R};
	{'EXIT', Pid, R} -> {error, R}
    end.

%% @hidden
%% @doc <b>This is an internal function: do not use.</b>
%% Sets up a newly spawned interface process.
%%
%% <p>
%% Sets up a newly spawned process to act as the interface between the
%% Erlang port driver and the rest of the Erlang environment.  The
%% interface process owns the actual port object, and keeps track of
%% the externally running port process which performs the low level
%% operating system calls.
%% </p>
%%
%% @see loop/4
init(Owner, Device, Cfg) ->
    case open_ioport(Device, Cfg) of
	{ok, Port} ->
	    case configure_ioport(Port, Cfg) of
		ok ->
		    case register_myself(Device, Cfg) of
			{ok, Me} ->
			    PortRef = #gen_serial{port = Port, pid = Me},
			    Owner ! {ok, self(), PortRef},
			    loop(Device, PortRef, Owner, Port);
			
			{error, Reason} ->
			    Owner ! {'EXIT', self(), Reason},
			    exit(normal)
		    end;
		
		{error, Reason} ->
		    Owner ! {'EXIT', self(), Reason},
		    exit(normal)
	    end;
	
	{error, Reason} ->
	    Owner ! {'EXIT', self(), Reason},
	    exit(normal)
    end.

open_ioport(Device, Cfg) ->
    Cmd = {spawn, command_line(Device, Cfg)},
    Opt = [{packet, 4}, use_stdio, in, out, binary, exit_status],
    case catch open_port(Cmd, Opt) of
	Port when is_port(Port) ->
	    receive
		{Port, {data, <<?PACKET_OK:8, ?PACKET_OPEN:8>>}} ->
		    {ok, Port};
		{Port, {data, <<?PACKET_ERROR:8, C:32/native, M/binary>>}} ->
		    {error, translate_error(C, binary_to_list(M))};
		{Port, {exit_status, Status}} ->
		    {error, {exit, translate_exit_status(Status)}}
	    after 5000 ->
		    {error, timeout}
	    end;
	
	{'EXIT', Reason} ->
	    {error, Reason}
    end.

configure_ioport(Port, Cfg) ->
    Struct = cfg2bin(Cfg),
    case catch port_command(Port, [<<?PACKET_CONFIG:8>>, Struct]) of
	true ->
	    receive
		{Port, {data, <<?PACKET_OK:8, ?PACKET_CONFIG:8>>}} ->
		    ok;
		{Port, {data, <<?PACKET_ERROR:8, C:32/native, M/binary>>}} ->
		    {error, translate_error(C, binary_to_list(M))};
		{Port, {exit_status, Status}} ->
		    {error, {exit, translate_exit_status(Status)}}
	    after 5000 ->
		    {error, timeout}
	    end;
	
	{'EXIT', Reason} ->
	    {error, Reason}
    end.

register_myself(_Device, #serial_cfg{e_register = false}) ->
    {ok, self()};
register_myself(Device, Cfg=#serial_cfg{e_register = true}) ->
    Name = lists:map(fun(C) when C >= $A, C =< $Z -> C + ($a - $A);
			(C) -> C
		     end,
		     lists:filter(fun(C) when C >= $a, C =< $z -> true;
				     (C) when C >= $A, C =< $Z -> true;
				     (C) when C >= $0, C =< $9 -> true;
				     ($_) -> true;
				     (_) -> false
				  end,
				  Device)),
    register_myself(Device, Cfg#serial_cfg{e_register = list_to_atom(Name)});
register_myself(_Device, #serial_cfg{e_register = Name}) ->
    case catch register(Name, self()) of
	true ->	{ok, Name};
	{'EXIT', Reason} -> {error, Reason}
    end.

%% @hidden
%% @doc <b>This is an internal function: do not use.</b>
%% Main loop of an interface process.
%%
%% @see init/3
loop(DeviceName, PortRef, Owner, Port) ->
    receive
	{Port, {data, <<?PACKET_DATA:8, Packet/binary>>}} ->
	    Owner ! {serial, PortRef, Packet},
	    ?MODULE:loop(DeviceName, PortRef, Owner, Port);
	
	{Port, {data, <<?PACKET_ERROR:8, ECode:32/native, EMsg/binary>>}} ->
	    error_logger:error_report([{pid, self()},
				       {owner, Owner},
				       {module, ?MODULE},
				       {port, Port},
				       {device, DeviceName},
				       {error, ECode},
				       {message, EMsg}]),
	    Owner ! {serial_error, PortRef,
		     translate_error(ECode, binary_to_list(EMsg))},
	    ?MODULE:loop(DeviceName, PortRef, Owner, Port);

	{Port, {data, <<?PACKET_OK:8, _PacketType:8>>}} ->
	    % Unexpected OK message.  Most likely a reply to a request
	    % we sent earlier (like PACKET_FLUSH) but that was wedged in
	    % a buffer longer than we expected.  Just ignore it.
	    ?MODULE:loop(DeviceName, PortRef, Owner, Port);

	{Port, {exit_status, Status}} ->
	    % The port program has shutdown, but we did not expect it to
	    % exit at this time.  Report the error to both the error logger
	    % and the parent, and stop this process.
	    Reason = translate_exit_status(Status),
	    error_logger:error_report([{pid, self()},
				       {owner, Owner},
				       {module, ?MODULE},
				       {port, Port},
				       {device, DeviceName},
				       {exit, Reason}]),
	    Owner ! {serial_closed, PortRef},
	    exit(normal);

	{send, Data} ->
	    port_command(Port, [<<?PACKET_DATA:8>> | Data]),
	    ?MODULE:loop(DeviceName, PortRef, Owner, Port);

	{'$call', _From, {recv, _Len}} ->
	    % FIXME
	    ?MODULE:loop(DeviceName, PortRef, Owner, Port);

	{'$call', From, {set_owner, Who}} ->
	    case catch link(Who) of
		true ->
		    unlink(Owner),
		    From ! {'$reply', self(), ok},
		    ?MODULE:loop(DeviceName, PortRef, Who, Port);
		
		{'EXIT', Reason} ->
		    From ! {'$reply', self(), {error, Reason}},
		    ?MODULE:loop(DeviceName, PortRef, Owner, Port)
	    end;

	{'$call', From, {flush, Timeout}} ->
	    % The caller wants us to make sure output has been transmitted.
	    % Send a flush packet to the driver, and wait for an OK reply
	    % which indicates the port has transmitted all prior data.
	    % If a timeout occurs, don't send anything to the caller, as
	    % the caller will have already broken out of his own timeout
	    % handler.
	    port_command(Port, <<?PACKET_FLUSH:8>>),
	    receive
		{Port, {data, <<?PACKET_OK:8, ?PACKET_FLUSH:8>>}} ->
		    From ! {'$reply', self(), ok};
		{Port, {data, <<?PACKET_ERROR:8, C:32/native, M/binary>>}} ->
		    Reason = translate_error(C, binary_to_list(M)),
		    From ! {'$reply', self(), {error, Reason}},
		    if From /= Owner ->
			    Owner ! {serial_error, PortRef, Reason}
		    end
	    after Timeout ->
		    ok
	    end,
	    ?MODULE:loop(DeviceName, PortRef, Owner, Port);

	{'$call', From, {close, Timeout}} ->
	    % Gracefully close the port.  This basically amounts to sending
	    % a close packet to the port, and waiting for the OK reply.
	    % If the OK comes back, the output buffers were drained fully
	    % before the close was done.
	    port_command(Port, <<?PACKET_CLOSE:8>>),
	    receive
		{Port, {data, <<?PACKET_OK:8, ?PACKET_CLOSE:8>>}} ->
		    From ! {'$reply', self(), ok}
	    after Timeout -> ok
	    end,
	    exit(normal);

	{'EXIT', Owner, Reason} ->
	    % The parent has crashed, and for some reason this process
	    % has trap_exit true.  Make sure we handle that and cleanup
	    % properly.
	    exit(Reason);

	Any ->
	    error_logger:error_report([{pid, self()},
				       {owner, Owner},
				       {module, ?MODULE},
				       {port, Port},
				       {device, DeviceName},
				       {unexpected_message, Any}]),
	    ?MODULE:loop(DeviceName, PortRef, Owner, Port)
    end.

command_line(Device, #serial_cfg{e_bufsz=BufSz, e_debug=""}) ->
    lists:flatten([port_program_path(Device),
		   " ", Device,
		   " ", integer_to_list(BufSz)]);
command_line(Device, #serial_cfg{e_bufsz=BufSz, e_debug=Dbg}) ->
    lists:flatten([port_program_path(Device),
		   " ", Device,
		   " ", integer_to_list(BufSz),
		   " ", Dbg]).

port_program_path(Device) ->
    filename:absname(filename:join(priv_bin_dir(),
				   port_program_filename(Device))).

port_program_filename(Device) ->
    case [os:type() | os:version()] of
	[{win32, _} | _] -> clone_esock("serial_esock.exe", Device ++ "_esock.exe");
	[{unix, _} | _] -> "serial_esock";
	Other -> exit({not_supported, Other})
	end.

clone_esock(Src, Target) ->
    SPath = filename:join(priv_bin_dir(), Src),
    TPath = filename:join(priv_bin_dir(), Target),
    case catch file:copy(SPath, TPath) of
	{ok, _} -> Target;
	_ -> Src
    end.

priv_bin_dir() ->
    case code:priv_dir(?MODULE) of
	Dir when is_list(Dir) -> filename:join(Dir, "bin");
	{error, Reason} -> exit(Reason)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

translate_exit_status(Status) ->
    case os:type() of
	{win32, _} ->
	    case Status of
		1 -> killed;
		_ -> Status
	    end;
	
	{unix, _} ->
	    case Status of
		Normal when Normal < 128 -> {exit, Normal};
		Signal when Signal >= 128 -> {signal, Signal - 128}
	    end;
	
	_ -> Status
    end.

translate_error(Code, Msg) ->
    case os:type() of
	{win32, _} -> translate_win32_error(Code, Msg);
	{unix, _} -> translate_unix_error(Code, Msg);
	_ -> {Code, Msg}
    end.

translate_win32_error(2, _) -> enodev;   % ERROR_FILE_NOT_FOUND
translate_win32_error(87, _) -> einval;  % ERROR_INVALID_PARAMETER
translate_win32_error(Code, Msg) -> {Code, Msg}.

translate_unix_error(Code, Msg) -> {Code, Msg}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cfg_defaults() ->
    #serial_cfg{
	      rcvbuf_size = 4096,
	      sndbuf_size = 4096,
	      baud_rate = 9600,
	      byte_size = 8,
	      parity = ?SERIAL_PARITY_NONE,
	      stop_bits = ?SERIAL_STOPBITS_1,
	      use_xonxoff = 0,
	      use_cts = 1,
	      use_dsr = 1,
	      use_dtr = 1,
	      use_rts = 1,
	      packet_format = ?SERIAL_PACKET_NONE,
	      initially_active = ?PORT_ACTIVE_TRUE,
	      packet_delim = <<>>,
	      e_bufsz = 8192,
	      e_debug = "",
	      e_register = false
	     }.

cfg([{rcvbuf, V} | T], C) when is_integer(V), V >= 32, V =< 32768 ->
    cfg(T, C#serial_cfg{rcvbuf_size = V});

cfg([{sndbuf, V} | T], C) when is_integer(V), V >= 32, V =< 32768 ->
    cfg(T, C#serial_cfg{sndbuf_size = V});

cfg([{bufsz, V} | T], C) when is_integer(V), V >= 128 ->
    cfg(T, C#serial_cfg{e_bufsz = V});

cfg([{debug_log, V} | T], C) when is_list(V), V /= "" ->
    cfg(T, C#serial_cfg{e_debug = V});

cfg([register | T], C) ->
    cfg(T, C#serial_cfg{e_register = true});

cfg([{register, Name} | T], C) when is_atom(Name) ->
    cfg(T, C#serial_cfg{e_register = Name});

cfg([{baud, V} | T], C) when is_integer(V), V >= 1 ->
    cfg(T, C#serial_cfg{baud_rate = V});

cfg([{baudrate, V} | T], C) when is_integer(V), V >= 1 ->
    cfg(T, C#serial_cfg{baud_rate = V});

cfg([{bytesz, V} | T], C) when is_integer(V), V >= 5, V =< 8 ->
    cfg(T, C#serial_cfg{byte_size = V});

cfg([{parity, none} | T], C) ->
    cfg(T, C#serial_cfg{parity = ?SERIAL_PARITY_NONE});

cfg([{parity, odd} | T], C) ->
    cfg(T, C#serial_cfg{parity = ?SERIAL_PARITY_ODD});

cfg([{parity, even} | T], C) ->
    cfg(T, C#serial_cfg{parity = ?SERIAL_PARITY_EVEN});

cfg([{stop_bits, 1} | T], C) ->
    cfg(T, C#serial_cfg{stop_bits = ?SERIAL_STOPBITS_1});

cfg([{stop_bits, 1.5} | T], C) ->
    cfg(T, C#serial_cfg{stop_bits = ?SERIAL_STOPBITS_1_5});

cfg([{stop_bits, 2} | T], C) ->
    cfg(T, C#serial_cfg{stop_bits = ?SERIAL_STOPBITS_2});

cfg([{flow_control, none} | T], C) ->
    cfg(T, C#serial_cfg{
	     use_xonxoff = 0,
	     use_cts = 0,
	     use_dsr = 0,
	     use_dtr = 0,
	     use_rts = 0
	    });

cfg([{flow_control, software} | T], C) ->
    cfg(T, C#serial_cfg{
	     use_xonxoff = 1,
	     use_cts = 0,
	     use_dsr = 0,
	     use_dtr = 0,
	     use_rts = 0
	    });

cfg([{flow_control, hardware} | T], C) ->
    cfg(T, C#serial_cfg{
	     use_xonxoff = 0,
	     use_cts = 1,
	     use_dsr = 1,
	     use_dtr = 1,
	     use_rts = 1
	    });

cfg([{packet, none} | T], C) ->
    cfg(T, C#serial_cfg{packet_format = ?SERIAL_PACKET_NONE});

cfg([{packet, 1} | T], C) ->
    cfg(T, C#serial_cfg{packet_format = ?SERIAL_PACKET_1});

cfg([{packet, 2} | T], C) ->
    cfg(T, C#serial_cfg{packet_format = ?SERIAL_PACKET_2});

cfg([{packet, 4} | T], C) ->
    cfg(T, C#serial_cfg{packet_format = ?SERIAL_PACKET_4});

cfg([{packet, {fixed, S}} | T], C) when is_integer(S), S >= 1 ->
    cfg(T, C#serial_cfg{ packet_format = -S });

cfg([{packet, {delimited, B}} | T], C) when is_binary(B), size(B) =< 8 ->
    cfg(T, C#serial_cfg{
	     packet_format = ?SERIAL_PACKET_DELIM,
	     packet_delim = B
	    });

cfg([{packet, lf} | T], C) ->
    cfg(T, C#serial_cfg{
	     packet_format = ?SERIAL_PACKET_DELIM,
	     packet_delim = <<"\n">>
	    });

cfg([{packet, cr} | T], C) ->
    cfg(T, C#serial_cfg{
	     packet_format = ?SERIAL_PACKET_DELIM,
	     packet_delim = <<"\r">>
	    });

cfg([{packet, crlf} | T], C) ->
    cfg(T, C#serial_cfg{
	     packet_format = ?SERIAL_PACKET_DELIM,
	     packet_delim = <<"\r\n">>
	    });

cfg([{active, false} | T], C) ->
    cfg(T, C#serial_cfg{initially_active = ?PORT_ACTIVE_FALSE});

cfg([{active, true} | T], C) ->
    cfg(T, C#serial_cfg{initially_active = ?PORT_ACTIVE_TRUE});

cfg([{active, once} | T], C) ->
    cfg(T, C#serial_cfg{initially_active = ?PORT_ACTIVE_ONCE});

cfg([], C) -> C;

cfg([X | _], _) -> {error, {badarg, X}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cfg2bin(#serial_cfg{
	   rcvbuf_size = RcvBufSize,
	   sndbuf_size = SndBufSize,
	   baud_rate = BaudRate,
	   byte_size = ByteSize,
	   parity = Parity,
	   stop_bits = StopBits,
	   use_xonxoff = UseXonXoff,
	   use_cts = UseCts,
	   use_dsr = UseDsr,
	   use_dtr = UseDtr,
	   use_rts = UseRts,
	   packet_format = PacketFormat,
	   initially_active = InitiallyActive,
	   packet_delim = PacketDelim}) ->

    PacketDelimLen = size(PacketDelim),
    PadLen = (8 - PacketDelimLen) * 8,
    <<?SERIAL_CFG_MAGIC/binary,
      RcvBufSize:32/native,
      SndBufSize:32/native,
      BaudRate:32/native,
      ByteSize:8/native,
      Parity:8/native,
      StopBits:8/native,
      UseXonXoff:8/native,
      UseCts:8/native,
      UseDsr:8/native,
      UseDtr:8/native,
      UseRts:8/native,
      PacketFormat:16/native,
      InitiallyActive:8/native,
      PacketDelimLen:8/native,
      PacketDelim/binary,
      0:PadLen/native>>.
