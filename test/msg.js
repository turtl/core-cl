var TurtlEvent	=	require('../TurtlEvent');

var test_bind	=	function()
{
	var Event	=	new TurtlEvent.Event();
	var Remote	=	new TurtlEvent.Remote();

	// pipe remote events into local trigger
	Remote.bind(Event.trigger);

	var send_ping	=	function()
	{
		Remote.send({ev: 'ping'});
	};

	Event.bind('pong', function(event) {
		setTimeout(send_ping, 4000);
	});

	TurtlEvent.start();

	setTimeout(send_ping, 100);
};

var test_response	=	function()
{
	var Event	=	new TurtlEvent.Event();
	var Remote	=	new TurtlEvent.Remote();

	// pipe remote events into local trigger
	Remote.bind(Event.trigger);

	var send_ping	=	function()
	{
		Remote.send({ev: 'ping'}, function(event) {
			console.log('js: got response: ', event);
			setTimeout(send_ping, 4000);
		});
	};

	TurtlEvent.start();

	setTimeout(send_ping, 100);
};

test_response();

