var turtl = require('turtl');

(function() {
	turtl.set_msg_callback(function(msg) {
		var ev	=	JSON.parse(msg);
		console.log('js: recv: ', ev);
	});

	turtl.start();

	var send_ping	=	function()
	{
		var msg	=	{ev: 'ping'};
		turtl.send_msg_lisp(JSON.stringify(msg));
	};
	setTimeout(send_ping, 100);
})();
