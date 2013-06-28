var net = require('net');

var socket = net.connect({
    port: 8080,
    host: 'localhost'
  },
  function () { 
    console.log('client connected');
    var command = { name: 'register' };
    command.data = { id: 'client1' };
    socket.write(JSON.stringify(command));
  }
);

socket.on('data', function (data) {
  var message = data.toString();
  var command = { name: 'respond' };
  command.data = { response: 'world!' };
  if (message == 'hello') {
    socket.write(JSON.stringify(command));
  }
});

socket.on('end', function () {
    console.log('client disconnected');
});