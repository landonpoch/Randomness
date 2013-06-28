var http = require('http');
var net = require('net');

var connections = {};
var responses = {};

var httpServer = http.createServer(function (req, res) {
  responses['client1'] = res; // Set the response for this client
  connections['client1'].write('hello'); // Send the command for this client
}).listen(1337, '127.0.0.1');

var tcpServer = net.createServer({ allowHalfOpen: false }, function (socket) {
  console.log('client connected');
  socket.on('data', function (data) {
    var command = JSON.parse(data.toString());
    if (command.name == 'register') {
      register(socket, command.data);
    }
    if (command.name == 'respond') {
      var res = responses['client1']; // Get the response for this client
      var response = command.data.response;
      res.writeHead(200, {'Content/Type': 'text/plain'});
      res.end(response);
    }
  });
  socket.on('end', function () {
    console.log('client disconnected');
  });
}).listen(8080, '127.0.0.1', function () {
  console.log('server bound');
});

var register = function (socket, data) {
  var id = data.id;
  connections[id] = socket;
  var message = 'Registered client as ' + id;
  console.log(message);
  socket.write(message);
};