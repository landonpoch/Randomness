var http = require('http'),
    net = require('net');

var response;
var connection;

var tcpServer = net.createServer(function(socket) {
  console.log('client connected');
  connection = socket;
  socket.on('readable', function() {
    var buffer = socket.read();
    if (buffer) response.write(buffer, 'base64');
  });
  socket.on('end', function() {
    response.end('');
    console.log('finished sending song to browser');
  });
}).listen(8888, function() { 
  console.log('TCP server listening');
});

var httpServer = http.createServer(function(req, res) {
  res.writeHead(200, {
    'Content-Type': 'audio/mpeg',
    'Content-Length': '3612971',
    'Transfer-Encoding': 'chunked'
  });
  response = res;
  console.log('requesting song');
  if (connection) connection.write('get my song!');
}).listen(8080, function() {
  console.log('HTTP server listening');
});