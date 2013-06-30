var http = require('http');
var net = require('net');
//var fs = require('fs');

var response;
var connection;
var file;

var tcpServer = net.createServer(function(socket) {
  console.log('client connected');
  connection = socket;
  
  socket.on('data', function(data) {
    //file.write(data);
    response.write(data, 'base64');
  });
}).listen(8888, function() { console.log('listenting'); });

var httpServer = http.createServer(function(req, res) {
  res.writeHead(200, {
    'Content-Type': 'audio/mpeg',
    'Transfer-Encoding': 'chunked'
  });
  response = res;
  //file = fs.createWriteStream('c:/test2.mp3');
  console.log('requesting song');
  connection.write('get my song!');
}).listen(8080, 'localhost');