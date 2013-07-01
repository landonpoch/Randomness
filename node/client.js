var net = require('net');
var fs = require('fs');

var path = 'c:/test.mp3';

var socket = net.createConnection(8888, 'sickbastard', function() {
    console.log('connected');
});

socket.on('data', function(data) {
  console.log('received command');
  var stream = fs.createReadStream(path);
  stream.on('open', function() {
    stream.pipe(socket, {end: false});
  });
  stream.on('end', function() {
    console.log('finished sending song to server');
  });
});

socket.on('end', function () {
  console.log('disconnected');
});