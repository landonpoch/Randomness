var net = require('net');
var fs = require('fs');

var path = '/home/landon/Music/Regular/Assorted/WITJ_Metallica.mp3';

var socket = net.createConnection(8888, 'sickbastard', function() {
  console.log('connected');
});

socket.on('data', function(data) {
  console.log('received command');
  var stream = fs.createReadStream(path);
  stream.on('open', function() {
    stream.pipe(socket);
  });
//  stream.pipe(socket);
//  stream.on('readable', function() {
//    var buf;
//    while (buf = stream.read()) {
//      console.log(buf);
//      socket.write(buf);
//    }
//  });
//  stream.once('end', function() {
//    
//  });
});
