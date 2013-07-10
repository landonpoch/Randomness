var stream = require('stream');
var fs = require('fs');
var path = require('path');

var fileName = path.join(__dirname, 'lowercase.txt');
var fileStream = fs.createReadStream(fileName, {highWaterMark: 31});

fileStream.on('readable', function() {  
  var cap = new stream.Transform({decodeStrings: false});
  cap._transform = function(chunk, encoding, callback) {
    callback(null, chunk.toString().toUpperCase());
  };
  cap.on('readable', function() {
    process.stdout.write(cap.read());
  });
  setTimeout(function() {
    cap.write(fileStream.read() + '|');
  }, 1000);
});

fileStream.on('error', function(err) {
  console.log('error: ' + err);
});

fileStream.on('end', function() {
  console.log('finished');
});
