var stream = require('stream');
var fs = require('fs');
var path = require('path');

var fileName = path.join(__dirname, 'lowercase.txt');
var fileStream = fs.createReadStream(fileName);
fileStream.on('readable', function() {
  var cap = new stream.Transform({decodeStrings: false});
  cap._transform = function(chunk, encoding, callback) {
    callback(null, chunk.toString().toUpperCase());
  };
  cap.on('readable', function() {
    process.stdout.write(cap.read(10));
  });
  cap.write(fileStream.read(10));
});

fileStream.on('error', function(err) {
  console.log('error: ' + err);
});

fileStream.on('end', function() {
  console.log('finished');
});