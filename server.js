const Bundler = require('parcel-bundler');
const express = require('express');
const https = require('https');

let bundler = new Bundler('index.html');
let app = express();

app.get('/access_token', (req, res) => {
  const data = JSON.stringify({
    client_id: process.env.CLIENT_ID,
    client_secret: process.env.CLIENT_SECRET,
    code: req.query.code,
  });

  const options = {
    hostname: 'github.com',
    path: '/login/oauth/access_token',
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Accept': 'application/json',
    },
  };

  const request = https.request(options, result => {
    result.on('data', d => {
      tokens = JSON.parse(d);
      res.send(tokens);
    });
  });

  request.on('error', error => {
    console.log(error);
  });

  request.write(data);
  request.end();
});
app.use(bundler.middleware());

app.listen(Number(process.env.PORT || 2345));
