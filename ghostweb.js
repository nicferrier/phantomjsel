/*
  ghostweb: phantomjs controlling server.
  Copyright (C) 2012 by Nic Ferrier

  This implements a phantom js module that starts a webserver on
  whatever you specify as the first arg:

    phantomjs ghostweb.js 8081

  the server accepts commands using a verb/noun HTTP header
  system; three commands (verbs) are supported: Open, Call and Exit.

  To open a url inside the page in phantomjs send a GET request to the
  server with these headers:

     Command: Open
     CommandArg: http://example.com

  for example, from curl:

    curl -i -H 'command: open' \
            -H 'commandarg: http://localhost:8005/talk/stuff/html/index.html' \
            http://localhost:8080/

  will open the page: http://localhost:8005/talk/stuff/html/index.html
  (presuming the ghostweb server is listening on port 8080)

  Urls which fail to load will cause the ghostweb to return a 400.


  To call a Javascript function inside the page in phantomjs send a
  GET request to the server with these headers:

     Command: Call
     CommandArg: setTimeout(function () { console.log("blah!"); }, 2000)

  the whole string is taken to be a Javascript expression that can be
  called.

  Any command causing an exception will cause a 400 and spit out some
  details about the exception:

    curl -i -H 'command: call' \
            -H 'commandarg: (function(){throw new Error("hello");})()' \
            http://localhost:8080/HTTP/1.1 

  produces:

    400 Bad Request
    Error:hello

  Finally, to exit phantomjs send "exit":

    curl -i -H 'command: exit' \
            http://localhost:8080/HTTP/1.1 

  the server responds before quitting:

    200 Ok

*/
try {
  var system = require('system');
  var page = require('webpage').create();

  page.onConsoleMessage = function(msg) {
    if (msg == "__quit__") {
      console.log("all done");
      phantom.exit();
    }
    else {
      console.log("page: " + msg);
    }
  };

  var server = require('webserver').create();
  // Start the server on whatever we were told to listen to.
  var service = server.listen(
    system.args[1], function (request, response) {
    if (request.headers.command == "open") {
      page.open(request.headers.commandarg, function (status) {
        if (status !== "success") {
          console.log("server: open " + request.headers.commandarg + " bad");
          response.statusCode = 400;
          response.write("Bad\n");
          response.close();
        }
        else {
          console.log("server: open " + request.headers.commandarg + " ok");
          response.statusCode = 200;
          response.write("Ok\n");
          response.close();
        }
      });
    }
    else if (request.headers.command == "call") {
      var f = Function(
        "try { return " 
          + request.headers.commandarg 
        // we have to do a lot of wierd work to get errors to come out from evaluate
          + "} catch (e) { return {'type': 'error', 'name': e.name, 'message': e.message }; }"
      );
      var retval = page.evaluate(f);

      if (retval["type"] == 'error') {
        response.statusCode = 400;
        response.write(retval.name + ":" + retval.message + "\n");
      }
      else {
        response.statusCode = 200;
        response.write(retval + "\n");
      }
      response.close();
    }
    else if (request.headers.command == "exit") {
      response.statusCode = 200;
      response.write("Ok\n");
      response.close();
      phantom.exit();
    }
    else {
      console.log("server: unknown request");
      response.statusCode = 404;
      response.write("Unknown\n");
      response.close();
    }
  });
}
catch (e) {
  console.log(e);
  phantom.exit();
}
