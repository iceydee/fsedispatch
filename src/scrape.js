var page = require('webpage').create();
var fs = require('fs');
var system = require('system');
var env = system.env;

var filePrefix = env['PREFIX'];

phantom.cookiesEnabled = true;
emptyStep = function() {
  console.log("empty next step");
};

step = emptyStep;
nextStep = emptyStep;
nextStepTrigger = 'notrigger';

var evaluate = function(func) {
  var args = [].slice.call(arguments, 1);
  var fn = "function() { return (" + func.toString() + ").apply(this, " + JSON.stringify(args) + ");}";
  return page.includeJs("http://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.min.js", function() {
    page.evaluate(fn);
  });
}

var save = function(name) {
  fs.write('./data/' + filePrefix + '-' + name.toLowerCase() + '.html', page.content, 'w');
}

page.onResourceRequested = function(requestData, networkRequest) {
  if (requestData.url.indexOf("server.fseconomy.net") >= 0) {
    // console.log('Request (#' + requestData.id + '): ' + requestData.url);
  }
};
page.onUrlChanged = function(targetUrl) {
  if (targetUrl.indexOf("server.fseconomy.net") >= 0) {
    console.log('New URL: ' + targetUrl);
  }
};
page.onLoadFinished = function(status) {
  console.log('Load Finished: ' + status);
  step();
  step = emptyStep;
};
page.onLoadStarted = function() {
  console.log('Load Started');
};
page.onNavigationRequested = function(url, type, willNavigate, main) {
  if (url.indexOf('server.fseconomy.net') >= 0) {
    console.log('Trying to navigate to: ' + url);
    if (url.indexOf(nextStepTrigger) >= 0) {
      step = nextStep;
    }
  }
};
page.onConsoleMessage = function(msg) {
  console.log(msg);
};

step = function() {
  console.log("--- Login");
  nextStep = saveLoggedInPage;
  nextStepTrigger = 'index.jsp';

  evaluate(function(user, pass) {
    $('input[name="user"]').val(user);
    $('input[name="password"]').val(pass);
    $('input[name="event"]').click();
  }, env['FSE_USER'], env['FSE_PASS']);
};
page.open('http://server.fseconomy.net');

saveLoggedInPage = function() {
  console.log("--- saveLoggedInPage");

  save('login');

  console.log('--- delay 1s for page render');
  setTimeout(airports, 1000);
};

airports = function() {
  console.log('--- airports');
  nextStep = loadAirport;
  nextStepTrigger = 'airport.jsp';

  page.open('http://server.fseconomy.net/airport.jsp');
};

var fetchIcao = env['FSE_ICAO'].split('-');
var curIcao = 'empty';
loadAirport = function() {
  nextStep = saveAirportForm;
  nextStepTrigger = 'airport.jsp';

  if (fetchIcao.length == 0) {
    phantom.exit();
  }

  curIcao = fetchIcao.shift();

  evaluate(function(icao) {
    var f = $('form[action="airport.jsp"]');
    f.find('input[name="icao"]').val(icao);
    f.find('.button[value="Go"]').click();
  }, curIcao);
};

saveAirportForm = function() {
  console.log('--- saveAirportForm');

  save(curIcao);

  console.log('--- delay 750ms for next fetch - not to hammer the server.');
  setTimeout(loadAirport, 750);
};
