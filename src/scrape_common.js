var page = require('webpage').create();
var fs = require('fs');
var system = require('system');
var env = system.env;

var filePrefix = env['PREFIX'];

var emptyStep = function() {
  console.log("empty next step");
};

var step = emptyStep;
var nextStep = emptyStep;
var nextStepTrigger = 'notrigger';
var loggedInCallbacks = [];

var evaluate = function(func) {
  console.log("debug: evaluate running");
  var args = [].slice.call(arguments, 1);
  var fn = "function() { return (" + func.toString() + ").apply(this, " + JSON.stringify(args) + ");}";
  return page.includeJs("http://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.min.js", function() {
    page.evaluate(fn);
  });
};

var setNextStep = function(newStep) {
  return nextStep = newStep;
};

var setNextTrigger = function(nextTrigger) {
  return nextStepTrigger = nextTrigger;
};

module.exports = {
  page: page,

  step: function() {
    return step;
  },

  setStep: function(newStep) {
    return step = newStep;
  },

  nextStep: function() {
    return nextStep;
  },

  setNextStep: setNextStep,

  nextTrigger: function() {
    return nextStepTrigger;
  },

  setNextTrigger: setNextTrigger,

  evaluate: evaluate,

  save: function(name) {
    fs.write('./data/' + filePrefix + '-' + name.toLowerCase() + '.html', page.content, 'w');
  },

  onLoggedIn: function(func) {
    loggedInCallbacks.push(func);
    return func;
  }
};

phantom.cookiesEnabled = true;

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

var step = function() {
  console.log("--- Login");

  setNextStep(loggedInPage);
  setNextTrigger('index.jsp');

  evaluate(function(user, pass) {
    $('input[name="user"]').val(user);
    $('input[name="password"]').val(pass);
    $('input[name="event"]').click();
  }, env['FSE_USER'], env['FSE_PASS']);
};
page.open('http://server.fseconomy.net');

var loggedInPage = function() {
  console.log("--- loggedInPage");

  console.log('--- delay 1s for page render');
  setTimeout(callLoggedInCallbacks, 1000);
};

var callLoggedInCallbacks = function() {
  console.log("--- triggering logged in callbacks");
  loggedInCallbacks.map(function(f) {f();});
};
