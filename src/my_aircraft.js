var sc = require('./scrape_common.js');
var system = require('system');
var env = system.env;
var fs = require('fs');
var ProgressBar = require('./progress.js');

var aircrafts = function() {
  console.log('--- aircrafts');
  sc.setNextStep(saveAircraft);
  sc.setNextTrigger('aircraft.jsp');

  sc.page.open('http://server.fseconomy.net/aircraft.jsp');
};

var saveAircraft = function() {
  console.log('--- saveAircraft');

  sc.save("my-aircraft");

  phantom.exit();
};

sc.onLoggedIn(function() {
  system.stderr.write("Starting to fetch aircraft\n");
  console.log("--- and we're over in aircraft collection mode.");
  aircrafts();
});
