var sc = require('./scrape_common.js');
var system = require('system');
var env = system.env;
var fs = require('fs');
var ProgressBar = require('./progress.js');

var icao = env['FSE_ICAO'].trim();
var acReg = env['AIRCRAFT_REGISTRATION'].trim();
var rentDry = parseInt(env['RENT_DRY'].trim(), 10) ? true:false;

var airports = function() {
  console.log('--- airports');
  sc.setNextStep(loadAirport);
  sc.setNextTrigger('airport.jsp');

  sc.page.open('http://server.fseconomy.net/airport.jsp');
};

var loadAirport = function() {
  sc.setNextStep(rentAircraft);
  sc.setNextTrigger('airport.jsp');

  sc.evaluate(function(icao) {
    var f = $('form[action="airport.jsp"]');
    f.find('input[name="icao"]').val(icao);
    f.find('.button[value="Go"]').click();
  }, icao);
};

var rentAircraft = function() {
  console.log('--- rentAircraft');
  system.stderr.write("Renting aircraft with registration: " + acReg + "\n");
  sc.setNextStep(ready);
  sc.setNextTrigger('airport.jsp');

  sc.evaluate(function(acReg, rentDry) {
    // Get the id
    var acId = $('table.aircraftTable td > a:contains("' + acReg + '")').attr("href");
    acId = acId.split("?id=")[1];

    if (rentDry) {
      doSubmit2(acId, 'dry');
    } else {
      doSubmit2(acId, 'wet');
    }
  }, acReg, rentDry);
};

var ready = function() {
  console.log('--- ready');
  phantom.exit();
}

sc.onLoggedIn(function() {
  system.stderr.write("Starting to fetch airports\n");
  console.log("--- and we're over in airport collection mode.");
  airports();
});
