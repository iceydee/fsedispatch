var sc = require("./scrape_common.js");
var system = require('system');
var env = system.env;

var airports = function() {
  console.log('--- airports');
  sc.setNextStep(loadAirport);
  sc.setNextTrigger('airport.jsp');

  sc.page.open('http://server.fseconomy.net/airport.jsp');
};

var fetchIcao = env['FSE_ICAO'].split('-');
var curIcao = 'empty';
var loadAirport = function() {
  sc.setNextStep(saveAirportForm);
  sc.setNextTrigger('airport.jsp');

  if (fetchIcao.length == 0) {
    phantom.exit();
  }

  curIcao = fetchIcao.shift();

  sc.evaluate(function(icao) {
    var f = $('form[action="airport.jsp"]');
    f.find('input[name="icao"]').val(icao);
    f.find('.button[value="Go"]').click();
  }, curIcao);
};

var saveAirportForm = function() {
  console.log('--- saveAirportForm');

  sc.save(curIcao);

  console.log('--- delay 750ms for next fetch - not to hammer the server.');
  setTimeout(loadAirport, 750);
};

sc.onLoggedIn(function() {
  console.log("--- and we're over in airport collection mode.");
  airports();
});
