var sc = require('./scrape_common.js');
var system = require('system');
var env = system.env;
var fs = require('fs');
var ProgressBar = require('./progress.js');

var airports = function() {
  console.log('--- airports');
  sc.setNextStep(loadAirport);
  sc.setNextTrigger('airport.jsp');

  sc.page.open('http://server.fseconomy.net/airport.jsp');
};

var stream = fs.open(env['FSE_ICAO_FILE'].trim(), 'r');
var fetchIcao = JSON.parse(stream.read());
stream.close();

var bar = new ProgressBar({total: fetchIcao.length});

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

  console.log('--- delay 100ms for next fetch - not to hammer the server.');

  bar.tick();
  setTimeout(loadAirport, 100);
};

sc.onLoggedIn(function() {
  system.stderr.write("Starting to fetch airports\n");
  console.log("--- and we're over in airport collection mode.");
  airports();
});
