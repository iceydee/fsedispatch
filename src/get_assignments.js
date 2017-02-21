var sc = require('./scrape_common.js');
var system = require('system');
var env = system.env;
var fs = require('fs');
var ProgressBar = require('./progress.js');

var icao = env['FSE_ICAO'].trim();
var group_id = env['GROUP_ID'];
if (typeof(group_id) === "undefined") {
  group_id = 0; // Assign to My Flight
}
var assignment_ids = env['ASSIGNMENT_IDS'].split(",")

var airports = function() {
  console.log('--- airports');
  sc.setNextStep(loadAirport);
  sc.setNextTrigger('airport.jsp');

  sc.page.open('http://server.fseconomy.net/airport.jsp');
};

var loadAirport = function() {
  sc.setNextStep(addAssignments);
  sc.setNextTrigger('airport.jsp');

  sc.evaluate(function(icao) {
    var f = $('form[action="airport.jsp"]');
    f.find('input[name="icao"]').val(icao);
    f.find('.button[value="Go"]').click();
  }, icao);
};

var addAssignments = function() {
  console.log('--- addAssignments');
  sc.setNextStep(ready);
  sc.setNextTrigger('airport.jsp');

  sc.evaluate(function(assignment_ids, group_id) {
    var t = $('form#airportForm');
    for (var i = 0, len = assignment_ids.length; i < len; i++) {
      t.find('input[value="' + assignment_ids[i] + '"]').prop('checked', true);
    }
    t.find('select#addToGroup').val(group_id);
    t.find('input#addSelectedButton').click();
  }, assignment_ids, group_id);
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
