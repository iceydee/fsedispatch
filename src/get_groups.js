var sc = require('./scrape_common.js');
var system = require('system');
var env = system.env;
var fs = require('fs');
var ProgressBar = require('./progress.js');

var groups = function() {
  console.log('--- groups');
  sc.setNextStep(saveGroups);
  sc.setNextTrigger('groups.jsp');

  sc.page.open('http://server.fseconomy.net/groups.jsp');
};

var saveGroups = function() {
  console.log('--- saveGroupsPage');

  sc.save("groups");

  phantom.exit();
};

sc.onLoggedIn(function() {
  system.stderr.write("Starting to fetch groups\n");
  console.log("--- and we're over in group collection mode.");
  groups();
});
