var system = require('system');

var ProgressBar = function(options) {
	var getTimestamp = function() {
		return (new Date()).getTime() / 1000;
	};

	this.start = getTimestamp();
	this.current = 0;
	this.total = options.total;
	this.complete = false;
	this.length = options.length;

	var twoDeci = function(val) {
		return Math.round(val * 100) / 100;
	}

	if (typeof(this.length) === 'undefined') {
		this.length = 50;
	}

	this.tick = function() {
		this.current += 1;
		if (this.current >= this.total) {
			this.complete = true;
		}
		this.update();
	};

	this.progress = function() {
		return this.current / this.total;
	}

	this.update = function() {
		var p = Math.ceil(this.progress() * this.length);
		var equals = Array(p).join('=');
		var spaces = Array(this.length - p).join(' ');
		var pct = twoDeci(this.progress() * 100);
		
		system.stderr.write('\r[' + equals + spaces + '] ' + this.current + '/' + this.total + '  ' + ' ' + pct + '%' + '  ' + this.remainTime() + ' remaining.' + Array(10).join(' '));
	};

	this.calcRemain = function() {
		var elapsed = getTimestamp() - this.start;
		var remain = ((1 / this.progress()) * elapsed) - elapsed;
		return twoDeci(remain);
	};

	this.remainTime = function() {
		var remain = this.calcRemain();
		if (remain > 60) {
			var remainMinutes = Math.floor(remain / 60);
			remain -= (remainMinutes * 60);
			remain = twoDeci(remain);

			if (remainMinutes > 60) {
				var remainHours = Math.floor(remainMinutes / 60);
				remainMinutes -= (remainHours * 60);
				return remainHours + 'h ' + remainMinutes + 'm ' + remain + 's';
			}

			return remainMinutes + 'm ' + remain + 's';
		}

		remain = twoDeci(remain);
		return remain + 's';
	}
};

module.exports = ProgressBar;