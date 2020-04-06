"use strict";

/* var Plotly = require("../node_modules/plotly.js-dist/plotly.js"); */

exports.lineChartUnc = function(domId, data, layout) {
        return Plotly.newPlot(domId, data, layout);
};

exports.restyleUnc = function(domId, key, value) {
        return Plotly.restyle(domId, key, value);
};

