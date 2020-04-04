"use strict";

/* var Plotly = require("../node_modules/plotly.js-dist/plotly.js"); */

exports.lineChartUnc = function(domId, data, layout) {
        return Plotly.newPlot(domId, data, layout);
};

